use std::collections::BTreeMap;
use std::fmt::Display;
use std::fmt::Write;
use std::path::PathBuf;

use async_trait::async_trait;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_operation::{
    Operation,
    operations::{
        file::FilePath,
        podman_compose::{COMPOSE_CONFIG_HASH_LABEL, PodmanComposeOperation, marker_network_name},
    },
};
use lusid_params::{ParseError, ParseParams, StructFields};
use rimu::{Span, Spanned, Value};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use thiserror::Error;
use tokio::io;

use crate::{ChangeKind, ResourceChangeTrait, ResourceType};

/// Wire-format version baked into [`compose_config_hash`]. Bumping this
/// invalidates every existing compose marker network on the next apply,
/// forcing a recreate cycle. Treat as a versioned hash input.
const COMPOSE_CONFIG_HASH_WIRE_VERSION: &str = "v1";

/// Plan-level parameters for the `@resource/podman-compose` resource.
///
/// Tagged by `state: "present" | "absent"`. Manages a podman-compose project
/// (one project per resource item). Drift is detected via a SHA-256 hash of
/// the declared spec, stored as a label on a lusid-owned marker network
/// created alongside the project - see the resource-level docs in
/// `docs/reference/resources.md` for the full lifecycle.
///
/// The `config_hash` field is populated by [`PodmanComposeParams::prepare`]
/// (run after `validate_host_paths` in the apply pipeline). At parse-time
/// it is `None`; resources expansion expects `Some(_)`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PodmanComposeParams {
    Present {
        project: String,
        /// Span of the `project:` value, kept for project-name regex
        /// diagnostics that surface post-parse. Skipped on the wire.
        #[serde(skip, default)]
        project_span: Span,
        files: Vec<FilePath>,
        /// Parallel to `files`: span per element so a "this file is missing"
        /// diagnostic points at the offending list entry. Skipped on the
        /// wire (validation runs pre-emit).
        #[serde(skip, default)]
        files_spans: Vec<Span>,
        /// Compose project working directory. Defaults at parse-time to the
        /// parent directory of `files[0]` so relative paths inside the
        /// compose YAML resolve consistently.
        working_dir: FilePath,
        #[serde(skip, default)]
        working_dir_span: Span,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        env_file: Option<FilePath>,
        #[serde(skip, default)]
        env_file_span: Option<Span>,
        /// Populated by [`PodmanComposeParams::prepare`]; `None` until then.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        config_hash: Option<String>,
        /// When set, every shell-out (probe + apply ops) runs under
        /// `sudo -n`. Selects rootful podman: project lives in root's
        /// podman runtime, entirely separate from rootless.
        sudo: bool,
    },
    Absent {
        project: String,
        #[serde(skip, default)]
        project_span: Span,
        sudo: bool,
    },
}

impl ParseParams for PodmanComposeParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let state = fields.take_discriminator("state", &["present", "absent"])?;
        let out = match state {
            "present" => {
                let (project, project_span) = fields.required_string_spanned("project")?.take();
                validate_project_name(&project, &project_span)?;
                let files_spanned = fields.required_host_path_spanned_list("files")?;
                if files_spanned.is_empty() {
                    return Err(Spanned::new(
                        ParseError::InvalidValue {
                            reason: "compose `files:` must contain at least one entry",
                            got: Box::new(Value::List(vec![])),
                        },
                        project_span,
                    ));
                }
                let (files, files_spans): (Vec<FilePath>, Vec<Span>) = files_spanned
                    .into_iter()
                    .map(|spanned| {
                        let (path, span) = spanned.take();
                        (FilePath::new(path.to_string_lossy().into_owned()), span)
                    })
                    .unzip();
                let working_dir_spanned = fields.optional_host_path_spanned("working_dir")?;
                let (working_dir, working_dir_span) = match working_dir_spanned {
                    Some(spanned) => {
                        let (path, span) = spanned.take();
                        (FilePath::new(path.to_string_lossy().into_owned()), span)
                    }
                    None => {
                        // Default to the parent directory of the first compose file.
                        // The list-non-empty check above guarantees indexing is safe.
                        let first = files[0].as_path();
                        let parent = first.parent().unwrap_or_else(|| std::path::Path::new("."));
                        (
                            FilePath::new(parent.to_string_lossy().into_owned()),
                            files_spans[0].clone(),
                        )
                    }
                };
                let env_file_spanned = fields.optional_host_path_spanned("env_file")?;
                let (env_file, env_file_span) = match env_file_spanned {
                    Some(spanned) => {
                        let (path, span) = spanned.take();
                        (
                            Some(FilePath::new(path.to_string_lossy().into_owned())),
                            Some(span),
                        )
                    }
                    None => (None, None),
                };
                let sudo = fields.optional_bool("sudo")?.unwrap_or(false);
                PodmanComposeParams::Present {
                    project,
                    project_span,
                    files,
                    files_spans,
                    working_dir,
                    working_dir_span,
                    env_file,
                    env_file_span,
                    config_hash: None,
                    sudo,
                }
            }
            "absent" => {
                let (project, project_span) = fields.required_string_spanned("project")?.take();
                validate_project_name(&project, &project_span)?;
                PodmanComposeParams::Absent {
                    project,
                    project_span,
                    sudo: fields.optional_bool("sudo")?.unwrap_or(false),
                }
            }
            _ => unreachable!(),
        };
        fields.finish()?;
        Ok(out)
    }
}

/// Validate a compose project name against `^[a-z0-9][a-z0-9_-]{0,62}$`.
///
/// The regex is enforced here so the value is safe to interpolate into the
/// `down` shell script (see `operation/src/operations/podman_compose.rs`),
/// and so an operator typo like `My_App` surfaces with a spanned diagnostic
/// pointing at the `project:` line rather than failing late inside
/// `podman-compose`'s own argument-parsing.
fn validate_project_name(value: &str, span: &Span) -> Result<(), Spanned<ParseError>> {
    let bytes = value.as_bytes();
    let valid = (1..=63).contains(&bytes.len())
        && (bytes[0].is_ascii_lowercase() || bytes[0].is_ascii_digit())
        && bytes[1..]
            .iter()
            .all(|b| b.is_ascii_lowercase() || b.is_ascii_digit() || *b == b'_' || *b == b'-');
    if valid {
        Ok(())
    } else {
        Err(Spanned::new(
            ParseError::InvalidValue {
                reason: "compose project name must match ^[a-z0-9][a-z0-9_-]{0,62}$ (lowercase letters, digits, `_`, `-`; ≤63 chars; cannot start with `-` or `_`)",
                got: Box::new(Value::String(value.to_string())),
            },
            span.clone(),
        ))
    }
}

impl Display for PodmanComposeParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = |sudo: bool| if sudo { "[sudo] " } else { "" };
        match self {
            PodmanComposeParams::Present { project, sudo, .. } => write!(
                f,
                "{}PodmanCompose::Present(project = {project})",
                prefix(*sudo)
            ),
            PodmanComposeParams::Absent { project, sudo, .. } => write!(
                f,
                "{}PodmanCompose::Absent(project = {project})",
                prefix(*sudo)
            ),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PodmanComposeResource {
    Present {
        project: String,
        files: Vec<FilePath>,
        working_dir: FilePath,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        env_file: Option<FilePath>,
        config_hash: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    Absent {
        project: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
}

impl Display for PodmanComposeResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = |sudo: bool| if sudo { "[sudo] " } else { "" };
        match self {
            PodmanComposeResource::Present { project, sudo, .. } => write!(
                f,
                "{}PodmanCompose::Present(project = {project})",
                prefix(*sudo)
            ),
            PodmanComposeResource::Absent { project, sudo } => write!(
                f,
                "{}PodmanCompose::Absent(project = {project})",
                prefix(*sudo)
            ),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PodmanComposeState {
    /// Marker network not found for the project. Containers labelled with
    /// the project may still exist (e.g. operator brought it up manually):
    /// the `change` function treats this as drift and emits `Up`, which is
    /// mostly idempotent under `podman-compose up -d` on a healthy project.
    Absent,
    /// Marker network exists, carrying the hash label from the most recent
    /// successful apply.
    Present {
        /// Value of the [`COMPOSE_CONFIG_HASH_LABEL`] on the marker network.
        /// `None` if the label is missing - treated as drift so old or
        /// foreign markers get recreated.
        config_hash: Option<String>,
    },
}

impl Display for PodmanComposeState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PodmanComposeState::Absent => write!(f, "PodmanCompose::Absent"),
            PodmanComposeState::Present { .. } => write!(f, "PodmanCompose::Present"),
        }
    }
}

#[derive(Error, Debug)]
pub enum PodmanComposeStateError {
    #[error(transparent)]
    Command(#[from] CommandError),

    #[error("failed to parse podman network inspect output: {source}\noutput: {output}")]
    ParseNetworkInspect {
        #[source]
        source: serde_json::Error,
        output: String,
    },

    #[error("podman network inspect returned empty array for marker network")]
    NetworkInspectEmpty,
}

/// Subset of `podman network inspect` JSON we care about: just the labels
/// map, where lusid stamps its config-hash on the marker network.
#[derive(Debug, Clone, Default, Deserialize)]
struct InspectNetwork {
    #[serde(rename = "labels", default)]
    labels: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PodmanComposeChange {
    /// Project absent on target but declared present. Bring it up.
    Up {
        project: String,
        files: Vec<FilePath>,
        working_dir: FilePath,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        env_file: Option<FilePath>,
        config_hash: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    /// Project present but its hash no longer matches; tear it down and
    /// bring it back up. Marker is uninstalled first, then `down`, then
    /// `up`, then marker is reinstalled - chained via causality `requires`
    /// edges (see [`PodmanCompose::operations`]).
    Recreate {
        project: String,
        files: Vec<FilePath>,
        working_dir: FilePath,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        env_file: Option<FilePath>,
        config_hash: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    /// Declared absent and the project is up. Tear it down (containers +
    /// networks; named volumes preserved).
    Down {
        project: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
}

impl Display for PodmanComposeChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = |sudo: bool| if sudo { "[sudo] " } else { "" };
        match self {
            PodmanComposeChange::Up { project, sudo, .. } => {
                write!(f, "{}PodmanCompose::Up(project = {project})", prefix(*sudo))
            }
            PodmanComposeChange::Recreate { project, sudo, .. } => write!(
                f,
                "{}PodmanCompose::Recreate(project = {project})",
                prefix(*sudo)
            ),
            PodmanComposeChange::Down { project, sudo } => write!(
                f,
                "{}PodmanCompose::Down(project = {project})",
                prefix(*sudo)
            ),
        }
    }
}

impl ResourceChangeTrait for PodmanComposeChange {
    fn kind(&self) -> ChangeKind {
        match self {
            PodmanComposeChange::Up { .. } => ChangeKind::Added,
            PodmanComposeChange::Recreate { .. } => ChangeKind::Modified,
            PodmanComposeChange::Down { .. } => ChangeKind::Removed,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PodmanCompose;

#[async_trait]
impl ResourceType for PodmanCompose {
    const ID: &'static str = "podman-compose";

    type Params = PodmanComposeParams;
    type Resource = PodmanComposeResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        let resource = match params {
            PodmanComposeParams::Present {
                project,
                files,
                working_dir,
                env_file,
                config_hash,
                sudo,
                ..
            } => PodmanComposeResource::Present {
                project,
                files,
                working_dir,
                env_file,
                // Programmer-invariant: `prepare()` must have run before
                // `resources()`. The apply pipeline (lusid-apply) wires this
                // between `validate_host_paths` and resources expansion;
                // missing here means a caller is bypassing the pipeline.
                config_hash: config_hash.expect(
                    "PodmanComposeParams::prepare must run before PodmanCompose::resources",
                ),
                sudo,
            },
            PodmanComposeParams::Absent { project, sudo, .. } => {
                PodmanComposeResource::Absent { project, sudo }
            }
        };
        vec![CausalityTree::leaf(CausalityMeta::default(), resource)]
    }

    type State = PodmanComposeState;
    type StateError = PodmanComposeStateError;

    async fn state(
        _ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        let (project, sudo) = match resource {
            PodmanComposeResource::Present { project, sudo, .. }
            | PodmanComposeResource::Absent { project, sudo } => (project, *sudo),
        };
        probe_state(project, sudo).await
    }

    type Change = PodmanComposeChange;

    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        match (resource, state) {
            (PodmanComposeResource::Absent { .. }, PodmanComposeState::Absent) => None,
            (
                PodmanComposeResource::Absent { project, sudo },
                PodmanComposeState::Present { .. },
            ) => Some(PodmanComposeChange::Down {
                project: project.clone(),
                sudo: *sudo,
            }),
            (
                PodmanComposeResource::Present {
                    project,
                    files,
                    working_dir,
                    env_file,
                    config_hash,
                    sudo,
                },
                PodmanComposeState::Absent,
            ) => Some(PodmanComposeChange::Up {
                project: project.clone(),
                files: files.clone(),
                working_dir: working_dir.clone(),
                env_file: env_file.clone(),
                config_hash: config_hash.clone(),
                sudo: *sudo,
            }),
            (
                PodmanComposeResource::Present {
                    project,
                    files,
                    working_dir,
                    env_file,
                    config_hash,
                    sudo,
                },
                PodmanComposeState::Present {
                    config_hash: current_config_hash,
                },
            ) => {
                // Hash mismatch (or missing label on a foreign marker) is the
                // sole drift signal. Match-by-hex avoids podman-compose
                // version-dependent labelling quirks - the marker label was
                // written by lusid with the same `compose_config_hash`
                // function that ran here.
                let hash_matches = current_config_hash.as_deref() == Some(config_hash.as_str());
                if hash_matches {
                    None
                } else {
                    Some(PodmanComposeChange::Recreate {
                        project: project.clone(),
                        files: files.clone(),
                        working_dir: working_dir.clone(),
                        env_file: env_file.clone(),
                        config_hash: config_hash.clone(),
                        sudo: *sudo,
                    })
                }
            }
        }
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        match change {
            PodmanComposeChange::Up {
                project,
                files,
                working_dir,
                env_file,
                config_hash,
                sudo,
            } => up_ops(
                project,
                files,
                working_dir,
                env_file,
                config_hash,
                sudo,
                None,
            ),
            PodmanComposeChange::Recreate {
                project,
                files,
                working_dir,
                env_file,
                config_hash,
                sudo,
            } => recreate_ops(project, files, working_dir, env_file, config_hash, sudo),
            PodmanComposeChange::Down { project, sudo } => down_ops(project, sudo),
        }
    }
}

/// Probe a compose project's state by inspecting its lusid marker network.
/// `podman network inspect` exits non-zero when the network is missing;
/// treat any non-success as `Absent` - distinguishing "absent" from "podman
/// itself failed" via stderr is unreliable across versions, and a broken
/// podman install surfaces at apply-time on the first up.
async fn probe_state(
    project: &str,
    sudo: bool,
) -> Result<PodmanComposeState, PodmanComposeStateError> {
    let marker = marker_network_name(project);
    let mut cmd = Command::new("podman");
    cmd.args(["network", "inspect", &marker]);
    let mut cmd = if sudo { cmd.sudo() } else { cmd };
    let outcome = cmd.outcome().await?;
    if !outcome.status.success() {
        return Ok(PodmanComposeState::Absent);
    }

    let networks: Vec<InspectNetwork> =
        serde_json::from_slice(&outcome.stdout).map_err(|source| {
            PodmanComposeStateError::ParseNetworkInspect {
                source,
                output: String::from_utf8_lossy(&outcome.stdout).into_owned(),
            }
        })?;
    let network = networks
        .into_iter()
        .next()
        .ok_or(PodmanComposeStateError::NetworkInspectEmpty)?;
    let config_hash = network.labels.get(COMPOSE_CONFIG_HASH_LABEL).cloned();
    Ok(PodmanComposeState::Present { config_hash })
}

/// Build the operation list for an `Up`. Optionally preceded by a
/// down + marker-uninstall pair when `after_down_id` is `Some`. Used by
/// both `Up` and `Recreate` changes so the causality wiring lives in one
/// place.
fn up_ops(
    project: String,
    files: Vec<FilePath>,
    working_dir: FilePath,
    env_file: Option<FilePath>,
    config_hash: String,
    sudo: bool,
    after_down_id: Option<&'static str>,
) -> Vec<CausalityTree<Operation>> {
    let mut ops: Vec<CausalityTree<Operation>> = Vec::new();

    let up_meta = CausalityMeta {
        id: Some("compose_up".into()),
        requires: after_down_id.map(|id| vec![id.into()]).unwrap_or_default(),
        required_by: vec![],
    };
    ops.push(CausalityTree::leaf(
        up_meta,
        Operation::PodmanCompose(PodmanComposeOperation::Up {
            project: project.clone(),
            files,
            working_dir,
            env_file,
            sudo,
        }),
    ));

    ops.push(CausalityTree::leaf(
        CausalityMeta::requires(vec!["compose_up".into()]),
        Operation::PodmanCompose(PodmanComposeOperation::MarkerInstall {
            project,
            config_hash,
            sudo,
        }),
    ));

    ops
}

/// Build the operation list for a `Down`: marker uninstall, then project
/// teardown. Causality IDs are scoped per branch by `map_plan_subitems`'s
/// `scope_id` machinery (see AGENTS.md), so the same IDs are reused by
/// `recreate_ops` without collision.
///
/// Marker uninstall runs first so a half-failed down does not leave a
/// marker pointing at a stale project.
fn down_ops(project: String, sudo: bool) -> Vec<CausalityTree<Operation>> {
    vec![
        CausalityTree::leaf(
            CausalityMeta::id("compose_marker_uninstall".into()),
            Operation::PodmanCompose(PodmanComposeOperation::MarkerUninstall {
                project: project.clone(),
                sudo,
            }),
        ),
        CausalityTree::leaf(
            CausalityMeta::requires(vec!["compose_marker_uninstall".into()]),
            Operation::PodmanCompose(PodmanComposeOperation::Down { project, sudo }),
        ),
    ]
}

/// Build the operation list for a `Recreate`: tear down, then bring back
/// up, with causality edges so the scheduler runs them in order.
fn recreate_ops(
    project: String,
    files: Vec<FilePath>,
    working_dir: FilePath,
    env_file: Option<FilePath>,
    config_hash: String,
    sudo: bool,
) -> Vec<CausalityTree<Operation>> {
    let mut ops = down_ops(project.clone(), sudo);
    let up = up_ops(
        project,
        files,
        working_dir,
        env_file,
        config_hash,
        sudo,
        Some("compose_down"),
    );
    ops.extend(up);
    ops
}

/// Compute the SHA-256 of the compose project's declared spec. Stored as
/// the [`COMPOSE_CONFIG_HASH_LABEL`] on the marker network at apply time
/// and compared against on every state observation to detect drift.
///
/// Inputs are taken in declaration order with `\0` separators so reordering
/// the `files` list (which has meaningful compose-merge semantics) busts
/// the hash and triggers a recreate. The wire-version prefix
/// ([`COMPOSE_CONFIG_HASH_WIRE_VERSION`]) lets a future input-schema change
/// be observably distinct: bump it, and every existing marker is invalidated
/// on the next apply.
///
/// Including the `sudo` flag in the hash means a runtime switch
/// (rootless ↔ rootful) produces a distinct hash even though the two
/// runtimes maintain entirely separate marker networks - matches the
/// single-container resource's per-spec philosophy.
fn compose_config_hash(
    project: &str,
    sudo: bool,
    files: &[(FilePath, Vec<u8>)],
    env_file: Option<(&FilePath, &[u8])>,
) -> String {
    let mut hasher = Sha256::new();
    hasher.update(COMPOSE_CONFIG_HASH_WIRE_VERSION.as_bytes());
    hasher.update(b"\0");
    hasher.update(project.as_bytes());
    hasher.update(b"\0");
    hasher.update([if sudo { 0x01 } else { 0x00 }]);
    hasher.update(b"\0");
    let file_count = u32::try_from(files.len()).unwrap_or(u32::MAX);
    hasher.update(file_count.to_le_bytes());
    for (_path, bytes) in files {
        hasher.update(bytes);
        hasher.update(b"\0");
    }
    match env_file {
        Some((_path, bytes)) => {
            hasher.update([0x01]);
            hasher.update(bytes);
        }
        None => hasher.update([0x00]),
    }
    let digest = hasher.finalize();
    let mut out = String::with_capacity(digest.len() * 2);
    for byte in digest {
        let _ = write!(out, "{byte:02x}");
    }
    out
}

/// Span-aware errors surfaced by [`PodmanComposeParams::prepare`].
#[derive(Debug, Error)]
pub enum PodmanComposePrepareError {
    #[error("failed to read compose file {path:?} (entry {index} of `files:`)")]
    ComposeFileRead {
        path: PathBuf,
        span: Span,
        index: usize,
        #[source]
        source: io::Error,
    },

    #[error("failed to read compose env_file {path:?}")]
    ComposeEnvFileRead {
        path: PathBuf,
        span: Span,
        #[source]
        source: io::Error,
    },
}

impl PodmanComposeParams {
    /// Read the compose files referenced by the params and bake the
    /// resulting hash into the params. Called by the apply pipeline between
    /// [`crate::ResourceParams::validate_host_paths`] and the `resources()`
    /// expansion. For `Absent` variants this is a pass-through.
    pub async fn prepare(self) -> Result<Self, PodmanComposePrepareError> {
        match self {
            PodmanComposeParams::Present {
                project,
                project_span,
                files,
                files_spans,
                working_dir,
                working_dir_span,
                env_file,
                env_file_span,
                config_hash: _,
                sudo,
            } => {
                let mut files_with_bytes: Vec<(FilePath, Vec<u8>)> =
                    Vec::with_capacity(files.len());
                for (index, file) in files.iter().enumerate() {
                    let bytes = tokio::fs::read(file.as_path()).await.map_err(|source| {
                        PodmanComposePrepareError::ComposeFileRead {
                            path: file.as_path().to_path_buf(),
                            span: files_spans[index].clone(),
                            index,
                            source,
                        }
                    })?;
                    files_with_bytes.push((file.clone(), bytes));
                }
                let env_file_bytes = match (env_file.as_ref(), env_file_span.as_ref()) {
                    (Some(ef), Some(span)) => {
                        let bytes = tokio::fs::read(ef.as_path()).await.map_err(|source| {
                            PodmanComposePrepareError::ComposeEnvFileRead {
                                path: ef.as_path().to_path_buf(),
                                span: span.clone(),
                                source,
                            }
                        })?;
                        Some(bytes)
                    }
                    _ => None,
                };
                let hash = compose_config_hash(
                    &project,
                    sudo,
                    &files_with_bytes,
                    env_file.as_ref().zip(env_file_bytes.as_deref()),
                );
                Ok(PodmanComposeParams::Present {
                    project,
                    project_span,
                    files,
                    files_spans,
                    working_dir,
                    working_dir_span,
                    env_file,
                    env_file_span,
                    config_hash: Some(hash),
                    sudo,
                })
            }
            other => Ok(other),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn resource(project: &str, hash: &str) -> PodmanComposeResource {
        PodmanComposeResource::Present {
            project: project.into(),
            files: vec![FilePath::new("/etc/compose/app.yaml")],
            working_dir: FilePath::new("/etc/compose"),
            env_file: None,
            config_hash: hash.into(),
            sudo: false,
        }
    }

    #[test]
    fn change_none_when_hash_matches() {
        let r = resource("app", "abc123");
        let s = PodmanComposeState::Present {
            config_hash: Some("abc123".into()),
        };
        assert!(PodmanCompose::change(&r, &s).is_none());
    }

    #[test]
    fn change_up_when_marker_absent() {
        // Adoption: marker missing on a project the operator may have
        // brought up manually. The eventual `up -d` is mostly idempotent
        // and installs the marker on the way through.
        let r = resource("app", "abc123");
        let s = PodmanComposeState::Absent;
        let change = PodmanCompose::change(&r, &s).expect("change");
        assert!(matches!(change, PodmanComposeChange::Up { .. }));
    }

    #[test]
    fn change_recreate_when_hash_differs() {
        let r = resource("app", "newhash");
        let s = PodmanComposeState::Present {
            config_hash: Some("oldhash".into()),
        };
        let change = PodmanCompose::change(&r, &s).expect("change");
        assert!(matches!(change, PodmanComposeChange::Recreate { .. }));
    }

    #[test]
    fn change_recreate_when_label_missing() {
        let r = resource("app", "abc123");
        let s = PodmanComposeState::Present { config_hash: None };
        let change = PodmanCompose::change(&r, &s).expect("change");
        assert!(matches!(change, PodmanComposeChange::Recreate { .. }));
    }

    #[test]
    fn change_down_when_declared_absent_and_present() {
        let r = PodmanComposeResource::Absent {
            project: "app".into(),
            sudo: false,
        };
        let s = PodmanComposeState::Present {
            config_hash: Some("abc123".into()),
        };
        let change = PodmanCompose::change(&r, &s).expect("change");
        assert!(matches!(change, PodmanComposeChange::Down { .. }));
    }

    #[test]
    fn change_up_carries_files_and_sudo() {
        // Regression guard: the Up change must propagate every field from
        // the resource. Catches a `..` rest-pattern accidentally swallowing
        // a new field in a future refactor.
        let r = PodmanComposeResource::Present {
            project: "app".into(),
            files: vec![FilePath::new("/c/a.yaml"), FilePath::new("/c/b.yaml")],
            working_dir: FilePath::new("/c"),
            env_file: Some(FilePath::new("/c/.env")),
            config_hash: "h".into(),
            sudo: true,
        };
        let change = PodmanCompose::change(&r, &PodmanComposeState::Absent).expect("change");
        match change {
            PodmanComposeChange::Up {
                project,
                files,
                working_dir,
                env_file,
                sudo,
                ..
            } => {
                assert_eq!(project, "app");
                assert_eq!(files.len(), 2);
                assert_eq!(working_dir.as_path().to_str().unwrap(), "/c");
                assert!(env_file.is_some());
                assert!(sudo);
            }
            other => panic!("expected Up, got {other:?}"),
        }
    }

    #[test]
    fn change_none_when_absent_matches() {
        let r = PodmanComposeResource::Absent {
            project: "app".into(),
            sudo: false,
        };
        let s = PodmanComposeState::Absent;
        assert!(PodmanCompose::change(&r, &s).is_none());
    }

    fn file_bytes(path: &str, body: &[u8]) -> (FilePath, Vec<u8>) {
        (FilePath::new(path), body.to_vec())
    }

    #[test]
    fn hash_changes_when_project_name_changes() {
        let files = vec![file_bytes(
            "/c/a.yaml",
            b"services:\n  web:\n    image: nginx\n",
        )];
        let a = compose_config_hash("app", false, &files, None);
        let b = compose_config_hash("renamed", false, &files, None);
        assert_ne!(a, b);
    }

    #[test]
    fn hash_changes_when_file_bytes_change() {
        let a = compose_config_hash(
            "app",
            false,
            &[file_bytes(
                "/c/a.yaml",
                b"services: { a: { image: nginx } }",
            )],
            None,
        );
        let b = compose_config_hash(
            "app",
            false,
            &[file_bytes(
                "/c/a.yaml",
                b"services: { a: { image: redis } }",
            )],
            None,
        );
        assert_ne!(a, b);
    }

    #[test]
    fn hash_changes_when_file_order_changes() {
        let a_bytes = b"services:\n  a: { image: nginx }\n";
        let b_bytes = b"services:\n  a: { image: redis }\n";
        let a = compose_config_hash(
            "app",
            false,
            &[
                file_bytes("/c/a.yaml", a_bytes),
                file_bytes("/c/b.yaml", b_bytes),
            ],
            None,
        );
        let b = compose_config_hash(
            "app",
            false,
            &[
                file_bytes("/c/b.yaml", b_bytes),
                file_bytes("/c/a.yaml", a_bytes),
            ],
            None,
        );
        assert_ne!(a, b);
    }

    #[test]
    fn hash_changes_when_env_file_added() {
        let files = vec![file_bytes("/c/a.yaml", b"x")];
        let env_path = FilePath::new("/c/.env");
        let env_bytes: &[u8] = b"X=1\n";
        let without = compose_config_hash("app", false, &files, None);
        let with = compose_config_hash("app", false, &files, Some((&env_path, env_bytes)));
        assert_ne!(without, with);
    }

    #[test]
    fn hash_changes_when_env_file_contents_change() {
        let files = vec![file_bytes("/c/a.yaml", b"x")];
        let env_path = FilePath::new("/c/.env");
        let a = compose_config_hash("app", false, &files, Some((&env_path, b"X=1\n")));
        let b = compose_config_hash("app", false, &files, Some((&env_path, b"X=2\n")));
        assert_ne!(a, b);
    }

    #[test]
    fn hash_changes_when_sudo_flag_flips() {
        let files = vec![file_bytes("/c/a.yaml", b"x")];
        let rootless = compose_config_hash("app", false, &files, None);
        let rootful = compose_config_hash("app", true, &files, None);
        assert_ne!(rootless, rootful);
    }

    #[test]
    fn hash_stable_across_repeat_compute() {
        let files = vec![file_bytes("/c/a.yaml", b"a"), file_bytes("/c/b.yaml", b"b")];
        assert_eq!(
            compose_config_hash("app", false, &files, None),
            compose_config_hash("app", false, &files, None)
        );
    }

    // -- parser ------------------------------------------------------------

    fn empty_span() -> rimu::Span {
        rimu::Span::new(rimu::SourceId::empty(), 0, 0)
    }

    fn s(value: Value) -> Spanned<Value> {
        Spanned::new(value, empty_span())
    }

    fn object(pairs: Vec<(&str, Value)>) -> Spanned<Value> {
        use indexmap::IndexMap;
        let mut map: IndexMap<String, Spanned<Value>> = IndexMap::new();
        for (k, v) in pairs {
            map.insert(k.to_string(), s(v));
        }
        s(Value::Object(map))
    }

    fn host_path(p: &str) -> Value {
        Value::HostPath(PathBuf::from(p))
    }

    #[test]
    fn parse_present_minimal_form() {
        let params = PodmanComposeParams::parse_params(object(vec![
            ("state", Value::String("present".into())),
            ("project", Value::String("myapp".into())),
            ("files", Value::List(vec![s(host_path("/c/app.yaml"))])),
        ]))
        .expect("parse");
        match params {
            PodmanComposeParams::Present {
                project,
                files,
                sudo,
                ..
            } => {
                assert_eq!(project, "myapp");
                assert_eq!(files.len(), 1);
                assert!(!sudo);
            }
            other => panic!("expected Present, got {other:?}"),
        }
    }

    #[test]
    fn parse_present_defaults_working_dir_to_first_file_parent() {
        let params = PodmanComposeParams::parse_params(object(vec![
            ("state", Value::String("present".into())),
            ("project", Value::String("myapp".into())),
            (
                "files",
                Value::List(vec![s(host_path("/etc/compose/app.yaml"))]),
            ),
        ]))
        .expect("parse");
        match params {
            PodmanComposeParams::Present { working_dir, .. } => {
                assert_eq!(working_dir.as_path().to_str().unwrap(), "/etc/compose");
            }
            other => panic!("expected Present, got {other:?}"),
        }
    }

    #[test]
    fn parse_present_carries_env_file_when_set() {
        let params = PodmanComposeParams::parse_params(object(vec![
            ("state", Value::String("present".into())),
            ("project", Value::String("myapp".into())),
            ("files", Value::List(vec![s(host_path("/c/app.yaml"))])),
            ("env_file", host_path("/c/.env")),
        ]))
        .expect("parse");
        match params {
            PodmanComposeParams::Present { env_file, .. } => {
                assert!(env_file.is_some());
            }
            other => panic!("expected Present, got {other:?}"),
        }
    }

    #[test]
    fn parse_present_rejects_invalid_project_name() {
        let err = PodmanComposeParams::parse_params(object(vec![
            ("state", Value::String("present".into())),
            ("project", Value::String("My_App".into())),
            ("files", Value::List(vec![s(host_path("/c/app.yaml"))])),
        ]))
        .expect_err("should reject uppercase project");
        match err.inner() {
            ParseError::InvalidValue { reason, .. } => {
                assert!(reason.contains("project name"), "got reason: {reason}");
            }
            other => panic!("expected InvalidValue, got {other:?}"),
        }
    }

    #[test]
    fn parse_present_rejects_empty_files() {
        let err = PodmanComposeParams::parse_params(object(vec![
            ("state", Value::String("present".into())),
            ("project", Value::String("app".into())),
            ("files", Value::List(vec![])),
        ]))
        .expect_err("should reject empty files");
        match err.inner() {
            ParseError::InvalidValue { reason, .. } => {
                assert!(reason.contains("at least one"), "got reason: {reason}");
            }
            other => panic!("expected InvalidValue, got {other:?}"),
        }
    }

    #[test]
    fn parse_absent_minimal_form() {
        let params = PodmanComposeParams::parse_params(object(vec![
            ("state", Value::String("absent".into())),
            ("project", Value::String("myapp".into())),
        ]))
        .expect("parse");
        match params {
            PodmanComposeParams::Absent { project, sudo, .. } => {
                assert_eq!(project, "myapp");
                assert!(!sudo);
            }
            other => panic!("expected Absent, got {other:?}"),
        }
    }

    #[test]
    fn parse_absent_rejects_invalid_project_name() {
        let err = PodmanComposeParams::parse_params(object(vec![
            ("state", Value::String("absent".into())),
            ("project", Value::String("".into())),
        ]))
        .expect_err("should reject empty project");
        assert!(matches!(err.inner(), ParseError::InvalidValue { .. }));
    }

    // -- operations lowering ----------------------------------------------

    #[test]
    fn up_change_lowers_to_up_plus_marker_install() {
        let change = PodmanComposeChange::Up {
            project: "app".into(),
            files: vec![FilePath::new("/c/a.yaml")],
            working_dir: FilePath::new("/c"),
            env_file: None,
            config_hash: "h".into(),
            sudo: false,
        };
        let ops = PodmanCompose::operations(change);
        assert_eq!(ops.len(), 2);
    }

    #[test]
    fn recreate_change_lowers_to_four_ops_in_order() {
        let change = PodmanComposeChange::Recreate {
            project: "app".into(),
            files: vec![FilePath::new("/c/a.yaml")],
            working_dir: FilePath::new("/c"),
            env_file: None,
            config_hash: "h".into(),
            sudo: false,
        };
        let ops = PodmanCompose::operations(change);
        // marker uninstall, down, up, marker install.
        assert_eq!(ops.len(), 4);
    }

    #[test]
    fn down_change_lowers_to_marker_uninstall_plus_down() {
        let change = PodmanComposeChange::Down {
            project: "app".into(),
            sudo: false,
        };
        let ops = PodmanCompose::operations(change);
        assert_eq!(ops.len(), 2);
    }
}
