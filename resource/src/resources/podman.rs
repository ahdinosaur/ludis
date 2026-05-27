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
        podman::{
            COMPOSE_CONFIG_HASH_LABEL, CONFIG_HASH_LABEL, PodmanOperation,
            compose_marker_network_name,
        },
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

/// Plan-level parameters for the `@resource/podman` resource.
///
/// Tagged by `state: "present" | "absent"`. Mirrors the shape of Ansible's
/// `containers.podman.podman_container` at a conservative subset - enough to
/// declare a long-running container without wrapping every podman flag.
///
/// Drift is decided by the *declared* spec, not the resolved image digest.
/// An upstream change to a floating tag (e.g. `nginx:latest` republished)
/// will not trigger a recreate - pin with `@sha256:...` for digest-level
/// control.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PodmanParams {
    Present {
        name: String,
        image: String,
        command: Option<Vec<String>>,
        env: Option<Vec<String>>,
        ports: Option<Vec<String>>,
        volumes: Option<Vec<String>>,
        restart_policy: Option<String>,
        /// Passed straight through to `podman create --network <value>`.
        /// Common values: `host` (share the host's network namespace -
        /// `ports` becomes meaningless and podman refuses the combination),
        /// `none`, a user-defined network name. `None` here omits the flag
        /// and podman uses its default (rootful: the `podman` bridge
        /// network; rootless: pasta or slirp4netns depending on version).
        network: Option<String>,
        running: Option<bool>,
        /// When set, the state probe (`podman container inspect`) and every
        /// emitted [`PodmanOperation`] run under `sudo -n`. Selects rootful
        /// podman: containers live in root's podman runtime and can
        /// bind-mount root-owned host paths or bind privileged ports.
        sudo: bool,
    },
    Absent {
        name: String,
        /// See [`PodmanParams::Present::sudo`].
        sudo: bool,
    },

    /// Manage a podman-compose project. Drift is detected via a SHA-256 of
    /// the project name + compose file bytes + env_file bytes (+ a versioned
    /// `v1` prefix and the `sudo` flag), stored as a label on a lusid-owned
    /// marker network created alongside the project. See the resource-level
    /// docs in `docs/reference/resources.md` for the full lifecycle and
    /// recovery story.
    ///
    /// The `config_hash` field is populated by [`PodmanParams::prepare`] (run
    /// after `validate_host_paths` in the apply pipeline). At parse-time it
    /// is `None`; resources expansion expects `Some(_)`.
    ComposePresent {
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
        /// Populated by [`PodmanParams::prepare`]; `None` until then.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        config_hash: Option<String>,
        sudo: bool,
    },

    /// Declare a compose project absent. Removes all containers and networks
    /// bearing the `com.docker.compose.project=<project>` label. Named
    /// volumes are preserved (matches `podman-compose down` default).
    ComposeAbsent {
        project: String,
        #[serde(skip, default)]
        project_span: Span,
        sudo: bool,
    },
}

impl ParseParams for PodmanParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let state = fields.take_discriminator(
            "state",
            &["present", "absent", "compose_present", "compose_absent"],
        )?;
        let out = match state {
            "present" => PodmanParams::Present {
                name: fields.required_string("name")?,
                image: fields.required_string("image")?,
                command: fields.optional_string_list("command")?,
                env: fields.optional_string_list("env")?,
                ports: fields.optional_string_list("ports")?,
                volumes: fields.optional_string_list("volumes")?,
                restart_policy: fields.optional_string("restart_policy")?,
                network: fields.optional_string("network")?,
                running: fields.optional_bool("running")?,
                sudo: fields.optional_bool("sudo")?.unwrap_or(false),
            },
            "absent" => PodmanParams::Absent {
                name: fields.required_string("name")?,
                sudo: fields.optional_bool("sudo")?.unwrap_or(false),
            },
            "compose_present" => {
                let (project, project_span) = fields.required_string_spanned("project")?.take();
                validate_project_name(&project, &project_span)?;
                let files_spanned = fields.required_host_path_spanned_list("files")?;
                if files_spanned.is_empty() {
                    // The list parser allows empty; compose requires ≥1.
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
                PodmanParams::ComposePresent {
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
            "compose_absent" => {
                let (project, project_span) = fields.required_string_spanned("project")?.take();
                validate_project_name(&project, &project_span)?;
                PodmanParams::ComposeAbsent {
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
/// `compose_down` shell script (see `operation/src/operations/podman.rs`),
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

impl Display for PodmanParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = |sudo: bool| if sudo { "[sudo] " } else { "" };
        match self {
            PodmanParams::Present {
                name, image, sudo, ..
            } => write!(
                f,
                "{}Podman::Present(name = {name}, image = {image})",
                prefix(*sudo)
            ),
            PodmanParams::Absent { name, sudo } => {
                write!(f, "{}Podman::Absent(name = {name})", prefix(*sudo))
            }
            PodmanParams::ComposePresent { project, sudo, .. } => write!(
                f,
                "{}Podman::ComposePresent(project = {project})",
                prefix(*sudo)
            ),
            PodmanParams::ComposeAbsent { project, sudo, .. } => write!(
                f,
                "{}Podman::ComposeAbsent(project = {project})",
                prefix(*sudo)
            ),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PodmanResource {
    Present {
        name: String,
        image: String,
        command: Option<Vec<String>>,
        env: Vec<String>,
        ports: Vec<String>,
        volumes: Vec<String>,
        restart_policy: Option<String>,
        /// See [`PodmanParams::Present::network`].
        #[serde(default, skip_serializing_if = "Option::is_none")]
        network: Option<String>,
        running: bool,
        /// See [`PodmanParams::Present::sudo`].
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    Absent {
        name: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },

    /// Compose project: bringing-up form. `config_hash` is baked in by
    /// [`PodmanParams::prepare`] and is the SHA-256 of the project's declared
    /// spec.
    ComposePresent {
        project: String,
        files: Vec<FilePath>,
        working_dir: FilePath,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        env_file: Option<FilePath>,
        config_hash: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },

    /// Compose project: bringing-down form.
    ComposeAbsent {
        project: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
}

impl Display for PodmanResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = |sudo: bool| if sudo { "[sudo] " } else { "" };
        match self {
            PodmanResource::Present {
                name,
                image,
                running,
                sudo,
                ..
            } => write!(
                f,
                "{}Podman::Present(name = {name}, image = {image}, running = {running})",
                prefix(*sudo)
            ),
            PodmanResource::Absent { name, sudo } => {
                write!(f, "{}Podman::Absent(name = {name})", prefix(*sudo))
            }
            PodmanResource::ComposePresent { project, sudo, .. } => write!(
                f,
                "{}Podman::ComposePresent(project = {project})",
                prefix(*sudo)
            ),
            PodmanResource::ComposeAbsent { project, sudo } => write!(
                f,
                "{}Podman::ComposeAbsent(project = {project})",
                prefix(*sudo)
            ),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PodmanState {
    Absent,
    Present {
        /// Image reference reported by `podman inspect`. Informational only -
        /// drift detection uses [`config_hash`] below.
        image: String,
        running: bool,
        /// Value of the [`CONFIG_HASH_LABEL`] on the running container, or
        /// `None` if the label is missing. `None` is treated as drift.
        config_hash: Option<String>,
    },
    /// Compose project absent: no marker network found for the project.
    /// Containers labelled with the project may exist (e.g. operator brought
    /// the project up manually) - that is handled at change-time by treating
    /// "missing marker" as drift and emitting `ComposeUp`, which is mostly
    /// idempotent under `podman-compose up -d` on a healthy project.
    ComposeAbsent,
    /// Compose project present: marker network exists, carrying the hash
    /// label from the most recent successful apply.
    ComposePresent {
        /// Value of the [`COMPOSE_CONFIG_HASH_LABEL`] on the marker network.
        /// `None` if the label is missing - treated as drift so old or
        /// foreign markers get recreated.
        config_hash: Option<String>,
    },
}

impl Display for PodmanState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PodmanState::Absent => write!(f, "Podman::Absent"),
            PodmanState::Present { image, running, .. } => {
                write!(f, "Podman::Present(image = {image}, running = {running})")
            }
            PodmanState::ComposeAbsent => write!(f, "Podman::ComposeAbsent"),
            PodmanState::ComposePresent { .. } => write!(f, "Podman::ComposePresent"),
        }
    }
}

#[derive(Error, Debug)]
pub enum PodmanStateError {
    #[error(transparent)]
    Command(#[from] CommandError),

    #[error("failed to parse podman inspect output: {source}\noutput: {output}")]
    ParseInspect {
        #[source]
        source: serde_json::Error,
        output: String,
    },

    #[error("podman inspect returned empty array for container")]
    InspectEmpty,

    #[error("failed to parse podman network inspect output: {source}\noutput: {output}")]
    ParseNetworkInspect {
        #[source]
        source: serde_json::Error,
        output: String,
    },

    #[error("podman network inspect returned empty array for marker network")]
    NetworkInspectEmpty,
}

/// Subset of `podman container inspect` JSON we care about. We deliberately
/// avoid pulling fields that podman normalises in version-dependent ways
/// (`.Config.Env` mixes user values with image defaults, `.HostConfig.Binds`
/// can rewrite SELinux flags, `.HostConfig.PortBindings` is a different
/// shape than the user's port strings) - drift over those fields is detected
/// via the [`CONFIG_HASH_LABEL`] instead.
#[derive(Debug, Clone, Deserialize)]
struct InspectContainer {
    #[serde(rename = "ImageName", default)]
    image_name: String,

    #[serde(rename = "Config", default)]
    config: InspectConfig,

    #[serde(rename = "State", default)]
    state: InspectState,
}

#[derive(Debug, Clone, Default, Deserialize)]
struct InspectConfig {
    #[serde(rename = "Labels", default)]
    labels: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Default, Deserialize)]
struct InspectState {
    #[serde(rename = "Running", default)]
    running: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PodmanChange {
    /// Container doesn't exist - create and optionally start.
    Create {
        name: String,
        image: String,
        command: Option<Vec<String>>,
        env: Vec<String>,
        ports: Vec<String>,
        volumes: Vec<String>,
        restart_policy: Option<String>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        network: Option<String>,
        start: bool,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    /// Container exists with the right config, but needs to be started.
    Start {
        name: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    /// Container exists with the right config, but needs to be stopped.
    Stop {
        name: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    /// Container exists but its config hash no longer matches; remove and recreate.
    Recreate {
        name: String,
        image: String,
        command: Option<Vec<String>>,
        env: Vec<String>,
        ports: Vec<String>,
        volumes: Vec<String>,
        restart_policy: Option<String>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        network: Option<String>,
        start: bool,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    /// Declared absent but the container exists; remove it.
    Remove {
        name: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },

    /// Compose project absent on target but declared present. Bring it up.
    ComposeUp {
        project: String,
        files: Vec<FilePath>,
        working_dir: FilePath,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        env_file: Option<FilePath>,
        config_hash: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    /// Compose project present but its hash no longer matches; tear it down
    /// and bring it back up. Marker is uninstalled first, then `compose
    /// down`, then `compose up`, then marker is reinstalled - chained via
    /// causality `requires` edges (see [`Podman::operations`]).
    ComposeRecreate {
        project: String,
        files: Vec<FilePath>,
        working_dir: FilePath,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        env_file: Option<FilePath>,
        config_hash: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    /// Declared compose_absent and the project is up. Tear it down (containers
    /// + networks; named volumes preserved).
    ComposeDown {
        project: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
}

impl Display for PodmanChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = |sudo: bool| if sudo { "[sudo] " } else { "" };
        match self {
            PodmanChange::Create {
                name, image, sudo, ..
            } => write!(
                f,
                "{}Podman::Create(name = {name}, image = {image})",
                prefix(*sudo)
            ),
            PodmanChange::Start { name, sudo } => {
                write!(f, "{}Podman::Start({name})", prefix(*sudo))
            }
            PodmanChange::Stop { name, sudo } => {
                write!(f, "{}Podman::Stop({name})", prefix(*sudo))
            }
            PodmanChange::Recreate {
                name, image, sudo, ..
            } => write!(
                f,
                "{}Podman::Recreate(name = {name}, image = {image})",
                prefix(*sudo)
            ),
            PodmanChange::Remove { name, sudo } => {
                write!(f, "{}Podman::Remove({name})", prefix(*sudo))
            }
            PodmanChange::ComposeUp { project, sudo, .. } => {
                write!(f, "{}Podman::ComposeUp(project = {project})", prefix(*sudo))
            }
            PodmanChange::ComposeRecreate { project, sudo, .. } => write!(
                f,
                "{}Podman::ComposeRecreate(project = {project})",
                prefix(*sudo)
            ),
            PodmanChange::ComposeDown { project, sudo } => write!(
                f,
                "{}Podman::ComposeDown(project = {project})",
                prefix(*sudo)
            ),
        }
    }
}

impl ResourceChangeTrait for PodmanChange {
    fn kind(&self) -> ChangeKind {
        match self {
            PodmanChange::Create { .. } | PodmanChange::ComposeUp { .. } => ChangeKind::Added,
            PodmanChange::Start { .. }
            | PodmanChange::Stop { .. }
            | PodmanChange::Recreate { .. }
            | PodmanChange::ComposeRecreate { .. } => ChangeKind::Modified,
            PodmanChange::Remove { .. } | PodmanChange::ComposeDown { .. } => ChangeKind::Removed,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Podman;

#[async_trait]
impl ResourceType for Podman {
    const ID: &'static str = "podman";

    type Params = PodmanParams;
    type Resource = PodmanResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        let resource = match params {
            PodmanParams::Present {
                name,
                image,
                command,
                env,
                ports,
                volumes,
                restart_policy,
                network,
                running,
                sudo,
            } => PodmanResource::Present {
                name,
                image,
                command,
                env: env.unwrap_or_default(),
                ports: ports.unwrap_or_default(),
                volumes: volumes.unwrap_or_default(),
                restart_policy,
                network,
                running: running.unwrap_or(true),
                sudo,
            },
            PodmanParams::Absent { name, sudo } => PodmanResource::Absent { name, sudo },
            PodmanParams::ComposePresent {
                project,
                files,
                working_dir,
                env_file,
                config_hash,
                sudo,
                ..
            } => PodmanResource::ComposePresent {
                project,
                files,
                working_dir,
                env_file,
                // Programmer-invariant: `prepare()` must have run before
                // `resources()`. The apply pipeline (lusid-apply) wires this
                // between `validate_host_paths` and resources expansion;
                // missing here means a caller is bypassing the pipeline.
                config_hash: config_hash
                    .expect("PodmanParams::prepare must run before Podman::resources"),
                sudo,
            },
            PodmanParams::ComposeAbsent { project, sudo, .. } => {
                PodmanResource::ComposeAbsent { project, sudo }
            }
        };
        vec![CausalityTree::leaf(CausalityMeta::default(), resource)]
    }

    type State = PodmanState;
    type StateError = PodmanStateError;

    async fn state(
        _ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        match resource {
            PodmanResource::Present { name, sudo, .. } | PodmanResource::Absent { name, sudo } => {
                probe_container_state(name, *sudo).await
            }
            PodmanResource::ComposePresent { project, sudo, .. }
            | PodmanResource::ComposeAbsent { project, sudo } => {
                probe_compose_state(project, *sudo).await
            }
        }
    }

    type Change = PodmanChange;

    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        match (resource, state) {
            // -- compose project --
            (PodmanResource::ComposeAbsent { .. }, PodmanState::ComposeAbsent) => None,
            (
                PodmanResource::ComposeAbsent { project, sudo },
                PodmanState::ComposePresent { .. },
            ) => Some(PodmanChange::ComposeDown {
                project: project.clone(),
                sudo: *sudo,
            }),
            (
                PodmanResource::ComposePresent {
                    project,
                    files,
                    working_dir,
                    env_file,
                    config_hash,
                    sudo,
                },
                PodmanState::ComposeAbsent,
            ) => Some(PodmanChange::ComposeUp {
                project: project.clone(),
                files: files.clone(),
                working_dir: working_dir.clone(),
                env_file: env_file.clone(),
                config_hash: config_hash.clone(),
                sudo: *sudo,
            }),
            (
                PodmanResource::ComposePresent {
                    project,
                    files,
                    working_dir,
                    env_file,
                    config_hash,
                    sudo,
                },
                PodmanState::ComposePresent {
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
                    Some(PodmanChange::ComposeRecreate {
                        project: project.clone(),
                        files: files.clone(),
                        working_dir: working_dir.clone(),
                        env_file: env_file.clone(),
                        config_hash: config_hash.clone(),
                        sudo: *sudo,
                    })
                }
            }

            // -- single container (existing) --
            (PodmanResource::Absent { .. }, PodmanState::Absent) => None,

            (PodmanResource::Absent { name, sudo }, PodmanState::Present { .. }) => {
                Some(PodmanChange::Remove {
                    name: name.clone(),
                    sudo: *sudo,
                })
            }

            (
                PodmanResource::Present {
                    name,
                    image,
                    command,
                    env,
                    ports,
                    volumes,
                    restart_policy,
                    network,
                    running,
                    sudo,
                },
                PodmanState::Absent,
            ) => Some(PodmanChange::Create {
                name: name.clone(),
                image: image.clone(),
                command: command.clone(),
                env: env.clone(),
                ports: ports.clone(),
                volumes: volumes.clone(),
                restart_policy: restart_policy.clone(),
                network: network.clone(),
                start: *running,
                sudo: *sudo,
            }),

            (
                PodmanResource::Present {
                    name,
                    image,
                    command,
                    env,
                    ports,
                    volumes,
                    restart_policy,
                    network,
                    running,
                    sudo,
                },
                PodmanState::Present {
                    running: current_running,
                    config_hash: current_config_hash,
                    ..
                },
            ) => {
                // The hash is the single source of truth for "did the spec
                // change?". Comparing it (instead of the inspect output's
                // image / env / port / volume / cmd / restart fields) sidesteps
                // podman's version-dependent normalisation of those fields.
                // A missing label is also treated as drift so older or foreign
                // containers get adopted-by-recreate, which installs the label.
                let declared_hash = config_hash(
                    image,
                    command.as_ref(),
                    env,
                    ports,
                    volumes,
                    restart_policy.as_ref(),
                    network.as_ref(),
                );
                let hash_matches = current_config_hash.as_deref() == Some(declared_hash.as_str());

                if !hash_matches {
                    Some(PodmanChange::Recreate {
                        name: name.clone(),
                        image: image.clone(),
                        command: command.clone(),
                        env: env.clone(),
                        ports: ports.clone(),
                        volumes: volumes.clone(),
                        restart_policy: restart_policy.clone(),
                        network: network.clone(),
                        start: *running,
                        sudo: *sudo,
                    })
                } else if *running != *current_running {
                    if *running {
                        Some(PodmanChange::Start {
                            name: name.clone(),
                            sudo: *sudo,
                        })
                    } else {
                        Some(PodmanChange::Stop {
                            name: name.clone(),
                            sudo: *sudo,
                        })
                    }
                } else {
                    None
                }
            }

            // `state()` returns the compose state variants only for compose
            // resources, and the container state variants only for container
            // resources, so a cross-family pair here means a probe regression.
            (PodmanResource::Present { .. }, PodmanState::ComposeAbsent)
            | (PodmanResource::Present { .. }, PodmanState::ComposePresent { .. })
            | (PodmanResource::Absent { .. }, PodmanState::ComposeAbsent)
            | (PodmanResource::Absent { .. }, PodmanState::ComposePresent { .. })
            | (PodmanResource::ComposePresent { .. }, PodmanState::Absent)
            | (PodmanResource::ComposePresent { .. }, PodmanState::Present { .. })
            | (PodmanResource::ComposeAbsent { .. }, PodmanState::Absent)
            | (PodmanResource::ComposeAbsent { .. }, PodmanState::Present { .. }) => {
                unreachable!("Podman::state returns the matching state family per resource family")
            }
        }
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        match change {
            PodmanChange::Create {
                name,
                image,
                command,
                env,
                ports,
                volumes,
                restart_policy,
                network,
                start,
                sudo,
            } => create_ops(
                name,
                image,
                command,
                env,
                ports,
                volumes,
                restart_policy,
                network,
                start,
                sudo,
                None,
            ),
            PodmanChange::Start { name, sudo } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                Operation::Podman(PodmanOperation::Start { name, sudo }),
            )],
            PodmanChange::Stop { name, sudo } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                Operation::Podman(PodmanOperation::Stop { name, sudo }),
            )],
            PodmanChange::Recreate {
                name,
                image,
                command,
                env,
                ports,
                volumes,
                restart_policy,
                network,
                start,
                sudo,
            } => create_ops(
                name,
                image,
                command,
                env,
                ports,
                volumes,
                restart_policy,
                network,
                start,
                sudo,
                Some("remove"),
            ),
            PodmanChange::Remove { name, sudo } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                Operation::Podman(PodmanOperation::Remove { name, sudo }),
            )],
            PodmanChange::ComposeUp {
                project,
                files,
                working_dir,
                env_file,
                config_hash,
                sudo,
            } => compose_up_ops(
                project,
                files,
                working_dir,
                env_file,
                config_hash,
                sudo,
                None,
            ),
            PodmanChange::ComposeRecreate {
                project,
                files,
                working_dir,
                env_file,
                config_hash,
                sudo,
            } => compose_recreate_ops(project, files, working_dir, env_file, config_hash, sudo),
            PodmanChange::ComposeDown { project, sudo } => compose_down_ops(project, sudo),
        }
    }
}

/// Build the Create (+ optional Start) operations, optionally preceded by a
/// Remove op when `remove_id` is `Some`. Used for both `Create` and `Recreate`
/// changes to keep the causality wiring in one place.
#[allow(clippy::too_many_arguments)]
fn create_ops(
    name: String,
    image: String,
    command: Option<Vec<String>>,
    env: Vec<String>,
    ports: Vec<String>,
    volumes: Vec<String>,
    restart_policy: Option<String>,
    network: Option<String>,
    start: bool,
    sudo: bool,
    remove_id: Option<&'static str>,
) -> Vec<CausalityTree<Operation>> {
    let mut ops: Vec<CausalityTree<Operation>> = Vec::new();

    if let Some(id) = remove_id {
        ops.push(CausalityTree::leaf(
            CausalityMeta::id(id.into()),
            Operation::Podman(PodmanOperation::Remove {
                name: name.clone(),
                sudo,
            }),
        ));
    }

    let hash = config_hash(
        &image,
        command.as_ref(),
        &env,
        &ports,
        &volumes,
        restart_policy.as_ref(),
        network.as_ref(),
    );

    let create_meta = CausalityMeta {
        id: Some("create".into()),
        requires: remove_id.map(|id| vec![id.into()]).unwrap_or_default(),
        required_by: vec![],
    };
    ops.push(CausalityTree::leaf(
        create_meta,
        Operation::Podman(PodmanOperation::Create {
            name: name.clone(),
            image,
            command,
            env,
            ports,
            volumes,
            restart_policy,
            network,
            config_hash: hash,
            sudo,
        }),
    ));

    if start {
        ops.push(CausalityTree::leaf(
            CausalityMeta::requires(vec!["create".into()]),
            Operation::Podman(PodmanOperation::Start { name, sudo }),
        ));
    }

    ops
}

/// Compute the SHA-256 of the canonical representation of a container's
/// declared spec. Stored as the [`CONFIG_HASH_LABEL`] value at create time
/// and compared against on every state observation to detect drift.
///
/// Inputs are taken in canonical form so that logically-equivalent
/// declarations (e.g. `nginx:latest` vs `docker.io/library/nginx:latest`)
/// produce the same hash. Field order is preserved within each list - for
/// `env` in particular, `KEY=a` then `KEY=b` is meaningfully different from
/// the reverse (last-write-wins under `podman create -e`), so reordering
/// should be drift.
///
/// `running` is intentionally excluded: it's a runtime state that can flip
/// without a recreate, handled by Start/Stop in [`Podman::change`].
fn config_hash(
    image: &str,
    command: Option<&Vec<String>>,
    env: &[String],
    ports: &[String],
    volumes: &[String],
    restart_policy: Option<&String>,
    network: Option<&String>,
) -> String {
    /// Stable, declaration-ordered serialisation target for hashing. Adding,
    /// removing, or reordering a field changes the hash for every existing
    /// container - that is, every container will be recreated once on the
    /// next apply. Treat this as a versioned wire format.
    #[derive(Serialize)]
    struct ConfigForHash<'a> {
        image: &'a str,
        command: Option<&'a Vec<String>>,
        env: &'a [String],
        ports: &'a [String],
        volumes: &'a [String],
        restart_policy: Option<&'a String>,
        network: Option<&'a String>,
    }

    let canonical_image = canonicalize_image(image);
    let cfg = ConfigForHash {
        image: &canonical_image,
        command,
        env,
        ports,
        volumes,
        restart_policy,
        network,
    };
    // Serialising a fixed-shape struct of owned-string-like fields cannot fail.
    let bytes = serde_json::to_vec(&cfg).expect("ConfigForHash serialisation is infallible");

    let digest = Sha256::digest(&bytes);
    let mut out = String::with_capacity(digest.len() * 2);
    for byte in digest {
        let _ = write!(out, "{byte:02x}");
    }
    out
}

/// Best-effort canonicalisation of a container image reference to the form
/// that `podman inspect` typically reports (`<registry>/<repo>:<tag>` or
/// `<registry>/<repo>@<digest>`). Used to keep [`config_hash`] stable across
/// short and fully-qualified declarations of the same image.
fn canonicalize_image(reference: &str) -> String {
    // Split off a digest if present. The digest itself is already unambiguous,
    // but the name preceding it still needs the same registry/repo prefixing
    // as a tagged reference. OCI also permits `name:tag@digest`, so the tag
    // splitting below still applies to the `head` either way.
    let (head, digest) = match reference.split_once('@') {
        Some((head, digest)) => (head, Some(digest.to_string())),
        None => (reference, None),
    };

    // Split off a tag from the head, if any. The tag delimiter is the *last*
    // `:`, but only when it's after the final `/` (otherwise it's a registry
    // port like `localhost:5000/foo`).
    let (name, tag) = match head.rsplit_once(':') {
        Some((name, tag)) if !tag.contains('/') => (name.to_string(), Some(tag.to_string())),
        _ => (head.to_string(), None),
    };

    // Default to `:latest` only when nothing pins the image. A digest reference
    // without an explicit tag is left tag-less, which is the form `inspect`
    // reports for digest-pinned containers.
    let tag = match (&tag, &digest) {
        (None, None) => Some("latest".to_string()),
        _ => tag,
    };

    // Does `name` start with a registry host? The OCI rule: if the first
    // path segment contains a `.` or `:`, or is exactly `localhost`, it's
    // treated as a registry host; otherwise it defaults to `docker.io`.
    let name = match name.split_once('/') {
        Some((first, _)) if first.contains('.') || first.contains(':') || first == "localhost" => {
            name
        }
        Some(_) => format!("docker.io/{name}"),
        None => format!("docker.io/library/{name}"),
    };

    let mut out = name;
    if let Some(tag) = tag {
        out.push(':');
        out.push_str(&tag);
    }
    if let Some(digest) = digest {
        out.push('@');
        out.push_str(&digest);
    }
    out
}

// ---------- compose ----------

/// Probe a single container's state by name. Extracted from `state` so the
/// compose branch can sit alongside without entangling.
async fn probe_container_state(name: &str, sudo: bool) -> Result<PodmanState, PodmanStateError> {
    // `podman container inspect` exits non-zero (125) when the container is
    // missing, which `outcome()` surfaces without raising. Distinguishing
    // "absent" from "podman itself failed" via stderr is unreliable across
    // versions, so we treat any non-success as Absent. A broken podman
    // install will then surface at apply-time on the first create.
    //
    // Rootful and rootless podman are entirely separate runtimes, so the
    // probe must run under sudo when the resource declares it - otherwise
    // we'd inspect the worm-user runtime while the container actually
    // lives in root's, see "Absent" every time, and recreate forever.
    let mut cmd = Command::new("podman");
    cmd.args(["container", "inspect", name]);
    let mut cmd = if sudo { cmd.sudo() } else { cmd };
    let outcome = cmd.outcome().await?;
    if !outcome.status.success() {
        return Ok(PodmanState::Absent);
    }

    let containers: Vec<InspectContainer> =
        serde_json::from_slice(&outcome.stdout).map_err(|source| {
            PodmanStateError::ParseInspect {
                source,
                output: String::from_utf8_lossy(&outcome.stdout).into_owned(),
            }
        })?;
    let container = containers
        .into_iter()
        .next()
        .ok_or(PodmanStateError::InspectEmpty)?;

    let config_hash = container.config.labels.get(CONFIG_HASH_LABEL).cloned();

    Ok(PodmanState::Present {
        image: container.image_name,
        running: container.state.running,
        config_hash,
    })
}

/// Probe a compose project's state by inspecting its lusid marker network.
/// `podman network inspect` exits non-zero when the network is missing;
/// treat any non-success as `ComposeAbsent` for the same reason
/// [`probe_container_state`] treats container-inspect non-success as Absent.
async fn probe_compose_state(project: &str, sudo: bool) -> Result<PodmanState, PodmanStateError> {
    let marker = compose_marker_network_name(project);
    let mut cmd = Command::new("podman");
    cmd.args(["network", "inspect", &marker]);
    let mut cmd = if sudo { cmd.sudo() } else { cmd };
    let outcome = cmd.outcome().await?;
    if !outcome.status.success() {
        return Ok(PodmanState::ComposeAbsent);
    }

    let networks: Vec<InspectNetwork> =
        serde_json::from_slice(&outcome.stdout).map_err(|source| {
            PodmanStateError::ParseNetworkInspect {
                source,
                output: String::from_utf8_lossy(&outcome.stdout).into_owned(),
            }
        })?;
    let network = networks
        .into_iter()
        .next()
        .ok_or(PodmanStateError::NetworkInspectEmpty)?;
    let config_hash = network.labels.get(COMPOSE_CONFIG_HASH_LABEL).cloned();
    Ok(PodmanState::ComposePresent { config_hash })
}

/// Subset of `podman network inspect` JSON we care about: just the labels
/// map, where lusid stamps its config-hash on the marker network.
#[derive(Debug, Clone, Default, Deserialize)]
struct InspectNetwork {
    #[serde(rename = "labels", default)]
    labels: BTreeMap<String, String>,
}

/// Build the operation list for a `ComposeUp`. Optionally preceded by a
/// down + marker-uninstall pair when `recreate` is `Some`. Used by both
/// `ComposeUp` and `ComposeRecreate` changes so the causality wiring lives
/// in one place.
fn compose_up_ops(
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
        Operation::Podman(PodmanOperation::ComposeUp {
            project: project.clone(),
            files,
            working_dir,
            env_file,
            sudo,
        }),
    ));

    ops.push(CausalityTree::leaf(
        CausalityMeta::requires(vec!["compose_up".into()]),
        Operation::Podman(PodmanOperation::ComposeMarkerInstall {
            project,
            config_hash,
            sudo,
        }),
    ));

    ops
}

/// Build the operation list for a `ComposeDown`: marker uninstall, then
/// the project teardown. Causality IDs are scoped per branch by
/// `map_plan_subitems`'s `scope_id` machinery (see AGENTS.md), so the same
/// IDs are reused by `compose_recreate_ops` without collision.
///
/// Marker uninstall runs first so a half-failed down does not leave a
/// marker pointing at a stale project.
fn compose_down_ops(project: String, sudo: bool) -> Vec<CausalityTree<Operation>> {
    vec![
        CausalityTree::leaf(
            CausalityMeta::id("compose_marker_uninstall".into()),
            Operation::Podman(PodmanOperation::ComposeMarkerUninstall {
                project: project.clone(),
                sudo,
            }),
        ),
        CausalityTree::leaf(
            CausalityMeta::requires(vec!["compose_marker_uninstall".into()]),
            Operation::Podman(PodmanOperation::ComposeDown { project, sudo }),
        ),
    ]
}

/// Build the operation list for a `ComposeRecreate`: tear down, then bring
/// back up, with causality edges so the scheduler runs them in order.
fn compose_recreate_ops(
    project: String,
    files: Vec<FilePath>,
    working_dir: FilePath,
    env_file: Option<FilePath>,
    config_hash: String,
    sudo: bool,
) -> Vec<CausalityTree<Operation>> {
    let mut ops = compose_down_ops(project.clone(), sudo);
    let up_ops = compose_up_ops(
        project,
        files,
        working_dir,
        env_file,
        config_hash,
        sudo,
        Some("compose_down"),
    );
    ops.extend(up_ops);
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

/// Span-aware errors surfaced by [`PodmanParams::prepare`].
#[derive(Debug, Error)]
pub enum PodmanPrepareError {
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

impl PodmanParams {
    /// Read any host-side files referenced by the params (e.g. compose YAML)
    /// and bake the resulting hash into the params. Called by the apply
    /// pipeline between [`crate::ResourceParams::validate_host_paths`] and
    /// the `resources()` expansion.
    ///
    /// For non-compose variants this is a pass-through.
    pub async fn prepare(self) -> Result<Self, PodmanPrepareError> {
        match self {
            PodmanParams::ComposePresent {
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
                        PodmanPrepareError::ComposeFileRead {
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
                            PodmanPrepareError::ComposeEnvFileRead {
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
                Ok(PodmanParams::ComposePresent {
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

    struct ResourceSpec {
        image: String,
        command: Option<Vec<String>>,
        env: Vec<String>,
        ports: Vec<String>,
        volumes: Vec<String>,
        restart_policy: Option<String>,
        network: Option<String>,
        running: bool,
    }

    impl Default for ResourceSpec {
        fn default() -> Self {
            Self {
                image: "docker.io/library/nginx:latest".into(),
                command: None,
                env: vec![],
                ports: vec!["8080:80".into()],
                volumes: vec![],
                restart_policy: Some("unless-stopped".into()),
                network: None,
                running: true,
            }
        }
    }

    fn resource(spec: ResourceSpec) -> PodmanResource {
        PodmanResource::Present {
            name: "web".into(),
            image: spec.image,
            command: spec.command,
            env: spec.env,
            ports: spec.ports,
            volumes: spec.volumes,
            restart_policy: spec.restart_policy,
            network: spec.network,
            running: spec.running,
            sudo: false,
        }
    }

    /// Build a state matching `spec`'s declared config - i.e. the label hash
    /// is computed from the same inputs. Use this for "no drift" tests.
    fn state_matching(spec: &ResourceSpec) -> PodmanState {
        PodmanState::Present {
            image: canonicalize_image(&spec.image),
            running: spec.running,
            config_hash: Some(config_hash(
                &spec.image,
                spec.command.as_ref(),
                &spec.env,
                &spec.ports,
                &spec.volumes,
                spec.restart_policy.as_ref(),
                spec.network.as_ref(),
            )),
        }
    }

    #[test]
    fn change_none_when_hash_matches() {
        let spec = ResourceSpec::default();
        let state = state_matching(&spec);
        assert!(Podman::change(&resource(spec), &state).is_none());
    }

    #[test]
    fn change_create_when_absent() {
        let change = Podman::change(&resource(ResourceSpec::default()), &PodmanState::Absent)
            .expect("change");
        assert!(matches!(change, PodmanChange::Create { start: true, .. }));
    }

    #[test]
    fn change_recreate_when_image_differs() {
        let spec = ResourceSpec::default();
        let other = ResourceSpec {
            image: "docker.io/library/nginx:1.25".into(),
            ..ResourceSpec::default()
        };
        let current = state_matching(&other);
        let change = Podman::change(&resource(spec), &current).expect("change");
        assert!(matches!(change, PodmanChange::Recreate { .. }));
    }

    #[test]
    fn change_none_when_image_short_form_matches_qualified() {
        // Declared short form should hash the same as its fully-qualified form.
        let qualified = ResourceSpec::default();
        let short = ResourceSpec {
            image: "nginx:latest".into(),
            ..ResourceSpec::default()
        };
        let current = state_matching(&qualified);
        assert!(Podman::change(&resource(short), &current).is_none());
    }

    #[test]
    fn change_recreate_when_command_differs() {
        let declared = ResourceSpec {
            command: Some(vec!["nginx".into(), "-g".into(), "daemon off;".into()]),
            ..ResourceSpec::default()
        };
        let current = state_matching(&ResourceSpec {
            command: Some(vec!["nginx".into()]),
            ..ResourceSpec::default()
        });
        let change = Podman::change(&resource(declared), &current).expect("change");
        assert!(matches!(change, PodmanChange::Recreate { .. }));
    }

    #[test]
    fn change_recreate_when_restart_policy_differs() {
        let declared = ResourceSpec::default();
        let current = state_matching(&ResourceSpec {
            restart_policy: Some("always".into()),
            ..ResourceSpec::default()
        });
        let change = Podman::change(&resource(declared), &current).expect("change");
        assert!(matches!(change, PodmanChange::Recreate { .. }));
    }

    #[test]
    fn change_recreate_when_env_differs() {
        let declared = ResourceSpec {
            env: vec!["FOO=bar".into()],
            ..ResourceSpec::default()
        };
        let current = state_matching(&ResourceSpec {
            env: vec!["FOO=baz".into()],
            ..ResourceSpec::default()
        });
        let change = Podman::change(&resource(declared), &current).expect("change");
        assert!(matches!(change, PodmanChange::Recreate { .. }));
    }

    #[test]
    fn change_recreate_when_ports_differ() {
        let declared = ResourceSpec {
            ports: vec!["8080:80".into()],
            ..ResourceSpec::default()
        };
        let current = state_matching(&ResourceSpec {
            ports: vec!["9090:80".into()],
            ..ResourceSpec::default()
        });
        let change = Podman::change(&resource(declared), &current).expect("change");
        assert!(matches!(change, PodmanChange::Recreate { .. }));
    }

    #[test]
    fn change_recreate_when_volumes_differ() {
        let declared = ResourceSpec {
            volumes: vec!["/srv/data:/data".into()],
            ..ResourceSpec::default()
        };
        let current = state_matching(&ResourceSpec::default());
        let change = Podman::change(&resource(declared), &current).expect("change");
        assert!(matches!(change, PodmanChange::Recreate { .. }));
    }

    #[test]
    fn change_recreate_when_env_order_differs() {
        // Order matters for env (last-write-wins under podman -e KEY=...);
        // reordering the user's declared list is treated as drift.
        let declared = ResourceSpec {
            env: vec!["A=1".into(), "B=2".into()],
            ..ResourceSpec::default()
        };
        let current = state_matching(&ResourceSpec {
            env: vec!["B=2".into(), "A=1".into()],
            ..ResourceSpec::default()
        });
        let change = Podman::change(&resource(declared), &current).expect("change");
        assert!(matches!(change, PodmanChange::Recreate { .. }));
    }

    #[test]
    fn change_recreate_when_label_missing() {
        // A container without our label is either pre-hash (older lusid) or
        // foreign. Recreate so the label is installed and we own state going
        // forward.
        let current = PodmanState::Present {
            image: "docker.io/library/nginx:latest".into(),
            running: true,
            config_hash: None,
        };
        let change = Podman::change(&resource(ResourceSpec::default()), &current).expect("change");
        assert!(matches!(change, PodmanChange::Recreate { .. }));
    }

    #[test]
    fn change_start_when_only_running_differs() {
        let spec = ResourceSpec::default();
        // Hash matches, only `running` flips - should be Start, not Recreate.
        let current = match state_matching(&spec) {
            PodmanState::Present {
                image, config_hash, ..
            } => PodmanState::Present {
                image,
                running: false,
                config_hash,
            },
            other => {
                unreachable!("state_matching always returns Present for container, got {other:?}")
            }
        };
        let change = Podman::change(&resource(spec), &current).expect("change");
        assert!(matches!(change, PodmanChange::Start { .. }));
    }

    #[test]
    fn change_stop_when_declared_not_running() {
        let declared = ResourceSpec {
            running: false,
            ..ResourceSpec::default()
        };
        // The state's hash must be computed against the same logical config
        // (running isn't part of the hash, so this is fine).
        let current = state_matching(&declared);
        let current = match current {
            PodmanState::Present {
                image, config_hash, ..
            } => PodmanState::Present {
                image,
                running: true,
                config_hash,
            },
            other => {
                unreachable!("state_matching always returns Present for container, got {other:?}")
            }
        };
        let change = Podman::change(&resource(declared), &current).expect("change");
        assert!(matches!(change, PodmanChange::Stop { .. }));
    }

    #[test]
    fn change_remove_when_declared_absent_but_present() {
        let declared = PodmanResource::Absent {
            name: "web".into(),
            sudo: false,
        };
        let current = state_matching(&ResourceSpec::default());
        let change = Podman::change(&declared, &current).expect("change");
        assert!(matches!(change, PodmanChange::Remove { .. }));
    }

    #[test]
    fn change_none_when_absent_matches() {
        let declared = PodmanResource::Absent {
            name: "web".into(),
            sudo: false,
        };
        assert!(Podman::change(&declared, &PodmanState::Absent).is_none());
    }

    #[test]
    fn config_hash_is_stable_for_equivalent_image_refs() {
        let a = config_hash("nginx", None, &[], &[], &[], None, None);
        let b = config_hash(
            "docker.io/library/nginx:latest",
            None,
            &[],
            &[],
            &[],
            None,
            None,
        );
        assert_eq!(a, b);
    }

    #[test]
    fn config_hash_changes_when_any_input_changes() {
        let base = config_hash(
            "nginx",
            Some(&vec!["sh".into()]),
            &["A=1".into()],
            &["80:80".into()],
            &["/x:/x".into()],
            Some(&"always".into()),
            None,
        );

        // Each variation should produce a distinct hash. We don't assert exact
        // values - just that no two collide and none equals the base.
        let variants: Vec<String> = vec![
            config_hash(
                "nginx:1.25",
                Some(&vec!["sh".into()]),
                &["A=1".into()],
                &["80:80".into()],
                &["/x:/x".into()],
                Some(&"always".into()),
                None,
            ),
            config_hash(
                "nginx",
                Some(&vec!["bash".into()]),
                &["A=1".into()],
                &["80:80".into()],
                &["/x:/x".into()],
                Some(&"always".into()),
                None,
            ),
            config_hash(
                "nginx",
                Some(&vec!["sh".into()]),
                &["A=2".into()],
                &["80:80".into()],
                &["/x:/x".into()],
                Some(&"always".into()),
                None,
            ),
            config_hash(
                "nginx",
                Some(&vec!["sh".into()]),
                &["A=1".into()],
                &["81:80".into()],
                &["/x:/x".into()],
                Some(&"always".into()),
                None,
            ),
            config_hash(
                "nginx",
                Some(&vec!["sh".into()]),
                &["A=1".into()],
                &["80:80".into()],
                &["/y:/y".into()],
                Some(&"always".into()),
                None,
            ),
            config_hash(
                "nginx",
                Some(&vec!["sh".into()]),
                &["A=1".into()],
                &["80:80".into()],
                &["/x:/x".into()],
                Some(&"unless-stopped".into()),
                None,
            ),
            config_hash(
                "nginx",
                None,
                &["A=1".into()],
                &["80:80".into()],
                &["/x:/x".into()],
                Some(&"always".into()),
                None,
            ),
            config_hash(
                "nginx",
                Some(&vec!["sh".into()]),
                &["A=1".into()],
                &["80:80".into()],
                &["/x:/x".into()],
                None,
                None,
            ),
            config_hash(
                "nginx",
                Some(&vec!["sh".into()]),
                &["A=1".into()],
                &["80:80".into()],
                &["/x:/x".into()],
                Some(&"always".into()),
                Some(&"host".into()),
            ),
        ];
        for v in &variants {
            assert_ne!(*v, base, "variant collided with base: {v}");
        }
    }

    #[test]
    fn change_recreate_when_network_differs() {
        let declared = ResourceSpec {
            network: Some("host".into()),
            ..ResourceSpec::default()
        };
        let current = state_matching(&ResourceSpec::default());
        let change = Podman::change(&resource(declared), &current).expect("change");
        assert!(matches!(change, PodmanChange::Recreate { .. }));
    }

    #[test]
    fn canonicalize_bare_image_adds_docker_hub_and_latest() {
        assert_eq!(
            canonicalize_image("nginx"),
            "docker.io/library/nginx:latest"
        );
    }

    #[test]
    fn canonicalize_tagged_bare_image_adds_docker_hub() {
        assert_eq!(
            canonicalize_image("nginx:1.25"),
            "docker.io/library/nginx:1.25"
        );
    }

    #[test]
    fn canonicalize_user_repo_adds_docker_hub() {
        assert_eq!(
            canonicalize_image("bitnami/redis"),
            "docker.io/bitnami/redis:latest"
        );
    }

    #[test]
    fn canonicalize_fully_qualified_passthrough() {
        assert_eq!(
            canonicalize_image("ghcr.io/foo/bar:v1"),
            "ghcr.io/foo/bar:v1"
        );
    }

    #[test]
    fn canonicalize_localhost_registry_preserved() {
        assert_eq!(
            canonicalize_image("localhost:5000/app:dev"),
            "localhost:5000/app:dev"
        );
    }

    #[test]
    fn canonicalize_fully_qualified_digest_unchanged() {
        let digest = "docker.io/library/nginx@sha256:deadbeef";
        assert_eq!(canonicalize_image(digest), digest);
    }

    #[test]
    fn canonicalize_bare_digest_adds_docker_hub() {
        // The digest itself is unambiguous, but the name still needs the
        // registry prefix so it matches the form `podman inspect` reports.
        assert_eq!(
            canonicalize_image("nginx@sha256:deadbeef"),
            "docker.io/library/nginx@sha256:deadbeef"
        );
    }

    #[test]
    fn canonicalize_user_repo_digest_adds_docker_hub() {
        assert_eq!(
            canonicalize_image("bitnami/redis@sha256:deadbeef"),
            "docker.io/bitnami/redis@sha256:deadbeef"
        );
    }

    // ---------- compose ----------

    fn compose_resource(project: &str, hash: &str) -> PodmanResource {
        PodmanResource::ComposePresent {
            project: project.into(),
            files: vec![FilePath::new("/etc/compose/app.yaml")],
            working_dir: FilePath::new("/etc/compose"),
            env_file: None,
            config_hash: hash.into(),
            sudo: false,
        }
    }

    #[test]
    fn compose_change_none_when_hash_matches() {
        let r = compose_resource("app", "abc123");
        let s = PodmanState::ComposePresent {
            config_hash: Some("abc123".into()),
        };
        assert!(Podman::change(&r, &s).is_none());
    }

    #[test]
    fn compose_change_up_when_marker_absent() {
        let r = compose_resource("app", "abc123");
        let s = PodmanState::ComposeAbsent;
        let change = Podman::change(&r, &s).expect("change");
        assert!(matches!(change, PodmanChange::ComposeUp { .. }));
    }

    #[test]
    fn compose_change_recreate_when_hash_differs() {
        let r = compose_resource("app", "newhash");
        let s = PodmanState::ComposePresent {
            config_hash: Some("oldhash".into()),
        };
        let change = Podman::change(&r, &s).expect("change");
        assert!(matches!(change, PodmanChange::ComposeRecreate { .. }));
    }

    #[test]
    fn compose_change_recreate_when_label_missing() {
        // Foreign marker (e.g. older lusid version) - re-up so the new
        // label gets installed and we own state going forward.
        let r = compose_resource("app", "abc123");
        let s = PodmanState::ComposePresent { config_hash: None };
        let change = Podman::change(&r, &s).expect("change");
        assert!(matches!(change, PodmanChange::ComposeRecreate { .. }));
    }

    #[test]
    fn compose_change_down_when_declared_absent_and_present() {
        let r = PodmanResource::ComposeAbsent {
            project: "app".into(),
            sudo: false,
        };
        let s = PodmanState::ComposePresent {
            config_hash: Some("abc123".into()),
        };
        let change = Podman::change(&r, &s).expect("change");
        assert!(matches!(change, PodmanChange::ComposeDown { .. }));
    }

    #[test]
    fn compose_change_none_when_absent_matches() {
        let r = PodmanResource::ComposeAbsent {
            project: "app".into(),
            sudo: false,
        };
        let s = PodmanState::ComposeAbsent;
        assert!(Podman::change(&r, &s).is_none());
    }

    #[test]
    fn compose_change_up_carries_files_and_sudo() {
        let r = PodmanResource::ComposePresent {
            project: "app".into(),
            files: vec![FilePath::new("/c/a.yaml"), FilePath::new("/c/b.yaml")],
            working_dir: FilePath::new("/c"),
            env_file: Some(FilePath::new("/c/.env")),
            config_hash: "h".into(),
            sudo: true,
        };
        let change = Podman::change(&r, &PodmanState::ComposeAbsent).expect("change");
        match change {
            PodmanChange::ComposeUp {
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
            other => panic!("expected ComposeUp, got {other:?}"),
        }
    }

    fn file_bytes(path: &str, body: &[u8]) -> (FilePath, Vec<u8>) {
        (FilePath::new(path), body.to_vec())
    }

    #[test]
    fn compose_hash_changes_when_project_name_changes() {
        let files = vec![file_bytes(
            "/c/a.yaml",
            b"services:\n  web:\n    image: nginx\n",
        )];
        let a = compose_config_hash("app", false, &files, None);
        let b = compose_config_hash("APP-renamed", false, &files, None);
        assert_ne!(a, b);
    }

    #[test]
    fn compose_hash_changes_when_file_bytes_change() {
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
    fn compose_hash_changes_when_file_order_changes() {
        // Compose merge semantics make file order meaningful; reorder = drift.
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
    fn compose_hash_changes_when_env_file_added() {
        let files = vec![file_bytes("/c/a.yaml", b"x")];
        let env_path = FilePath::new("/c/.env");
        let env_bytes: &[u8] = b"X=1\n";
        let without = compose_config_hash("app", false, &files, None);
        let with = compose_config_hash("app", false, &files, Some((&env_path, env_bytes)));
        assert_ne!(without, with);
    }

    #[test]
    fn compose_hash_changes_when_env_file_contents_change() {
        let files = vec![file_bytes("/c/a.yaml", b"x")];
        let env_path = FilePath::new("/c/.env");
        let a = compose_config_hash("app", false, &files, Some((&env_path, b"X=1\n")));
        let b = compose_config_hash("app", false, &files, Some((&env_path, b"X=2\n")));
        assert_ne!(a, b);
    }

    #[test]
    fn compose_hash_changes_when_sudo_flag_flips() {
        let files = vec![file_bytes("/c/a.yaml", b"x")];
        let rootless = compose_config_hash("app", false, &files, None);
        let rootful = compose_config_hash("app", true, &files, None);
        assert_ne!(rootless, rootful);
    }

    #[test]
    fn compose_hash_stable_across_repeat_compute() {
        // Regression: a HashMap-iteration-ordering bug in a future refactor
        // would show up as nondeterminism here.
        let files = vec![file_bytes("/c/a.yaml", b"a"), file_bytes("/c/b.yaml", b"b")];
        assert_eq!(
            compose_config_hash("app", false, &files, None),
            compose_config_hash("app", false, &files, None)
        );
    }

    // -- parser ------------------------------------------------------------

    fn empty_span() -> rimu::Span {
        use rimu::SourceId;
        rimu::Span::new(SourceId::empty(), 0, 0)
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
    fn compose_present_parses_minimal_form() {
        let params = PodmanParams::parse_params(object(vec![
            ("state", Value::String("compose_present".into())),
            ("project", Value::String("myapp".into())),
            ("files", Value::List(vec![s(host_path("/c/app.yaml"))])),
        ]))
        .expect("parse");
        match params {
            PodmanParams::ComposePresent {
                project,
                files,
                sudo,
                ..
            } => {
                assert_eq!(project, "myapp");
                assert_eq!(files.len(), 1);
                assert!(!sudo);
            }
            other => panic!("expected ComposePresent, got {other:?}"),
        }
    }

    #[test]
    fn compose_present_defaults_working_dir_to_first_file_parent() {
        let params = PodmanParams::parse_params(object(vec![
            ("state", Value::String("compose_present".into())),
            ("project", Value::String("myapp".into())),
            (
                "files",
                Value::List(vec![s(host_path("/etc/compose/app.yaml"))]),
            ),
        ]))
        .expect("parse");
        match params {
            PodmanParams::ComposePresent { working_dir, .. } => {
                assert_eq!(working_dir.as_path().to_str().unwrap(), "/etc/compose");
            }
            other => panic!("expected ComposePresent, got {other:?}"),
        }
    }

    #[test]
    fn compose_present_carries_env_file_when_set() {
        let params = PodmanParams::parse_params(object(vec![
            ("state", Value::String("compose_present".into())),
            ("project", Value::String("myapp".into())),
            ("files", Value::List(vec![s(host_path("/c/app.yaml"))])),
            ("env_file", host_path("/c/.env")),
        ]))
        .expect("parse");
        match params {
            PodmanParams::ComposePresent { env_file, .. } => {
                assert!(env_file.is_some());
            }
            other => panic!("expected ComposePresent, got {other:?}"),
        }
    }

    #[test]
    fn compose_present_rejects_invalid_project_name() {
        let err = PodmanParams::parse_params(object(vec![
            ("state", Value::String("compose_present".into())),
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
    fn compose_present_rejects_empty_files() {
        let err = PodmanParams::parse_params(object(vec![
            ("state", Value::String("compose_present".into())),
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
    fn compose_absent_parses_minimal_form() {
        let params = PodmanParams::parse_params(object(vec![
            ("state", Value::String("compose_absent".into())),
            ("project", Value::String("myapp".into())),
        ]))
        .expect("parse");
        match params {
            PodmanParams::ComposeAbsent { project, sudo, .. } => {
                assert_eq!(project, "myapp");
                assert!(!sudo);
            }
            other => panic!("expected ComposeAbsent, got {other:?}"),
        }
    }

    #[test]
    fn compose_absent_rejects_invalid_project_name() {
        let err = PodmanParams::parse_params(object(vec![
            ("state", Value::String("compose_absent".into())),
            ("project", Value::String("".into())),
        ]))
        .expect_err("should reject empty project");
        assert!(matches!(err.inner(), ParseError::InvalidValue { .. }));
    }

    // -- operations lowering -------------------------------------------------

    #[test]
    fn compose_up_change_lowers_to_up_plus_marker_install() {
        let change = PodmanChange::ComposeUp {
            project: "app".into(),
            files: vec![FilePath::new("/c/a.yaml")],
            working_dir: FilePath::new("/c"),
            env_file: None,
            config_hash: "h".into(),
            sudo: false,
        };
        let ops = Podman::operations(change);
        // Two leaves: compose_up then compose_marker_install requires it.
        assert_eq!(ops.len(), 2);
    }

    #[test]
    fn compose_recreate_change_lowers_to_four_ops_in_order() {
        let change = PodmanChange::ComposeRecreate {
            project: "app".into(),
            files: vec![FilePath::new("/c/a.yaml")],
            working_dir: FilePath::new("/c"),
            env_file: None,
            config_hash: "h".into(),
            sudo: false,
        };
        let ops = Podman::operations(change);
        // marker uninstall, down, up, marker install.
        assert_eq!(ops.len(), 4);
    }

    #[test]
    fn compose_down_change_lowers_to_marker_uninstall_plus_down() {
        let change = PodmanChange::ComposeDown {
            project: "app".into(),
            sudo: false,
        };
        let ops = Podman::operations(change);
        assert_eq!(ops.len(), 2);
    }
}
