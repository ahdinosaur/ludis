use std::collections::BTreeMap;
use std::fmt::Display;
use std::fmt::Write;

use async_trait::async_trait;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_operation::{
    Operation,
    operations::podman::{CONFIG_HASH_LABEL, PodmanOperation},
};
use lusid_params::{ParseError, ParseParams, StructFields};
use lusid_view::impl_display_render;
use rimu::{Spanned, Value};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use thiserror::Error;

use crate::ResourceType;

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
#[derive(Debug, Clone)]
pub enum PodmanParams {
    Present {
        name: String,
        image: String,
        command: Option<Vec<String>>,
        env: Option<Vec<String>>,
        ports: Option<Vec<String>>,
        volumes: Option<Vec<String>>,
        restart_policy: Option<String>,
        running: Option<bool>,
    },
    Absent {
        name: String,
    },
}

impl ParseParams for PodmanParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let state = fields.take_discriminator("state", &["present", "absent"])?;
        let out = match state {
            "present" => PodmanParams::Present {
                name: fields.required_string("name")?,
                image: fields.required_string("image")?,
                command: fields.optional_string_list("command")?,
                env: fields.optional_string_list("env")?,
                ports: fields.optional_string_list("ports")?,
                volumes: fields.optional_string_list("volumes")?,
                restart_policy: fields.optional_string("restart_policy")?,
                running: fields.optional_bool("running")?,
            },
            "absent" => PodmanParams::Absent {
                name: fields.required_string("name")?,
            },
            _ => unreachable!(),
        };
        fields.finish()?;
        Ok(out)
    }
}

impl Display for PodmanParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PodmanParams::Present { name, image, .. } => {
                write!(f, "Podman::Present(name = {name}, image = {image})")
            }
            PodmanParams::Absent { name } => write!(f, "Podman::Absent(name = {name})"),
        }
    }
}

impl_display_render!(PodmanParams);

#[derive(Debug, Clone)]
pub enum PodmanResource {
    Present {
        name: String,
        image: String,
        command: Option<Vec<String>>,
        env: Vec<String>,
        ports: Vec<String>,
        volumes: Vec<String>,
        restart_policy: Option<String>,
        running: bool,
    },
    Absent {
        name: String,
    },
}

impl Display for PodmanResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PodmanResource::Present {
                name,
                image,
                running,
                ..
            } => write!(
                f,
                "Podman::Present(name = {name}, image = {image}, running = {running})"
            ),
            PodmanResource::Absent { name } => write!(f, "Podman::Absent(name = {name})"),
        }
    }
}

impl_display_render!(PodmanResource);

#[derive(Debug, Clone)]
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
}

impl Display for PodmanState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PodmanState::Absent => write!(f, "Podman::Absent"),
            PodmanState::Present { image, running, .. } => {
                write!(f, "Podman::Present(image = {image}, running = {running})")
            }
        }
    }
}

impl_display_render!(PodmanState);

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

#[derive(Debug, Clone)]
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
        start: bool,
    },
    /// Container exists with the right config, but needs to be started.
    Start { name: String },
    /// Container exists with the right config, but needs to be stopped.
    Stop { name: String },
    /// Container exists but its config hash no longer matches; remove and recreate.
    Recreate {
        name: String,
        image: String,
        command: Option<Vec<String>>,
        env: Vec<String>,
        ports: Vec<String>,
        volumes: Vec<String>,
        restart_policy: Option<String>,
        start: bool,
    },
    /// Declared absent but the container exists; remove it.
    Remove { name: String },
}

impl Display for PodmanChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PodmanChange::Create { name, image, .. } => {
                write!(f, "Podman::Create(name = {name}, image = {image})")
            }
            PodmanChange::Start { name } => write!(f, "Podman::Start({name})"),
            PodmanChange::Stop { name } => write!(f, "Podman::Stop({name})"),
            PodmanChange::Recreate { name, image, .. } => {
                write!(f, "Podman::Recreate(name = {name}, image = {image})")
            }
            PodmanChange::Remove { name } => write!(f, "Podman::Remove({name})"),
        }
    }
}

impl_display_render!(PodmanChange);

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
                running,
            } => PodmanResource::Present {
                name,
                image,
                command,
                env: env.unwrap_or_default(),
                ports: ports.unwrap_or_default(),
                volumes: volumes.unwrap_or_default(),
                restart_policy,
                running: running.unwrap_or(true),
            },
            PodmanParams::Absent { name } => PodmanResource::Absent { name },
        };
        vec![CausalityTree::leaf(CausalityMeta::default(), resource)]
    }

    type State = PodmanState;
    type StateError = PodmanStateError;

    async fn state(
        _ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        let name = match resource {
            PodmanResource::Present { name, .. } | PodmanResource::Absent { name } => name,
        };

        // `podman container inspect` exits non-zero (125) when the container is
        // missing, which `outcome()` surfaces without raising. Distinguishing
        // "absent" from "podman itself failed" via stderr is unreliable across
        // versions, so we treat any non-success as Absent. A broken podman
        // install will then surface at apply-time on the first create.
        let outcome = Command::new("podman")
            .args(["container", "inspect", name])
            .outcome()
            .await?;
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

    type Change = PodmanChange;

    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        match (resource, state) {
            (PodmanResource::Absent { .. }, PodmanState::Absent) => None,

            (PodmanResource::Absent { name }, PodmanState::Present { .. }) => {
                Some(PodmanChange::Remove { name: name.clone() })
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
                    running,
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
                start: *running,
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
                    running,
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
                        start: *running,
                    })
                } else if *running != *current_running {
                    if *running {
                        Some(PodmanChange::Start { name: name.clone() })
                    } else {
                        Some(PodmanChange::Stop { name: name.clone() })
                    }
                } else {
                    None
                }
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
                start,
            } => create_ops(
                name,
                image,
                command,
                env,
                ports,
                volumes,
                restart_policy,
                start,
                None,
            ),
            PodmanChange::Start { name } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                Operation::Podman(PodmanOperation::Start { name }),
            )],
            PodmanChange::Stop { name } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                Operation::Podman(PodmanOperation::Stop { name }),
            )],
            PodmanChange::Recreate {
                name,
                image,
                command,
                env,
                ports,
                volumes,
                restart_policy,
                start,
            } => create_ops(
                name,
                image,
                command,
                env,
                ports,
                volumes,
                restart_policy,
                start,
                Some("remove"),
            ),
            PodmanChange::Remove { name } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                Operation::Podman(PodmanOperation::Remove { name }),
            )],
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
    start: bool,
    remove_id: Option<&'static str>,
) -> Vec<CausalityTree<Operation>> {
    let mut ops: Vec<CausalityTree<Operation>> = Vec::new();

    if let Some(id) = remove_id {
        ops.push(CausalityTree::leaf(
            CausalityMeta::id(id.into()),
            Operation::Podman(PodmanOperation::Remove { name: name.clone() }),
        ));
    }

    let hash = config_hash(
        &image,
        command.as_ref(),
        &env,
        &ports,
        &volumes,
        restart_policy.as_ref(),
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
            config_hash: hash,
        }),
    ));

    if start {
        ops.push(CausalityTree::leaf(
            CausalityMeta::requires(vec!["create".into()]),
            Operation::Podman(PodmanOperation::Start { name }),
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
    }

    let canonical_image = canonicalize_image(image);
    let cfg = ConfigForHash {
        image: &canonical_image,
        command,
        env,
        ports,
        volumes,
        restart_policy,
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
            running: spec.running,
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
            PodmanState::Absent => unreachable!(),
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
            PodmanState::Absent => unreachable!(),
        };
        let change = Podman::change(&resource(declared), &current).expect("change");
        assert!(matches!(change, PodmanChange::Stop { .. }));
    }

    #[test]
    fn change_remove_when_declared_absent_but_present() {
        let declared = PodmanResource::Absent { name: "web".into() };
        let current = state_matching(&ResourceSpec::default());
        let change = Podman::change(&declared, &current).expect("change");
        assert!(matches!(change, PodmanChange::Remove { .. }));
    }

    #[test]
    fn change_none_when_absent_matches() {
        let declared = PodmanResource::Absent { name: "web".into() };
        assert!(Podman::change(&declared, &PodmanState::Absent).is_none());
    }

    #[test]
    fn config_hash_is_stable_for_equivalent_image_refs() {
        let a = config_hash("nginx", None, &[], &[], &[], None);
        let b = config_hash("docker.io/library/nginx:latest", None, &[], &[], &[], None);
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
            ),
            config_hash(
                "nginx",
                Some(&vec!["bash".into()]),
                &["A=1".into()],
                &["80:80".into()],
                &["/x:/x".into()],
                Some(&"always".into()),
            ),
            config_hash(
                "nginx",
                Some(&vec!["sh".into()]),
                &["A=2".into()],
                &["80:80".into()],
                &["/x:/x".into()],
                Some(&"always".into()),
            ),
            config_hash(
                "nginx",
                Some(&vec!["sh".into()]),
                &["A=1".into()],
                &["81:80".into()],
                &["/x:/x".into()],
                Some(&"always".into()),
            ),
            config_hash(
                "nginx",
                Some(&vec!["sh".into()]),
                &["A=1".into()],
                &["80:80".into()],
                &["/y:/y".into()],
                Some(&"always".into()),
            ),
            config_hash(
                "nginx",
                Some(&vec!["sh".into()]),
                &["A=1".into()],
                &["80:80".into()],
                &["/x:/x".into()],
                Some(&"unless-stopped".into()),
            ),
            config_hash(
                "nginx",
                None,
                &["A=1".into()],
                &["80:80".into()],
                &["/x:/x".into()],
                Some(&"always".into()),
            ),
            config_hash(
                "nginx",
                Some(&vec!["sh".into()]),
                &["A=1".into()],
                &["80:80".into()],
                &["/x:/x".into()],
                None,
            ),
        ];
        for v in &variants {
            assert_ne!(*v, base, "variant collided with base: {v}");
        }
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
}
