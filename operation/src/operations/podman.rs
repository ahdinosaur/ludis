use async_trait::async_trait;
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use serde::{Deserialize, Serialize};
use std::{fmt::Display, pin::Pin};
use thiserror::Error;
use tokio::process::{ChildStderr, ChildStdout};
use tracing::{debug, info};

use crate::OperationType;
use crate::operations::file::FilePath;

/// Label key written on every container lusid creates. Its value is the
/// resource layer's `config_hash` of the declared spec, used by drift
/// detection on the next plan to tell "still matches" from "needs recreate".
/// Kept in this crate so the create command and the resource-side reader
/// can't disagree - change the key in one place.
pub const CONFIG_HASH_LABEL: &str = "lusid.config-hash";

/// Label key written on the lusid-owned marker network for a compose project.
/// Its presence (matching [`COMPOSE_CONFIG_HASH_LABEL`]) tells the next probe
/// that the project was brought up by lusid; its value identifies the project
/// for label-based teardown enumeration would not need it (we use the
/// `com.docker.compose.project` label written by podman-compose for that) but
/// keeping our own label scoped to a distinct namespace prevents accidental
/// teardown sweeps from chewing on the marker. See the resource-side probe
/// for the consumer.
pub const COMPOSE_PROJECT_LABEL: &str = "lusid.compose_project";

/// Label key carrying the SHA-256 hex of the compose project's declared spec.
/// Written on the marker network at [`PodmanOperation::ComposeMarkerInstall`]
/// time; read back at state-probe time to detect drift.
pub const COMPOSE_CONFIG_HASH_LABEL: &str = "lusid.compose_config_hash";

/// Name of the lusid-owned marker network for a compose project. The
/// `lusid-compose-marker-` prefix is reserved: operators are documented not
/// to declare compose networks under this name. Containers and networks
/// created by `podman-compose` itself use a different naming scheme
/// (`<project>_<network>` / `<project>_default`).
pub fn compose_marker_network_name(project: &str) -> String {
    format!("lusid-compose-marker-{project}")
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PodmanOperation {
    /// Create a container from `image` under `name`. `--pull=missing` is used
    /// so the image is fetched inline when it isn't already present locally -
    /// keeps the operation set small without exposing a separate Pull op.
    /// `config_hash` is written as the [`CONFIG_HASH_LABEL`] label so the
    /// next state observation can detect drift without re-deriving fields
    /// from podman's normalised inspect output.
    Create {
        name: String,
        image: String,
        command: Option<Vec<String>>,
        env: Vec<String>,
        ports: Vec<String>,
        volumes: Vec<String>,
        restart_policy: Option<String>,
        /// Passed through to `podman create --network <value>`. `None` omits
        /// the flag entirely. See the resource-side field doc for accepted
        /// values and the host-network interaction with `ports`.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        network: Option<String>,
        config_hash: String,
        /// When set, the `podman create` shell-out runs under `sudo -n` so
        /// the container lives in the root podman runtime (rootful podman).
        /// Required when the container needs to bind-mount root-owned host
        /// paths or bind privileged ports. See module docs on
        /// [`crate::OperationType`] for the shared rationale.
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    Start {
        name: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    Stop {
        name: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    /// Remove a container. Uses `--force` so a running container is stopped
    /// first; this matches the declarative "make this not exist" intent.
    Remove {
        name: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },

    /// Bring a compose project up via `podman-compose -p <project> -f f1 ... up -d`.
    /// Does *not* install the marker network - that's a separate, causality-
    /// chained [`PodmanOperation::ComposeMarkerInstall`] so a half-up
    /// failure does not leave a marker pointing at a stale project.
    ComposeUp {
        project: String,
        files: Vec<FilePath>,
        working_dir: FilePath,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        env_file: Option<FilePath>,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },

    /// Tear down a compose project. Uses the `com.docker.compose.project`
    /// label that `podman-compose` itself writes - we do not require the
    /// compose files to be present at teardown. Named volumes are preserved
    /// (matches `podman-compose down` default; avoids data loss). Runs as a
    /// single `sh -c` script so the apply-tuple's single-child-stream
    /// contract is preserved.
    ComposeDown {
        project: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },

    /// Refresh images via `podman-compose -p <project> -f f1 ... pull`.
    /// Author-facing only: emitted by `@operation/podman action: "compose_pull"`,
    /// never lowered from a resource's change. Pair with a follow-up
    /// `compose_up` to roll services onto the new images.
    ComposePull {
        project: String,
        files: Vec<FilePath>,
        working_dir: FilePath,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        env_file: Option<FilePath>,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },

    /// Install the lusid marker network for a compose project, labelled with
    /// the project's declared `config_hash`. Uses `--replace` so retries are
    /// idempotent. Causality-wired to run after [`PodmanOperation::ComposeUp`]
    /// so a half-up failure leaves no marker.
    ComposeMarkerInstall {
        project: String,
        config_hash: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },

    /// Remove the lusid marker network for a compose project. Causality-wired
    /// to run *before* [`PodmanOperation::ComposeDown`] so a half-failed down
    /// does not leave a marker pointing at a stale project. Uses `--ignore`
    /// so a missing marker is not an error.
    ComposeMarkerUninstall {
        project: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
}

impl Display for PodmanOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = |sudo: bool| if sudo { "[sudo] " } else { "" };
        match self {
            PodmanOperation::Create {
                name, image, sudo, ..
            } => write!(
                f,
                "{}Podman::Create(name = {name}, image = {image})",
                prefix(*sudo)
            ),
            PodmanOperation::Start { name, sudo } => {
                write!(f, "{}Podman::Start({name})", prefix(*sudo))
            }
            PodmanOperation::Stop { name, sudo } => {
                write!(f, "{}Podman::Stop({name})", prefix(*sudo))
            }
            PodmanOperation::Remove { name, sudo } => {
                write!(f, "{}Podman::Remove({name})", prefix(*sudo))
            }
            PodmanOperation::ComposeUp { project, sudo, .. } => {
                write!(f, "{}Podman::ComposeUp(project = {project})", prefix(*sudo))
            }
            PodmanOperation::ComposeDown { project, sudo } => {
                write!(
                    f,
                    "{}Podman::ComposeDown(project = {project})",
                    prefix(*sudo)
                )
            }
            PodmanOperation::ComposePull { project, sudo, .. } => {
                write!(
                    f,
                    "{}Podman::ComposePull(project = {project})",
                    prefix(*sudo)
                )
            }
            PodmanOperation::ComposeMarkerInstall { project, sudo, .. } => write!(
                f,
                "{}Podman::ComposeMarkerInstall(project = {project})",
                prefix(*sudo)
            ),
            PodmanOperation::ComposeMarkerUninstall { project, sudo } => write!(
                f,
                "{}Podman::ComposeMarkerUninstall(project = {project})",
                prefix(*sudo)
            ),
        }
    }
}

#[derive(Error, Debug)]
pub enum PodmanApplyError {
    #[error(transparent)]
    Command(#[from] CommandError),
}

#[derive(Debug, Clone)]
pub struct Podman;

#[async_trait]
impl OperationType for Podman {
    type Operation = PodmanOperation;

    // Note(cc): merge is a no-op. Each op targets a single named container and
    // ordering matters (create before start, remove before recreate) - that
    // ordering is already expressed in the causality tree, so merging would
    // have to respect it. Not worth the complexity for the typical "handful of
    // containers per plan" case.
    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation> {
        operations
    }

    type ApplyOutput = Pin<Box<dyn Future<Output = Result<(), Self::ApplyError>> + Send + 'static>>;
    type ApplyError = PodmanApplyError;
    type ApplyStdout = ChildStdout;
    type ApplyStderr = ChildStderr;

    async fn apply(
        _ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError> {
        match operation {
            PodmanOperation::Create {
                name,
                image,
                command,
                env,
                ports,
                volumes,
                restart_policy,
                network,
                config_hash,
                sudo,
            } => {
                info!(sudo, "[podman] create: {} from {}", name, image);
                let mut cmd = Command::new("podman");
                cmd.arg("create")
                    .arg("--pull=missing")
                    .arg("--name")
                    .arg(name)
                    .arg("--label")
                    .arg(format!("{CONFIG_HASH_LABEL}={config_hash}"));
                if let Some(policy) = restart_policy {
                    cmd.arg("--restart").arg(policy);
                }
                if let Some(value) = network {
                    cmd.arg("--network").arg(value);
                }
                for value in env {
                    cmd.arg("-e").arg(value);
                }
                for mapping in ports {
                    cmd.arg("-p").arg(mapping);
                }
                for mapping in volumes {
                    cmd.arg("-v").arg(mapping);
                }
                cmd.arg("--").arg(image);
                if let Some(command) = command {
                    cmd.args(command);
                }
                let mut cmd = if *sudo { cmd.sudo() } else { cmd };
                let output = cmd.output().await?;
                Ok((
                    Box::pin(async move {
                        output.status.await?;
                        Ok(())
                    }),
                    output.stdout,
                    output.stderr,
                ))
            }
            PodmanOperation::Start { name, sudo } => {
                info!(sudo, "[podman] start: {}", name);
                let mut cmd = Command::new("podman");
                cmd.arg("start").arg("--").arg(name);
                let mut cmd = if *sudo { cmd.sudo() } else { cmd };
                let output = cmd.output().await?;
                Ok((
                    Box::pin(async move {
                        output.status.await?;
                        Ok(())
                    }),
                    output.stdout,
                    output.stderr,
                ))
            }
            PodmanOperation::Stop { name, sudo } => {
                info!(sudo, "[podman] stop: {}", name);
                let mut cmd = Command::new("podman");
                cmd.arg("stop").arg("--").arg(name);
                let mut cmd = if *sudo { cmd.sudo() } else { cmd };
                let output = cmd.output().await?;
                Ok((
                    Box::pin(async move {
                        output.status.await?;
                        Ok(())
                    }),
                    output.stdout,
                    output.stderr,
                ))
            }
            PodmanOperation::Remove { name, sudo } => {
                info!(sudo, "[podman] remove: {}", name);
                let mut cmd = Command::new("podman");
                cmd.arg("rm").arg("--force").arg("--").arg(name);
                let mut cmd = if *sudo { cmd.sudo() } else { cmd };
                let output = cmd.output().await?;
                Ok((
                    Box::pin(async move {
                        output.status.await?;
                        Ok(())
                    }),
                    output.stdout,
                    output.stderr,
                ))
            }
            PodmanOperation::ComposeUp {
                project,
                files,
                working_dir,
                env_file,
                sudo,
            } => {
                info!(
                    sudo,
                    project = %project,
                    files = ?files.iter().map(|p| p.as_path()).collect::<Vec<_>>(),
                    "[podman] compose up"
                );
                let cmd = build_compose_up(project, files, working_dir, env_file.as_ref());
                run_compose_op(if *sudo { cmd.sudo() } else { cmd }).await
            }
            PodmanOperation::ComposeDown { project, sudo } => {
                // Note(cc): we use the `com.docker.compose.project` label
                // (written by podman-compose itself) for teardown enumeration,
                // intentionally distinct from `lusid.compose_project` on the
                // marker network. The marker is touched only by the separate
                // `ComposeMarkerUninstall` operation; if the two label
                // namespaces are ever unified, this sweep starts ripping the
                // marker out and breaks the causality ordering.
                info!(sudo, project = %project, "[podman] compose down");
                let cmd = build_compose_down(project);
                run_compose_op(if *sudo { cmd.sudo() } else { cmd }).await
            }
            PodmanOperation::ComposePull {
                project,
                files,
                working_dir,
                env_file,
                sudo,
            } => {
                info!(
                    sudo,
                    project = %project,
                    "[podman] compose pull"
                );
                let cmd = build_compose_pull(project, files, working_dir, env_file.as_ref());
                run_compose_op(if *sudo { cmd.sudo() } else { cmd }).await
            }
            PodmanOperation::ComposeMarkerInstall {
                project,
                config_hash,
                sudo,
            } => {
                info!(
                    sudo,
                    project = %project,
                    hash = %config_hash,
                    "[podman] compose marker install"
                );
                let cmd = build_compose_marker_install(project, config_hash);
                run_compose_op(if *sudo { cmd.sudo() } else { cmd }).await
            }
            PodmanOperation::ComposeMarkerUninstall { project, sudo } => {
                info!(
                    sudo,
                    project = %project,
                    "[podman] compose marker uninstall"
                );
                let cmd = build_compose_marker_uninstall(project);
                run_compose_op(if *sudo { cmd.sudo() } else { cmd }).await
            }
        }
    }
}

/// Run a podman-compose-family command to completion and surface a non-zero
/// exit as `CommandError::Failure`. Unlike the existing single-container
/// arms (which preserve a pre-existing behaviour of swallowing non-zero
/// exits), the compose arms must propagate failure so causality-chained
/// follow-ups (e.g. marker install after compose up) do not run on a half-
/// completed parent.
async fn run_compose_op(
    mut cmd: Command,
) -> Result<
    (
        <Podman as OperationType>::ApplyOutput,
        <Podman as OperationType>::ApplyStdout,
        <Podman as OperationType>::ApplyStderr,
    ),
    PodmanApplyError,
> {
    let cmd_display = cmd.to_string();
    let output = cmd.output().await?;
    let future: Pin<Box<dyn Future<Output = Result<(), PodmanApplyError>> + Send + 'static>> =
        Box::pin(async move {
            let status = output.status.await?;
            if !status.success() {
                return Err(PodmanApplyError::Command(CommandError::Failure {
                    command: cmd_display,
                    stderr: format!("exit status {status}"),
                }));
            }
            Ok(())
        });
    Ok((future, output.stdout, output.stderr))
}

/// Build the `podman-compose ... up -d` command. Pure: no I/O, no sudo wrap.
/// `working_dir` is the directory that compose treats as the project root for
/// relative-path resolution inside the compose files. `env_file` if `Some`
/// passes `--env-file <path>` for `${VAR}` interpolation in the YAML.
fn build_compose_up(
    project: &str,
    files: &[FilePath],
    working_dir: &FilePath,
    env_file: Option<&FilePath>,
) -> Command {
    let mut cmd = Command::new("podman-compose");
    cmd.current_dir(working_dir.as_path());
    cmd.arg("-p").arg(project);
    for file in files {
        cmd.arg("-f").arg(file.as_path());
    }
    if let Some(env_file) = env_file {
        cmd.arg("--env-file").arg(env_file.as_path());
    }
    cmd.arg("up").arg("-d");
    cmd
}

/// Build the compose-down command: a single `sh -c` script that enumerates
/// containers and networks bearing `com.docker.compose.project=<project>`
/// and removes them. `project` is pre-validated upstream against
/// `^[a-z0-9][a-z0-9_-]{0,62}$` so direct interpolation cannot inject shell
/// metacharacters; this is what allows the single-child-stream contract to
/// be satisfied without `shell-words` quoting acrobatics.
///
/// Named volumes are deliberately preserved (matches `podman-compose down`
/// default; wiping persistent data on a typo would be career-ending).
fn build_compose_down(project: &str) -> Command {
    debug_assert!(is_valid_project_name(project), "project not pre-validated");
    let script = format!(
        "set -e\n\
         ids=$(podman ps -a --format '{{{{.ID}}}}' --filter label=com.docker.compose.project={project})\n\
         [ -n \"$ids\" ] && podman rm --force $ids\n\
         networks=$(podman network ls --format '{{{{.Name}}}}' --filter label=com.docker.compose.project={project})\n\
         [ -n \"$networks\" ] && podman network rm $networks\n\
         true\n"
    );
    debug!(project = %project, script = %script, "[podman] compose down script");
    Command::new_sh(&script)
}

/// Build the `podman-compose ... pull` command.
fn build_compose_pull(
    project: &str,
    files: &[FilePath],
    working_dir: &FilePath,
    env_file: Option<&FilePath>,
) -> Command {
    let mut cmd = Command::new("podman-compose");
    cmd.current_dir(working_dir.as_path());
    cmd.arg("-p").arg(project);
    for file in files {
        cmd.arg("-f").arg(file.as_path());
    }
    if let Some(env_file) = env_file {
        cmd.arg("--env-file").arg(env_file.as_path());
    }
    cmd.arg("pull");
    cmd
}

/// Build the marker-install command: `podman network create --replace --label
/// lusid.compose_project=<project> --label lusid.compose_config_hash=<hex>
/// lusid-compose-marker-<project>`.
fn build_compose_marker_install(project: &str, config_hash: &str) -> Command {
    let mut cmd = Command::new("podman");
    cmd.arg("network").arg("create").arg("--replace");
    cmd.arg("--label")
        .arg(format!("{COMPOSE_PROJECT_LABEL}={project}"));
    cmd.arg("--label")
        .arg(format!("{COMPOSE_CONFIG_HASH_LABEL}={config_hash}"));
    cmd.arg("--").arg(compose_marker_network_name(project));
    cmd
}

/// Build the marker-uninstall command: `podman network rm --force --ignore
/// lusid-compose-marker-<project>`. `--ignore` makes a missing marker a no-op,
/// which is what we want during a normal teardown (the marker might not exist
/// if a previous up failed half-way).
fn build_compose_marker_uninstall(project: &str) -> Command {
    let mut cmd = Command::new("podman");
    cmd.arg("network").arg("rm").arg("--force").arg("--ignore");
    cmd.arg("--").arg(compose_marker_network_name(project));
    cmd
}

/// Mirror of the project-name regex enforced at parse-time. Kept here as a
/// debug-only assertion guard for [`build_compose_down`] (where the value is
/// interpolated into a shell script) so a future bypass at the parser layer
/// trips loudly in tests instead of silently shipping a shell-injection
/// hole.
fn is_valid_project_name(project: &str) -> bool {
    let bytes = project.as_bytes();
    if bytes.is_empty() || bytes.len() > 63 {
        return false;
    }
    let first_ok = bytes[0].is_ascii_lowercase() || bytes[0].is_ascii_digit();
    if !first_ok {
        return false;
    }
    bytes[1..]
        .iter()
        .all(|b| b.is_ascii_lowercase() || b.is_ascii_digit() || *b == b'_' || *b == b'-')
}

#[cfg(test)]
mod compose_builder_tests {
    use super::*;

    #[test]
    fn compose_up_includes_project_and_files() {
        let cmd = build_compose_up(
            "my_app",
            &[
                FilePath::new("/etc/compose/a.yaml"),
                FilePath::new("/etc/compose/b.yaml"),
            ],
            &FilePath::new("/etc/compose"),
            None,
        );
        let s = cmd.to_string();
        assert_eq!(
            s,
            "podman-compose -p my_app -f /etc/compose/a.yaml -f /etc/compose/b.yaml up -d"
        );
    }

    #[test]
    fn compose_up_passes_env_file_when_set() {
        let cmd = build_compose_up(
            "app",
            &[FilePath::new("/c/compose.yaml")],
            &FilePath::new("/c"),
            Some(&FilePath::new("/c/.env")),
        );
        let s = cmd.to_string();
        assert!(
            s.contains("--env-file /c/.env"),
            "env-file should appear: {s}"
        );
    }

    #[test]
    fn compose_up_omits_env_file_when_unset() {
        let cmd = build_compose_up(
            "app",
            &[FilePath::new("/c/compose.yaml")],
            &FilePath::new("/c"),
            None,
        );
        let s = cmd.to_string();
        assert!(!s.contains("--env-file"), "env-file should not appear: {s}");
    }

    #[test]
    fn compose_up_under_sudo_prefixes_with_sudo() {
        let cmd = build_compose_up(
            "app",
            &[FilePath::new("/c/compose.yaml")],
            &FilePath::new("/c"),
            None,
        )
        .sudo();
        let s = cmd.to_string();
        assert!(s.starts_with("sudo -n podman-compose "), "got: {s}");
    }

    #[test]
    fn compose_down_filters_on_compose_project_label_and_id_format() {
        let cmd = build_compose_down("my_app");
        let s = cmd.to_string();
        // Single sh -c invocation. Both stages reference the same project
        // label via the format the resource probe also uses.
        assert!(s.starts_with("sh -c "), "expected sh -c: {s}");
        assert!(
            s.contains("com.docker.compose.project=my_app"),
            "label filter missing: {s}"
        );
        assert!(s.contains("{{.ID}}"), "ID format missing: {s}");
        assert!(s.contains("{{.Name}}"), "Name format missing: {s}");
    }

    #[test]
    fn compose_pull_uses_pull_action() {
        let cmd = build_compose_pull(
            "app",
            &[FilePath::new("/c/compose.yaml")],
            &FilePath::new("/c"),
            None,
        );
        let s = cmd.to_string();
        assert_eq!(s, "podman-compose -p app -f /c/compose.yaml pull");
    }

    #[test]
    fn compose_marker_install_carries_both_labels() {
        let cmd = build_compose_marker_install("app", "abc123");
        let s = cmd.to_string();
        assert!(s.contains("--replace"), "got: {s}");
        assert!(s.contains("lusid.compose_project=app"), "got: {s}");
        assert!(s.contains("lusid.compose_config_hash=abc123"), "got: {s}");
        assert!(s.ends_with(" -- lusid-compose-marker-app"), "got: {s}");
    }

    #[test]
    fn compose_marker_uninstall_uses_ignore() {
        let cmd = build_compose_marker_uninstall("app");
        let s = cmd.to_string();
        assert!(s.contains("--force"), "got: {s}");
        assert!(s.contains("--ignore"), "got: {s}");
        assert!(s.ends_with(" -- lusid-compose-marker-app"), "got: {s}");
    }

    #[test]
    fn compose_marker_network_name_format_is_stable() {
        // Wire-format-ish: changing this name invalidates every existing
        // marker network. Catching the change here means the test author
        // is reminded to consider the upgrade path.
        assert_eq!(
            compose_marker_network_name("foo"),
            "lusid-compose-marker-foo"
        );
    }

    #[test]
    fn is_valid_project_name_accepts_lower_digit_hyphen_underscore() {
        assert!(is_valid_project_name("a"));
        assert!(is_valid_project_name("a-b"));
        assert!(is_valid_project_name("a_b"));
        assert!(is_valid_project_name("a1b2"));
        assert!(is_valid_project_name("0xabc"));
    }

    #[test]
    fn is_valid_project_name_rejects_uppercase_or_leading_hyphen() {
        assert!(!is_valid_project_name(""));
        assert!(!is_valid_project_name("A"));
        assert!(!is_valid_project_name("-abc"));
        assert!(!is_valid_project_name("_abc"));
        assert!(!is_valid_project_name("a;b"));
        assert!(!is_valid_project_name("a b"));
    }

    #[test]
    fn compose_op_serdes_omit_sudo_when_false() {
        let op = PodmanOperation::ComposeDown {
            project: "app".into(),
            sudo: false,
        };
        let json = serde_json::to_string(&op).unwrap();
        assert!(!json.contains("sudo"), "sudo:false should omit: {json}");
    }

    #[test]
    fn compose_op_serdes_include_sudo_when_true() {
        let op = PodmanOperation::ComposeDown {
            project: "app".into(),
            sudo: true,
        };
        let json = serde_json::to_string(&op).unwrap();
        assert!(json.contains(r#""sudo":true"#), "got: {json}");
    }
}
