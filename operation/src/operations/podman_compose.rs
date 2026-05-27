use async_trait::async_trait;
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_params::{ParseError, ParseParams, StructFields};
use rimu::{Span, Spanned, Value};
use serde::{Deserialize, Serialize};
use std::{fmt::Display, pin::Pin};
use thiserror::Error;
use tokio::process::{ChildStderr, ChildStdout};
use tracing::info;

use crate::OperationType;
use crate::operations::file::FilePath;

/// Label key written on the lusid-owned marker network for a compose project.
/// Its presence identifies the project as lusid-managed; the companion
/// [`COMPOSE_CONFIG_HASH_LABEL`] carries the hash. Distinct from the
/// `com.docker.compose.project` label that `podman-compose` itself writes
/// (which we use for teardown enumeration) so the two label namespaces do
/// not get accidentally co-swept.
pub const COMPOSE_PROJECT_LABEL: &str = "lusid.compose_project";

/// Label key carrying the SHA-256 hex of the compose project's declared spec.
/// Written on the marker network at [`PodmanComposeOperation::MarkerInstall`]
/// time; read back at state-probe time to detect drift.
pub const COMPOSE_CONFIG_HASH_LABEL: &str = "lusid.compose_config_hash";

/// Name of the lusid-owned marker network for a compose project. The
/// `lusid-compose-marker-` prefix is reserved: operators are documented not
/// to declare compose networks under this name. Containers and networks
/// created by `podman-compose` itself use a different naming scheme
/// (`<project>_<network>` / `<project>_default`).
pub fn marker_network_name(project: &str) -> String {
    format!("lusid-compose-marker-{project}")
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PodmanComposeOperation {
    /// Bring a compose project up via `podman-compose -p <project> -f f1 ... up -d`.
    /// Does *not* install the marker network - that's a separate, causality-
    /// chained [`PodmanComposeOperation::MarkerInstall`] so a half-up
    /// failure does not leave a marker pointing at a stale project.
    Up {
        project: String,
        files: Vec<FilePath>,
        working_dir: FilePath,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        env_file: Option<FilePath>,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },

    /// Tear down a compose project. Uses the `com.docker.compose.project`
    /// label that `podman-compose` itself writes - the compose files are
    /// not required at teardown. Named volumes are preserved (matches
    /// `podman-compose down` default; avoids data loss). Runs as a single
    /// `sh -c` script so the apply-tuple's single-child-stream contract is
    /// preserved.
    Down {
        project: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },

    /// Refresh images via `podman-compose -p <project> -f f1 ... pull`.
    /// Author-facing only: emitted by `@operation/podman-compose action: "pull"`,
    /// never lowered from a resource's change. Pair with a follow-up `up`
    /// to roll services onto the new images.
    Pull {
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
    /// idempotent. Causality-wired to run after [`PodmanComposeOperation::Up`]
    /// so a half-up failure leaves no marker.
    MarkerInstall {
        project: String,
        config_hash: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },

    /// Remove the lusid marker network for a compose project. Causality-wired
    /// to run *before* [`PodmanComposeOperation::Down`] so a half-failed down
    /// does not leave a marker pointing at a stale project. Uses `--ignore`
    /// so a missing marker is not an error.
    MarkerUninstall {
        project: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
}

impl Display for PodmanComposeOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = |sudo: bool| if sudo { "[sudo] " } else { "" };
        match self {
            PodmanComposeOperation::Up { project, sudo, .. } => {
                write!(f, "{}PodmanCompose::Up(project = {project})", prefix(*sudo))
            }
            PodmanComposeOperation::Down { project, sudo } => write!(
                f,
                "{}PodmanCompose::Down(project = {project})",
                prefix(*sudo)
            ),
            PodmanComposeOperation::Pull { project, sudo, .. } => write!(
                f,
                "{}PodmanCompose::Pull(project = {project})",
                prefix(*sudo)
            ),
            PodmanComposeOperation::MarkerInstall { project, sudo, .. } => write!(
                f,
                "{}PodmanCompose::MarkerInstall(project = {project})",
                prefix(*sudo)
            ),
            PodmanComposeOperation::MarkerUninstall { project, sudo } => write!(
                f,
                "{}PodmanCompose::MarkerUninstall(project = {project})",
                prefix(*sudo)
            ),
        }
    }
}

#[derive(Error, Debug)]
pub enum PodmanComposeApplyError {
    #[error(transparent)]
    Command(#[from] CommandError),
}

#[derive(Debug, Clone)]
pub struct PodmanCompose;

#[async_trait]
impl OperationType for PodmanCompose {
    type Operation = PodmanComposeOperation;

    // Note(cc): merge is a no-op. Each op targets a single named project and
    // intra-project ordering matters (down before up, marker uninstall before
    // down, marker install after up) - the causality tree already expresses
    // that. Coalescing duplicate projects would mask a plan-level bug; let
    // it surface.
    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation> {
        operations
    }

    type ApplyOutput = Pin<Box<dyn Future<Output = Result<(), Self::ApplyError>> + Send + 'static>>;
    type ApplyError = PodmanComposeApplyError;
    type ApplyStdout = ChildStdout;
    type ApplyStderr = ChildStderr;

    async fn apply(
        _ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError> {
        match operation {
            PodmanComposeOperation::Up {
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
                    "[podman-compose] up"
                );
                let cmd = build_up(project, files, working_dir, env_file.as_ref());
                run(if *sudo { cmd.sudo() } else { cmd }).await
            }
            PodmanComposeOperation::Down { project, sudo } => {
                // Note(cc): teardown filter is `com.docker.compose.project`
                // (written by podman-compose), intentionally distinct from
                // `lusid.compose_project` on the marker network. The marker
                // is touched only by `MarkerUninstall`; if the two label
                // namespaces are ever unified, this sweep starts ripping
                // the marker out and breaks the causality ordering.
                info!(sudo, project = %project, "[podman-compose] down");
                let cmd = build_down(project);
                run(if *sudo { cmd.sudo() } else { cmd }).await
            }
            PodmanComposeOperation::Pull {
                project,
                files,
                working_dir,
                env_file,
                sudo,
            } => {
                info!(sudo, project = %project, "[podman-compose] pull");
                let cmd = build_pull(project, files, working_dir, env_file.as_ref());
                run(if *sudo { cmd.sudo() } else { cmd }).await
            }
            PodmanComposeOperation::MarkerInstall {
                project,
                config_hash,
                sudo,
            } => {
                info!(
                    sudo,
                    project = %project,
                    hash = %config_hash,
                    "[podman-compose] marker install"
                );
                let cmd = build_marker_install(project, config_hash);
                run(if *sudo { cmd.sudo() } else { cmd }).await
            }
            PodmanComposeOperation::MarkerUninstall { project, sudo } => {
                info!(
                    sudo,
                    project = %project,
                    "[podman-compose] marker uninstall"
                );
                let cmd = build_marker_uninstall(project);
                run(if *sudo { cmd.sudo() } else { cmd }).await
            }
        }
    }
}

/// Run a podman / podman-compose command to completion and surface a
/// non-zero exit as `CommandError::Failure`. Required for the compose
/// causality chain: if `up` returns Ok despite a failed exit code, the
/// subsequent `MarkerInstall` runs against a half-up project.
async fn run(
    mut cmd: Command,
) -> Result<
    (
        <PodmanCompose as OperationType>::ApplyOutput,
        <PodmanCompose as OperationType>::ApplyStdout,
        <PodmanCompose as OperationType>::ApplyStderr,
    ),
    PodmanComposeApplyError,
> {
    let cmd_display = cmd.to_string();
    let output = cmd.output().await?;
    let future: Pin<
        Box<dyn Future<Output = Result<(), PodmanComposeApplyError>> + Send + 'static>,
    > = Box::pin(async move {
        let status = output.status.await?;
        if !status.success() {
            return Err(PodmanComposeApplyError::Command(CommandError::Failure {
                command: cmd_display,
                stderr: format!("exit status {status}"),
            }));
        }
        Ok(())
    });
    Ok((future, output.stdout, output.stderr))
}

/// Build the `podman-compose ... up -d` command. Pure: no I/O, no sudo wrap.
fn build_up(
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
fn build_down(project: &str) -> Command {
    debug_assert!(is_valid_project_name(project), "project not pre-validated");
    let script = format!(
        "set -e\n\
         ids=$(podman ps -a --format '{{{{.ID}}}}' --filter label=com.docker.compose.project={project})\n\
         [ -n \"$ids\" ] && podman rm --force $ids\n\
         networks=$(podman network ls --format '{{{{.Name}}}}' --filter label=com.docker.compose.project={project})\n\
         [ -n \"$networks\" ] && podman network rm $networks\n\
         true\n"
    );
    Command::new_sh(&script)
}

/// Build the `podman-compose ... pull` command.
fn build_pull(
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

/// Build the marker-install command: `podman network create --replace
/// --label ... lusid-compose-marker-<project>`.
fn build_marker_install(project: &str, config_hash: &str) -> Command {
    let mut cmd = Command::new("podman");
    cmd.arg("network").arg("create").arg("--replace");
    cmd.arg("--label")
        .arg(format!("{COMPOSE_PROJECT_LABEL}={project}"));
    cmd.arg("--label")
        .arg(format!("{COMPOSE_CONFIG_HASH_LABEL}={config_hash}"));
    cmd.arg("--").arg(marker_network_name(project));
    cmd
}

/// Build the marker-uninstall command: `podman network rm --force --ignore
/// lusid-compose-marker-<project>`. `--ignore` makes a missing marker a
/// no-op, which is what we want during a normal teardown.
fn build_marker_uninstall(project: &str) -> Command {
    let mut cmd = Command::new("podman");
    cmd.arg("network").arg("rm").arg("--force").arg("--ignore");
    cmd.arg("--").arg(marker_network_name(project));
    cmd
}

/// Mirror of the project-name regex enforced at parse-time. Kept here as a
/// debug-only assertion guard for [`build_down`] (where the value is
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

impl ParseParams for PodmanComposeOperation {
    /// Parse an `@operation/podman-compose` author-facing params object.
    /// Discriminator: `action: "up" | "down" | "pull"`. The marker variants
    /// are internal-only - they're emitted by the resource layer to keep
    /// the marker network's lifecycle bound to the resource's hash, and
    /// exposing them author-side would let plan authors leave half-installed
    /// markers behind.
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let action = fields.take_discriminator("action", &["up", "down", "pull"])?;
        let out = match action {
            "up" => parse_with_files(
                &mut fields,
                |project, files, working_dir, env_file, sudo| PodmanComposeOperation::Up {
                    project,
                    files,
                    working_dir,
                    env_file,
                    sudo,
                },
            )?,
            "down" => {
                let (project, project_span) = fields.required_string_spanned("project")?.take();
                check_project_name(&project, &project_span)?;
                PodmanComposeOperation::Down {
                    project,
                    sudo: fields.optional_bool("sudo")?.unwrap_or(false),
                }
            }
            "pull" => parse_with_files(
                &mut fields,
                |project, files, working_dir, env_file, sudo| PodmanComposeOperation::Pull {
                    project,
                    files,
                    working_dir,
                    env_file,
                    sudo,
                },
            )?,
            _ => unreachable!(),
        };
        fields.finish()?;
        Ok(out)
    }
}

/// Shared parsing for `up` and `pull`: project + files (≥1) + optional
/// working_dir + optional env_file + optional sudo. The closure picks
/// which variant to build.
fn parse_with_files<F>(
    fields: &mut StructFields,
    build: F,
) -> Result<PodmanComposeOperation, Spanned<ParseError>>
where
    F: FnOnce(String, Vec<FilePath>, FilePath, Option<FilePath>, bool) -> PodmanComposeOperation,
{
    let (project, project_span) = fields.required_string_spanned("project")?.take();
    check_project_name(&project, &project_span)?;
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
    let files: Vec<FilePath> = files_spanned
        .into_iter()
        .map(|spanned| FilePath::new(spanned.into_inner().to_string_lossy().into_owned()))
        .collect();
    let working_dir = match fields.optional_host_path_spanned("working_dir")? {
        Some(s) => FilePath::new(s.into_inner().to_string_lossy().into_owned()),
        None => {
            let first = files[0].as_path();
            let parent = first.parent().unwrap_or_else(|| std::path::Path::new("."));
            FilePath::new(parent.to_string_lossy().into_owned())
        }
    };
    let env_file = fields
        .optional_host_path_spanned("env_file")?
        .map(|s| FilePath::new(s.into_inner().to_string_lossy().into_owned()));
    let sudo = fields.optional_bool("sudo")?.unwrap_or(false);
    Ok(build(project, files, working_dir, env_file, sudo))
}

/// Enforce the project-name regex on author-facing input. Refuses a
/// malicious value before it can reach [`build_down`]'s shell interpolation.
/// Kept in this crate so the parser and the `is_valid_project_name`
/// debug-assert can't drift.
fn check_project_name(value: &str, span: &Span) -> Result<(), Spanned<ParseError>> {
    if is_valid_project_name(value) {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn up_includes_project_and_files() {
        let cmd = build_up(
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
    fn up_passes_env_file_when_set() {
        let cmd = build_up(
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
    fn up_omits_env_file_when_unset() {
        let cmd = build_up(
            "app",
            &[FilePath::new("/c/compose.yaml")],
            &FilePath::new("/c"),
            None,
        );
        let s = cmd.to_string();
        assert!(!s.contains("--env-file"), "env-file should not appear: {s}");
    }

    #[test]
    fn up_under_sudo_prefixes_with_sudo() {
        let cmd = build_up(
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
    fn down_filters_on_compose_project_label_and_id_format() {
        let cmd = build_down("my_app");
        let s = cmd.to_string();
        assert!(s.starts_with("sh -c "), "expected sh -c: {s}");
        assert!(
            s.contains("com.docker.compose.project=my_app"),
            "label filter missing: {s}"
        );
        assert!(s.contains("{{.ID}}"), "ID format missing: {s}");
        assert!(s.contains("{{.Name}}"), "Name format missing: {s}");
    }

    #[test]
    fn pull_uses_pull_action() {
        let cmd = build_pull(
            "app",
            &[FilePath::new("/c/compose.yaml")],
            &FilePath::new("/c"),
            None,
        );
        let s = cmd.to_string();
        assert_eq!(s, "podman-compose -p app -f /c/compose.yaml pull");
    }

    #[test]
    fn marker_install_carries_both_labels() {
        let cmd = build_marker_install("app", "abc123");
        let s = cmd.to_string();
        assert!(s.contains("--replace"), "got: {s}");
        assert!(s.contains("lusid.compose_project=app"), "got: {s}");
        assert!(s.contains("lusid.compose_config_hash=abc123"), "got: {s}");
        assert!(s.ends_with(" -- lusid-compose-marker-app"), "got: {s}");
    }

    #[test]
    fn marker_uninstall_uses_ignore() {
        let cmd = build_marker_uninstall("app");
        let s = cmd.to_string();
        assert!(s.contains("--force"), "got: {s}");
        assert!(s.contains("--ignore"), "got: {s}");
        assert!(s.ends_with(" -- lusid-compose-marker-app"), "got: {s}");
    }

    #[test]
    fn marker_network_name_format_is_stable() {
        assert_eq!(marker_network_name("foo"), "lusid-compose-marker-foo");
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
    fn op_serdes_omit_sudo_when_false() {
        let op = PodmanComposeOperation::Down {
            project: "app".into(),
            sudo: false,
        };
        let json = serde_json::to_string(&op).unwrap();
        assert!(!json.contains("sudo"), "sudo:false should omit: {json}");
    }

    #[test]
    fn op_serdes_include_sudo_when_true() {
        let op = PodmanComposeOperation::Down {
            project: "app".into(),
            sudo: true,
        };
        let json = serde_json::to_string(&op).unwrap();
        assert!(json.contains(r#""sudo":true"#), "got: {json}");
    }

    // -- ParseParams ------------------------------------------------------

    fn empty_span() -> rimu::Span {
        rimu::Span::new(rimu::SourceId::empty(), 0, 0)
    }

    fn sv(value: Value) -> Spanned<Value> {
        Spanned::new(value, empty_span())
    }

    fn obj(pairs: Vec<(&str, Value)>) -> Spanned<Value> {
        use indexmap::IndexMap;
        let mut map: IndexMap<String, Spanned<Value>> = IndexMap::new();
        for (k, v) in pairs {
            map.insert(k.to_string(), sv(v));
        }
        sv(Value::Object(map))
    }

    fn hp(p: &str) -> Value {
        Value::HostPath(std::path::PathBuf::from(p))
    }

    #[test]
    fn parse_up_with_files() {
        let op = PodmanComposeOperation::parse_params(obj(vec![
            ("action", Value::String("up".into())),
            ("project", Value::String("app".into())),
            ("files", Value::List(vec![sv(hp("/c/app.yaml"))])),
        ]))
        .expect("parse");
        match op {
            PodmanComposeOperation::Up {
                project,
                files,
                working_dir,
                ..
            } => {
                assert_eq!(project, "app");
                assert_eq!(files.len(), 1);
                assert_eq!(working_dir.as_path().to_str().unwrap(), "/c");
            }
            other => panic!("expected Up, got {other:?}"),
        }
    }

    #[test]
    fn parse_down_action() {
        let op = PodmanComposeOperation::parse_params(obj(vec![
            ("action", Value::String("down".into())),
            ("project", Value::String("app".into())),
        ]))
        .expect("parse");
        assert!(matches!(op, PodmanComposeOperation::Down { .. }));
    }

    #[test]
    fn parse_pull_action() {
        let op = PodmanComposeOperation::parse_params(obj(vec![
            ("action", Value::String("pull".into())),
            ("project", Value::String("app".into())),
            ("files", Value::List(vec![sv(hp("/c/app.yaml"))])),
        ]))
        .expect("parse");
        assert!(matches!(op, PodmanComposeOperation::Pull { .. }));
    }

    #[test]
    fn parse_rejects_unknown_action() {
        let err = PodmanComposeOperation::parse_params(obj(vec![(
            "action",
            Value::String("delete".into()),
        )]))
        .expect_err("should reject unknown action");
        assert!(matches!(
            err.inner(),
            ParseError::UnknownDiscriminator { .. }
        ));
    }

    #[test]
    fn parse_rejects_empty_files_for_up() {
        let err = PodmanComposeOperation::parse_params(obj(vec![
            ("action", Value::String("up".into())),
            ("project", Value::String("app".into())),
            ("files", Value::List(vec![])),
        ]))
        .expect_err("should reject empty files");
        assert!(matches!(err.inner(), ParseError::InvalidValue { .. }));
    }

    /// Refuse a project name with shell metacharacters before it can reach
    /// `build_down`'s `format!()` interpolation.
    #[test]
    fn parse_down_rejects_shell_metacharacters_in_project() {
        let err = PodmanComposeOperation::parse_params(obj(vec![
            ("action", Value::String("down".into())),
            ("project", Value::String("x; rm -rf $HOME".into())),
        ]))
        .expect_err("should reject malicious project name");
        match err.inner() {
            ParseError::InvalidValue { reason, .. } => {
                assert!(reason.contains("project name"), "got reason: {reason}");
            }
            other => panic!("expected InvalidValue, got {other:?}"),
        }
    }

    #[test]
    fn parse_up_rejects_invalid_project_name() {
        let err = PodmanComposeOperation::parse_params(obj(vec![
            ("action", Value::String("up".into())),
            ("project", Value::String("My_App".into())),
            ("files", Value::List(vec![sv(hp("/c/app.yaml"))])),
        ]))
        .expect_err("should reject uppercase project");
        assert!(matches!(err.inner(), ParseError::InvalidValue { .. }));
    }
}
