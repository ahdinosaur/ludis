//! Helpers for `sudo: true` execution shared between
//! [`crate::operations::file`] and [`crate::operations::directory`].
//!
//! Both modules need the same shape: stage into a user-writable cache (for
//! byte-writes), invoke a `sudo -n`-wrapped command, stream its stdio, and
//! best-effort clean up the stage afterwards. This module centralises the
//! pieces that don't vary across the file/dir split.

use std::path::PathBuf;
use std::pin::Pin;
use std::time::SystemTime;

use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_fs::{self as fs, FsError};
use tokio::io::AsyncRead;

use crate::operations::file::{FileGroup, FilePath, FileUser};

/// Default mode applied to staged byte-writes and to `install -m` when no
/// explicit `mode:` is declared. `0644` avoids `install`'s default `0755`
/// (executable bit) landing on a plain config file. Explicit `mode:` on the
/// resource triggers a follow-up `ChangeMode` atom that overrides this.
pub const SUDO_DEFAULT_FILE_MODE: u32 = 0o644;

/// Default mode for `install -d` under sudo. Matches `mkdir`'s usual `0755`
/// expectation. Explicit `mode:` is applied via a follow-up `ChangeMode`
/// atom. Note: `install -d` applies this mode to every intermediate it
/// creates, unlike `mkdir -p` which uses the umask for intermediates.
pub const SUDO_DEFAULT_DIR_MODE: u32 = 0o755;

/// Stage mode for secret writes - pins `0o600` from `open(2)` so plaintext
/// never sits on disk under a looser umask, even briefly.
pub const SUDO_SECRET_FILE_MODE: u32 = 0o600;

/// Subdirectory under [`Context::paths().cache_dir()`] where sudo-stage temp
/// files live. Best-effort cleaned up after a successful install; orphans on
/// apply panic/kill are a known limitation (same as `@resource/apt-repo`'s
/// stage dir).
pub const SUDO_STAGE_SUBDIR: &str = "file-sudo-stage";

/// Build a unique nanos-named stage path under
/// `<cache>/file-sudo-stage/<nanos>.tmp`. Sequential applies don't collide
/// thanks to nano-resolution; concurrent applies to the same target shouldn't
/// happen and aren't guarded against here.
pub async fn sudo_stage_path(ctx: &Context) -> Result<PathBuf, FsError> {
    let dir = ctx.paths().cache_dir().join(SUDO_STAGE_SUBDIR);
    fs::create_dir(&dir).await?;
    let nanos = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    Ok(dir.join(format!("{nanos}.tmp")))
}

/// Empty stdio stream for non-streaming op completions (used by the non-sudo
/// fast path so the apply tuple shape is uniform).
pub fn empty_stdout() -> Pin<Box<dyn AsyncRead + Send + 'static>> {
    Box::pin(tokio::io::empty())
}

/// Empty stderr stream. See [`empty_stdout`].
pub fn empty_stderr() -> Pin<Box<dyn AsyncRead + Send + 'static>> {
    Box::pin(tokio::io::empty())
}

/// Reject an empty `FilePath` or a literal `/` before shelling out under
/// sudo. `lusid-apply` has no business issuing `sudo rm -rf -- ""` or
/// `sudo rm -rf -- /`; this guard catches an upstream regression at the
/// shell-out boundary rather than letting the shell interpret it. Promotes
/// the previous `debug_assert!` to a release-build check (paranoia is cheap
/// here; an unguarded `sudo rm -rf /` is career-ending).
pub fn check_path_under_sudo(path: &FilePath) -> Result<(), UnsafeSudoPath> {
    let p = path.as_path();
    if p.as_os_str().is_empty() {
        return Err(UnsafeSudoPath::Empty);
    }
    if p == std::path::Path::new("/") {
        return Err(UnsafeSudoPath::RootSlash);
    }
    Ok(())
}

/// Returned by [`check_path_under_sudo`] for an unsafe path shape. Wrappers
/// (e.g. [`crate::operations::file::FileApplyError`]) carry their own
/// variants for this case.
#[derive(Debug)]
pub enum UnsafeSudoPath {
    /// Empty `FilePath`.
    Empty,
    /// Literal `/`. Refusing to `sudo rm -rf /` is non-negotiable.
    RootSlash,
}

/// Render a `chown` spec for the three shapes lusid emits:
/// - `Some(user)`, `None` -> `"<user>"`
/// - `None`, `Some(group)` -> `":<group>"`
/// - `Some(user)`, `Some(group)` -> `"<user>:<group>"`
///
/// The `None, None` case is precluded by the resource layer's
/// `permission_atoms` (an empty `ChangeOwner` is never emitted), but defaults
/// to `:` here so the shell-out fails loudly rather than silently chowning to
/// nothing if a future change ever produces that shape.
pub fn chown_spec(user: Option<&FileUser>, group: Option<&FileGroup>) -> String {
    match (user, group) {
        (Some(u), Some(g)) => format!("{u}:{g}"),
        (Some(u), None) => u.to_string(),
        (None, Some(g)) => format!(":{g}"),
        (None, None) => ":".to_string(),
    }
}

/// Drive a `sudo -n`-wrapped command to completion, streaming its stdio. The
/// returned future awaits the child, best-effort removes `cleanup_path` if
/// any, and surfaces a non-zero exit as [`CommandError::Failure`].
///
/// `E` is the caller's `ApplyError` family; the bound `From<CommandError>`
/// lets each module map `CommandError` into its own enum variant.
pub async fn run_sudo_apply<E>(
    mut cmd: Command,
    cleanup_path: Option<PathBuf>,
) -> Result<
    (
        Pin<Box<dyn Future<Output = Result<(), E>> + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
    ),
    E,
>
where
    E: From<CommandError> + Send + 'static,
{
    let cmd_display = cmd.to_string();
    let output = cmd.output().await.map_err(E::from)?;
    let stdout: Pin<Box<dyn AsyncRead + Send + 'static>> = Box::pin(output.stdout);
    let stderr: Pin<Box<dyn AsyncRead + Send + 'static>> = Box::pin(output.stderr);
    let future: Pin<Box<dyn Future<Output = Result<(), E>> + Send + 'static>> =
        Box::pin(async move {
            let result = output.status.await;
            if let Some(path) = cleanup_path.as_ref() {
                let _ = tokio::fs::remove_file(path).await;
            }
            let status = result.map_err(E::from)?;
            if !status.success() {
                return Err(E::from(CommandError::Failure {
                    command: cmd_display,
                    stderr: format!("exit status {status}"),
                }));
            }
            Ok(())
        });
    Ok((future, stdout, stderr))
}

#[cfg(test)]
mod path_guard_tests {
    use super::*;

    #[test]
    fn empty_path_is_rejected() {
        match check_path_under_sudo(&FilePath::new("")) {
            Err(UnsafeSudoPath::Empty) => {}
            other => panic!("expected UnsafeSudoPath::Empty, got {other:?}"),
        }
    }

    #[test]
    fn root_slash_is_rejected() {
        match check_path_under_sudo(&FilePath::new("/")) {
            Err(UnsafeSudoPath::RootSlash) => {}
            other => panic!("expected UnsafeSudoPath::RootSlash, got {other:?}"),
        }
    }

    #[test]
    fn normal_path_is_accepted() {
        check_path_under_sudo(&FilePath::new("/etc/foo")).expect("normal path should pass");
    }

    #[test]
    fn chown_spec_renders_three_shapes() {
        let u = FileUser::new("alice");
        let g = FileGroup::new("wheel");
        assert_eq!(chown_spec(Some(&u), Some(&g)), "alice:wheel");
        assert_eq!(chown_spec(Some(&u), None), "alice");
        assert_eq!(chown_spec(None, Some(&g)), ":wheel");
        // Defensive: an empty `ChangeOwner` shouldn't be emitted by the
        // resource layer, but if it ever is, the spec defaults to `:`
        // which makes the shell-out fail loudly rather than silently
        // succeed by chowning to nothing.
        assert_eq!(chown_spec(None, None), ":");
    }
}
