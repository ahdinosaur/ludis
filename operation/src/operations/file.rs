use async_trait::async_trait;
use displaydoc::Display as DisplaydocDisplay;
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_fs::{self as fs, FsError};
use secrecy::ExposeSecret;
use serde::{Deserialize, Serialize};
use std::{
    fmt::{Debug, Display},
    path::{Path, PathBuf},
    pin::Pin,
    time::SystemTime,
};
use thiserror::Error;
use tokio::io::AsyncRead;
use tracing::info;

use crate::OperationType;
use crate::sudo::{
    SUDO_DEFAULT_FILE_MODE, SUDO_SECRET_FILE_MODE, UnsafeSudoPath, check_path_under_sudo,
    chown_spec, empty_stderr, empty_stdout, run_sudo_apply, sudo_stage_path,
};

/// Errors from applying a [`FileOperation`]: filesystem I/O, a sudo
/// shell-out, or a missing secret lookup during [`FileSource::Secret`]
/// resolution.
#[derive(Debug, Error, DisplaydocDisplay)]
pub enum FileApplyError {
    /// {0}
    Fs(#[from] FsError),

    /// {0}
    Command(#[from] CommandError),

    // Twin of `lusid_resource::resources::file::FileStateError::MissingSecret`
    // - the state-side fires when a file already exists (contents diffed
    // against the bundle); this apply-side variant is the backstop for
    // new-file writes, where state short-circuited on the missing path
    // without consulting the bundle.
    /// secret {name:?} referenced by file operation was not found in decrypted secrets bundle
    MissingSecret { name: String },

    /// refusing to operate on empty file path under sudo
    EmptyPathUnderSudo,

    /// refusing to operate on '/' under sudo
    RootSlashUnderSudo,
}

impl From<UnsafeSudoPath> for FileApplyError {
    fn from(err: UnsafeSudoPath) -> Self {
        match err {
            UnsafeSudoPath::Empty => FileApplyError::EmptyPathUnderSudo,
            UnsafeSudoPath::RootSlash => FileApplyError::RootSlashUnderSudo,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FileSource {
    Contents(Vec<u8>),

    /// Copy the file at this host path into `path` atomically.
    Path(FilePath),

    /// Reference to a decrypted secret by name; resolved against
    /// [`Context::secrets`] at apply time so plaintext never lives in the
    /// resource/change/operation tree.
    Secret(String),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(transparent)]
pub struct FilePath(String);

impl FilePath {
    pub fn new(value: impl Into<String>) -> Self {
        Self(value.into())
    }

    pub fn as_path(&self) -> &Path {
        Path::new(&self.0)
    }
}

impl Display for FilePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct FileMode(u32);

impl FileMode {
    pub fn new(value: u32) -> Self {
        Self(value)
    }

    pub fn as_u32(&self) -> u32 {
        self.0
    }
}

impl Display for FileMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:o}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct FileUser(String);

impl FileUser {
    pub fn new(value: impl Into<String>) -> Self {
        Self(value.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Display for FileUser {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct FileGroup(String);

impl FileGroup {
    pub fn new(value: impl Into<String>) -> Self {
        Self(value.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Display for FileGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FileOperation {
    Write {
        path: FilePath,
        source: FileSource,
        /// When set, the write happens via a user-cache stage file followed by
        /// `sudo -n install`, so target paths under `/etc/`, `/var/`, etc.
        /// land correctly under `lusid local apply` (which runs as the
        /// calling user). When false, [`fs::write_file_atomic`] /
        /// [`fs::copy_file_atomic`] write directly as the current user.
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    /// Atomically create (or replace) a symlink at `path` targeting `source`.
    /// Emitted by `@resource/file state: "linked"`. Under `sudo`, replaces
    /// atomically by staging the symlink at a sibling temp path and then
    /// `mv -Tf`ing it over the destination - same `rename(2)` guarantee as
    /// the user-mode path.
    CreateSymlink {
        source: FilePath,
        path: FilePath,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    Remove {
        path: FilePath,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    ChangeMode {
        path: FilePath,
        mode: FileMode,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    ChangeOwner {
        path: FilePath,
        user: Option<FileUser>,
        group: Option<FileGroup>,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
}

impl Display for FileOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = |sudo: bool| if sudo { "[sudo] " } else { "" };
        match self {
            FileOperation::Write { path, source, sudo } => {
                write!(f, "{}", prefix(*sudo))?;
                match source {
                    FileSource::Contents(contents) => write!(
                        f,
                        "File::Write(path = {}, source = Contents({} bytes))",
                        path,
                        contents.len()
                    ),
                    FileSource::Path(source_path) => write!(
                        f,
                        "File::Write(path = {}, source = Path({}))",
                        path, source_path
                    ),
                    FileSource::Secret(name) => {
                        write!(f, "File::Write(path = {}, source = Secret({}))", path, name)
                    }
                }
            }
            FileOperation::CreateSymlink { source, path, sudo } => write!(
                f,
                "{}File::CreateSymlink(source = {}, path = {})",
                prefix(*sudo),
                source,
                path
            ),
            FileOperation::Remove { path, sudo } => {
                write!(f, "{}File::Remove(path = {})", prefix(*sudo), path)
            }
            FileOperation::ChangeMode { path, mode, sudo } => {
                write!(
                    f,
                    "{}File::ChangeMode(path = {}, mode = {})",
                    prefix(*sudo),
                    path,
                    mode
                )
            }
            FileOperation::ChangeOwner {
                path,
                user,
                group,
                sudo,
            } => {
                write!(
                    f,
                    "{}File::ChangeOwner(path = {}, user = {:?}, group = {:?})",
                    prefix(*sudo),
                    path,
                    user,
                    group
                )
            }
        }
    }
}

/// Apply-time resolution of a [`FileSource`] for a write:
///
/// - `Bytes` covers inline plan-supplied contents.
/// - `SecretBytes` covers decrypted-secret plaintext. Treated separately
///   so the atomic-write helper can pin mode `0o600` on the temp file
///   from the moment it's `open(2)`'d - closing the umask-window between
///   write and the followup `ChangeMode` op.
/// - `Copy` covers a path-sourced copy.
///
/// Resolved up-front so the inner async block doesn't borrow `ctx` (and so
/// secret plaintext lives only as long as the `Vec<u8>` it's copied into).
enum WriteSource {
    Bytes(Vec<u8>),
    SecretBytes(Vec<u8>),
    Copy(FilePath),
}

/// Initial mode pinned on the temp file when writing a decrypted secret.
/// Matches `lusid_resource::resources::secret::DEFAULT_MODE`; if the plan
/// declared a more permissive `mode`, the followup `ChangeMode` op
/// relaxes it after the file lands. The point of pinning here is to
/// ensure the bytes never sit on disk under umask perms (typically
/// `0o644`), even briefly.
const SECRET_INITIAL_MODE: u32 = 0o600;

#[derive(Debug, Clone)]
pub struct File;

#[async_trait]
impl OperationType for File {
    type Operation = FileOperation;

    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation> {
        operations
    }

    type ApplyOutput = Pin<Box<dyn Future<Output = Result<(), Self::ApplyError>> + Send + 'static>>;
    type ApplyError = FileApplyError;

    type ApplyStdout = Pin<Box<dyn AsyncRead + Send + 'static>>;
    type ApplyStderr = Pin<Box<dyn AsyncRead + Send + 'static>>;

    async fn apply(
        ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError> {
        match operation.clone() {
            FileOperation::Write { path, source, sudo } => {
                let resolved: WriteSource = match source {
                    FileSource::Contents(bytes) => {
                        info!(
                            sudo,
                            "[file] write contents: {} ({} bytes)",
                            path,
                            bytes.len()
                        );
                        WriteSource::Bytes(bytes)
                    }
                    FileSource::Path(source) => {
                        info!(sudo, "[file] copy file: {} -> {}", source, path);
                        WriteSource::Copy(source)
                    }
                    FileSource::Secret(name) => {
                        info!(sudo, "[file] write secret: {} -> {}", name, path);
                        let secret = ctx
                            .secrets()
                            .get(&name)
                            .ok_or_else(|| FileApplyError::MissingSecret { name: name.clone() })?;
                        WriteSource::SecretBytes(secret.expose_secret().as_bytes().to_vec())
                    }
                };
                if sudo {
                    apply_write_sudo(ctx, path, resolved).await
                } else {
                    Ok((
                        Box::pin(async move {
                            match resolved {
                                WriteSource::Bytes(bytes) => {
                                    fs::write_file_atomic(path.as_path(), &bytes).await?
                                }
                                WriteSource::SecretBytes(bytes) => {
                                    fs::write_file_atomic_with_initial_mode(
                                        path.as_path(),
                                        &bytes,
                                        Some(SECRET_INITIAL_MODE),
                                    )
                                    .await?
                                }
                                WriteSource::Copy(source) => {
                                    fs::copy_file_atomic(source.as_path(), path.as_path()).await?
                                }
                            }
                            Ok(())
                        }),
                        empty_stdout(),
                        empty_stderr(),
                    ))
                }
            }
            FileOperation::CreateSymlink { source, path, sudo } => {
                info!(sudo, "[file] create symlink: {} -> {}", path, source);
                if sudo {
                    apply_create_symlink_sudo(source, path).await
                } else {
                    Ok((
                        Box::pin(async move {
                            fs::create_symlink_atomic(source.as_path(), path.as_path()).await?;
                            Ok(())
                        }),
                        empty_stdout(),
                        empty_stderr(),
                    ))
                }
            }
            FileOperation::Remove { path, sudo } => {
                info!(sudo, "[file] remove file: {}", path);
                if sudo {
                    apply_remove_sudo(path).await
                } else {
                    Ok((
                        Box::pin(async move {
                            fs::remove_file(path.as_path()).await?;
                            Ok(())
                        }),
                        empty_stdout(),
                        empty_stderr(),
                    ))
                }
            }
            FileOperation::ChangeMode { path, mode, sudo } => {
                info!(sudo, "[file] change mode: {} -> {}", path, mode);
                if sudo {
                    apply_change_mode_sudo(path, mode).await
                } else {
                    Ok((
                        Box::pin(async move {
                            fs::change_mode(path.as_path(), mode.as_u32()).await?;
                            Ok(())
                        }),
                        empty_stdout(),
                        empty_stderr(),
                    ))
                }
            }
            FileOperation::ChangeOwner {
                path,
                user,
                group,
                sudo,
            } => {
                info!(
                    sudo,
                    "[file] change user: {} -> user {:?} + group {:?}", path, user, group
                );
                if sudo {
                    apply_change_owner_sudo(path, user, group).await
                } else {
                    Ok((
                        Box::pin(async move {
                            fs::change_owner(
                                path.as_path(),
                                user.as_ref().map(|u| u.as_str()),
                                group.as_ref().map(|g| g.as_str()),
                            )
                            .await?;
                            Ok(())
                        }),
                        empty_stdout(),
                        empty_stderr(),
                    ))
                }
            }
        }
    }
}

async fn apply_write_sudo(
    ctx: &mut Context,
    path: FilePath,
    source: WriteSource,
) -> Result<
    (
        Pin<Box<dyn Future<Output = Result<(), FileApplyError>> + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
    ),
    FileApplyError,
> {
    check_path_under_sudo(&path)?;
    let (stage, mode_str, cleanup) = match source {
        WriteSource::Bytes(bytes) => {
            let stage = sudo_stage_path(ctx).await?;
            fs::write_file_atomic_with_initial_mode(&stage, &bytes, Some(SUDO_DEFAULT_FILE_MODE))
                .await?;
            (
                stage.clone(),
                format!("{:o}", SUDO_DEFAULT_FILE_MODE),
                Some(stage),
            )
        }
        WriteSource::SecretBytes(bytes) => {
            let stage = sudo_stage_path(ctx).await?;
            fs::write_file_atomic_with_initial_mode(&stage, &bytes, Some(SUDO_SECRET_FILE_MODE))
                .await?;
            (
                stage.clone(),
                format!("{:o}", SUDO_SECRET_FILE_MODE),
                Some(stage),
            )
        }
        WriteSource::Copy(source_path) => (
            // Source is already on the operator's filesystem; `install` reads
            // it directly. No staging needed, no cleanup.
            PathBuf::from(source_path.as_path()),
            format!("{:o}", SUDO_DEFAULT_FILE_MODE),
            None,
        ),
    };

    let mut cmd = Command::new("install");
    cmd.arg("-m")
        .arg(&mode_str)
        .arg("--")
        .arg(&stage)
        .arg(path.as_path());
    run_sudo_apply(cmd.sudo(), cleanup).await
}

async fn apply_create_symlink_sudo(
    source: FilePath,
    path: FilePath,
) -> Result<
    (
        Pin<Box<dyn Future<Output = Result<(), FileApplyError>> + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
    ),
    FileApplyError,
> {
    check_path_under_sudo(&path)?;
    // Atomic replace: stage a sibling temp symlink and `mv -Tf` it over the
    // destination. `mv -T` (no-target-directory) refuses to rename into a
    // directory, matching `rename(2)` semantics on Linux. Both steps run as
    // root via a single `sudo -n sh -c`, since the sibling lives in the
    // root-owned target directory.
    //
    // Note(cc): `mv -T` is GNU coreutils-specific (no `-T` on BSD `mv`).
    // lusid targets Linux today (Arch/Debian); a future busybox target would
    // need to swap the shell shape for a non-atomic `ln -sfn`, or invoke
    // `mv --no-target-directory` from busybox-coreutils.
    let nanos = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let stage = format!("{}.{}.tmp", path.as_path().display(), nanos);
    let script = "ln -s -- \"$0\" \"$1\" && mv -Tf \"$1\" \"$2\"";
    let mut cmd = Command::new("sh");
    cmd.arg("-c")
        .arg(script)
        .arg(source.as_path())
        .arg(&stage)
        .arg(path.as_path());
    run_sudo_apply(cmd.sudo(), None).await
}

/// File-level `Remove` under sudo. Files are never removed recursively; the
/// caller in `Directory::apply` has its own helper for `rm -rf`. The `/`
/// check is handled up-front by [`check_path_under_sudo`].
async fn apply_remove_sudo(
    path: FilePath,
) -> Result<
    (
        Pin<Box<dyn Future<Output = Result<(), FileApplyError>> + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
    ),
    FileApplyError,
> {
    check_path_under_sudo(&path)?;
    let mut cmd = Command::new("rm");
    cmd.arg("-f").arg("--").arg(path.as_path());
    run_sudo_apply(cmd.sudo(), None).await
}

async fn apply_change_mode_sudo(
    path: FilePath,
    mode: FileMode,
) -> Result<
    (
        Pin<Box<dyn Future<Output = Result<(), FileApplyError>> + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
    ),
    FileApplyError,
> {
    check_path_under_sudo(&path)?;
    // `chmod` interprets numeric args as octal; emit the octal digits
    // without an `0o` prefix so it's unambiguous to busybox and coreutils
    // alike.
    let mode_str = format!("{:o}", mode.as_u32());
    let mut cmd = Command::new("chmod");
    cmd.arg(&mode_str).arg("--").arg(path.as_path());
    run_sudo_apply(cmd.sudo(), None).await
}

async fn apply_change_owner_sudo(
    path: FilePath,
    user: Option<FileUser>,
    group: Option<FileGroup>,
) -> Result<
    (
        Pin<Box<dyn Future<Output = Result<(), FileApplyError>> + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
    ),
    FileApplyError,
> {
    check_path_under_sudo(&path)?;
    let spec = chown_spec(user.as_ref(), group.as_ref());
    let mut cmd = Command::new("chown");
    cmd.arg(&spec).arg("--").arg(path.as_path());
    run_sudo_apply(cmd.sudo(), None).await
}

#[cfg(test)]
mod sudo_tests {
    use super::*;

    // `chown_spec` lives in `crate::sudo` now; tested there.

    #[test]
    fn write_with_sudo_serializes_field() {
        let op = FileOperation::Write {
            path: FilePath::new("/etc/foo"),
            source: FileSource::Contents(b"hello".to_vec()),
            sudo: true,
        };
        let json = serde_json::to_string(&op).unwrap();
        assert!(
            json.contains(r#""sudo":true"#),
            "sudo:true should appear on the wire: {json}"
        );
    }

    #[test]
    fn write_without_sudo_omits_field_from_wire() {
        // `skip_serializing_if = "Not::not"` should drop the field entirely
        // when false; this keeps the wire payload stable for the 99% case
        // and avoids gratuitous diff churn in any snapshot tests.
        let op = FileOperation::Write {
            path: FilePath::new("/tmp/foo"),
            source: FileSource::Contents(b"hello".to_vec()),
            sudo: false,
        };
        let json = serde_json::to_string(&op).unwrap();
        assert!(
            !json.contains("sudo"),
            "sudo:false should be omitted on the wire: {json}"
        );
    }

    #[test]
    fn write_with_sudo_missing_field_deserializes_to_false() {
        // `serde(default)` ensures older payloads (pre-sudo) deserialize
        // cleanly into the new shape with `sudo: false`.
        let json = r#"{"Write":{"path":"/tmp/foo","source":{"Contents":[]}}}"#;
        let op: FileOperation = serde_json::from_str(json).unwrap();
        match op {
            FileOperation::Write { sudo, .. } => {
                assert!(!sudo, "missing sudo should default to false")
            }
            other => panic!("expected Write, got {other:?}"),
        }
    }

    #[test]
    fn display_prefixes_sudo_ops() {
        let op = FileOperation::Remove {
            path: FilePath::new("/etc/foo"),
            sudo: true,
        };
        let display = op.to_string();
        assert!(display.starts_with("[sudo] "), "got: {display}");
    }
}
