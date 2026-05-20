use async_trait::async_trait;
use displaydoc::Display as DisplaydocDisplay;
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_fs::{self as fs, FsError};
use serde::{Deserialize, Serialize};
use std::{fmt::Display, pin::Pin, time::SystemTime};
use thiserror::Error;
use tokio::io::AsyncRead;
use tracing::info;

use crate::OperationType;
use crate::operations::file::{FileGroup, FileMode, FilePath, FileUser};
use crate::sudo::{
    SUDO_DEFAULT_DIR_MODE, UnsafeSudoPath, check_path_under_sudo, chown_spec, empty_stderr,
    empty_stdout, run_sudo_apply,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DirectoryOperation {
    Create {
        path: FilePath,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },

    /// Atomically create (or replace) a symlink at `path` targeting `source`.
    /// Emitted by `@resource/directory state: "linked"`. Implemented via the same
    /// `fs::create_symlink_atomic` primitive as the file equivalent - kept in
    /// this enum so the streaming TUI message reads `Directory::*` for a
    /// directory resource, rather than `File::Write`. Under `sudo: true`,
    /// uses the same stage-symlink-then-atomic-rename shell shape as the
    /// file equivalent.
    CreateSymlink {
        source: FilePath,
        path: FilePath,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },

    /// Recursively copy `source` into `path`. Emitted by
    /// `@resource/directory state: "sourced"`; `source` is a host-path that must
    /// be reachable from the apply binary (already true on local apply, and
    /// pre-staged onto the same host for dev/remote apply).
    CopyTree {
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

/// Errors from applying a [`DirectoryOperation`]: filesystem I/O or a sudo
/// shell-out failure.
#[derive(Debug, Error, DisplaydocDisplay)]
pub enum DirectoryApplyError {
    /// {0}
    Fs(#[from] FsError),

    /// {0}
    Command(#[from] CommandError),

    /// refusing to operate on empty directory path under sudo
    EmptyPathUnderSudo,

    /// refusing to operate on '/' under sudo
    RootSlashUnderSudo,
}

impl From<UnsafeSudoPath> for DirectoryApplyError {
    fn from(err: UnsafeSudoPath) -> Self {
        match err {
            UnsafeSudoPath::Empty => DirectoryApplyError::EmptyPathUnderSudo,
            UnsafeSudoPath::RootSlash => DirectoryApplyError::RootSlashUnderSudo,
        }
    }
}

impl Display for DirectoryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = |sudo: bool| if sudo { "[sudo] " } else { "" };
        match self {
            DirectoryOperation::Create { path, sudo } => {
                write!(f, "{}Directory::Create(path = {path})", prefix(*sudo))
            }
            DirectoryOperation::CreateSymlink { source, path, sudo } => {
                write!(
                    f,
                    "{}Directory::CreateSymlink(source = {source}, path = {path})",
                    prefix(*sudo)
                )
            }
            DirectoryOperation::CopyTree { source, path, sudo } => {
                write!(
                    f,
                    "{}Directory::CopyTree(source = {source}, path = {path})",
                    prefix(*sudo)
                )
            }
            DirectoryOperation::Remove { path, sudo } => {
                write!(f, "{}Directory::Remove(path = {path})", prefix(*sudo))
            }
            DirectoryOperation::ChangeMode { path, mode, sudo } => {
                write!(
                    f,
                    "{}Directory::ChangeMode(path = {path}, mode = {mode})",
                    prefix(*sudo)
                )
            }
            DirectoryOperation::ChangeOwner {
                path,
                user,
                group,
                sudo,
            } => {
                write!(
                    f,
                    "{}Directory::ChangeOwner(path = {path}, user = {user:?}, group = {group:?})",
                    prefix(*sudo)
                )
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Directory;

#[async_trait]
impl OperationType for Directory {
    type Operation = DirectoryOperation;

    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation> {
        operations
    }

    type ApplyOutput = Pin<Box<dyn Future<Output = Result<(), Self::ApplyError>> + Send + 'static>>;
    type ApplyError = DirectoryApplyError;

    type ApplyStdout = Pin<Box<dyn AsyncRead + Send + 'static>>;
    type ApplyStderr = Pin<Box<dyn AsyncRead + Send + 'static>>;

    async fn apply(
        _ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError> {
        match operation.clone() {
            DirectoryOperation::Create { path, sudo } => {
                info!(sudo, "[directory] create: {}", path);
                if sudo {
                    apply_create_sudo(path).await
                } else {
                    Ok((
                        Box::pin(async move {
                            fs::create_dir(path.as_path()).await?;
                            Ok(())
                        }),
                        empty_stdout(),
                        empty_stderr(),
                    ))
                }
            }
            DirectoryOperation::CreateSymlink { source, path, sudo } => {
                info!(sudo, "[directory] create symlink: {} -> {}", path, source);
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
            DirectoryOperation::CopyTree { source, path, sudo } => {
                info!(sudo, "[directory] copy tree: {} -> {}", source, path);
                if sudo {
                    apply_copy_tree_sudo(source, path).await
                } else {
                    Ok((
                        Box::pin(async move {
                            fs::copy_dir(source.as_path(), path.as_path()).await?;
                            Ok(())
                        }),
                        empty_stdout(),
                        empty_stderr(),
                    ))
                }
            }
            DirectoryOperation::Remove { path, sudo } => {
                info!(sudo, "[directory] remove: {}", path);
                if sudo {
                    apply_remove_sudo(path).await
                } else {
                    Ok((
                        Box::pin(async move {
                            fs::remove_dir(path.as_path()).await?;
                            Ok(())
                        }),
                        empty_stdout(),
                        empty_stderr(),
                    ))
                }
            }
            DirectoryOperation::ChangeMode { path, mode, sudo } => {
                info!(sudo, "[directory] change mode: {} -> {}", path, mode);
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
            DirectoryOperation::ChangeOwner {
                path,
                user,
                group,
                sudo,
            } => {
                info!(
                    sudo,
                    "[directory] change owner: {} -> user {:?} + group {:?}", path, user, group
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

async fn apply_create_sudo(
    path: FilePath,
) -> Result<
    (
        Pin<Box<dyn Future<Output = Result<(), DirectoryApplyError>> + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
    ),
    DirectoryApplyError,
> {
    check_path_under_sudo(&path)?;
    // `install -d` mirrors `mkdir -p` (idempotent re-create) and pins a
    // default mode. Intermediates inherit that mode too - documented
    // divergence from `create_dir_all` which uses umask for intermediates.
    let mode_str = format!("{:o}", SUDO_DEFAULT_DIR_MODE);
    let mut cmd = Command::new("install");
    cmd.arg("-d")
        .arg("-m")
        .arg(&mode_str)
        .arg("--")
        .arg(path.as_path());
    run_sudo_apply(cmd.sudo(), None).await
}

async fn apply_create_symlink_sudo(
    source: FilePath,
    path: FilePath,
) -> Result<
    (
        Pin<Box<dyn Future<Output = Result<(), DirectoryApplyError>> + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
    ),
    DirectoryApplyError,
> {
    check_path_under_sudo(&path)?;
    // Same atomic-replace shape as `crate::operations::file::apply_create_symlink_sudo`.
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

async fn apply_copy_tree_sudo(
    source: FilePath,
    path: FilePath,
) -> Result<
    (
        Pin<Box<dyn Future<Output = Result<(), DirectoryApplyError>> + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
    ),
    DirectoryApplyError,
> {
    check_path_under_sudo(&path)?;
    let mut cmd = Command::new("cp");
    cmd.arg("--recursive")
        .arg("--")
        .arg(source.as_path())
        .arg(path.as_path());
    run_sudo_apply(cmd.sudo(), None).await
}

/// Directory removes are always recursive. The `/` check is handled
/// up-front by [`check_path_under_sudo`].
async fn apply_remove_sudo(
    path: FilePath,
) -> Result<
    (
        Pin<Box<dyn Future<Output = Result<(), DirectoryApplyError>> + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
    ),
    DirectoryApplyError,
> {
    check_path_under_sudo(&path)?;
    let mut cmd = Command::new("rm");
    cmd.arg("-rf").arg("--").arg(path.as_path());
    run_sudo_apply(cmd.sudo(), None).await
}

async fn apply_change_mode_sudo(
    path: FilePath,
    mode: FileMode,
) -> Result<
    (
        Pin<Box<dyn Future<Output = Result<(), DirectoryApplyError>> + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
    ),
    DirectoryApplyError,
> {
    check_path_under_sudo(&path)?;
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
        Pin<Box<dyn Future<Output = Result<(), DirectoryApplyError>> + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
        Pin<Box<dyn AsyncRead + Send + 'static>>,
    ),
    DirectoryApplyError,
> {
    check_path_under_sudo(&path)?;
    let spec = chown_spec(user.as_ref(), group.as_ref());
    let mut cmd = Command::new("chown");
    cmd.arg(&spec).arg("--").arg(path.as_path());
    run_sudo_apply(cmd.sudo(), None).await
}
