use async_trait::async_trait;
use displaydoc::Display as DisplaydocDisplay;
use lusid_ctx::Context;
use lusid_fs::{self as fs, FsError};
use lusid_view::impl_display_render;
use secrecy::ExposeSecret;
use std::{
    fmt::{Debug, Display},
    path::Path,
    pin::Pin,
};
use thiserror::Error;
use tokio::io::AsyncRead;
use tracing::info;

use crate::OperationType;

/// Errors from applying a [`FileOperation`]: filesystem I/O or a missing
/// secret lookup during [`FileSource::Secret`] resolution.
#[derive(Debug, Error, DisplaydocDisplay)]
pub enum FileApplyError {
    /// {0}
    Fs(#[from] FsError),

    // Twin of `lusid_resource::resources::file::FileStateError::MissingSecret`
    // - the state-side fires when a file already exists (contents diffed
    // against the bundle); this apply-side variant is the backstop for
    // new-file writes, where state short-circuited on the missing path
    // without consulting the bundle.
    /// secret {name:?} referenced by file operation was not found in decrypted secrets bundle
    MissingSecret { name: String },
}

#[derive(Debug, Clone)]
pub enum FileSource {
    Contents(Vec<u8>),

    /// Copy the file at this host path into `path` atomically.
    Path(FilePath),

    /// Reference to a decrypted secret by name; resolved against
    /// [`Context::secrets`] at apply time so plaintext never lives in the
    /// resource/change/operation tree.
    Secret(String),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone)]
pub enum FileOperation {
    Write {
        path: FilePath,
        source: FileSource,
    },
    /// Atomically create (or replace) a symlink at `path` targeting `source`.
    /// Emitted by `@resource/file state: "linked"`.
    CreateSymlink {
        source: FilePath,
        path: FilePath,
    },
    Remove {
        path: FilePath,
    },
    ChangeMode {
        path: FilePath,
        mode: FileMode,
    },
    ChangeOwner {
        path: FilePath,
        user: Option<FileUser>,
        group: Option<FileGroup>,
    },
}

impl Display for FileOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileOperation::Write { path, source } => match source {
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
            },
            FileOperation::CreateSymlink { source, path } => write!(
                f,
                "File::CreateSymlink(source = {}, path = {})",
                source, path
            ),
            FileOperation::Remove { path } => write!(f, "File::Remove(path = {})", path),
            FileOperation::ChangeMode { path, mode } => {
                write!(f, "File::ChangeMode(path = {}, mode = {})", path, mode)
            }
            FileOperation::ChangeOwner { path, user, group } => {
                write!(
                    f,
                    "File::ChangeOwner(path = {}, user = {:?}, group = {:?})",
                    path, user, group
                )
            }
        }
    }
}

impl_display_render!(FileOperation);

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
        let stdout = Box::pin(tokio::io::empty());
        let stderr = Box::pin(tokio::io::empty());

        match operation.clone() {
            FileOperation::Write { path, source } => {
                let resolved: WriteSource = match source {
                    FileSource::Contents(bytes) => {
                        info!("[file] write contents: {} ({} bytes)", path, bytes.len());
                        WriteSource::Bytes(bytes)
                    }
                    FileSource::Path(source) => {
                        info!("[file] copy file: {} -> {}", source, path);
                        WriteSource::Copy(source)
                    }
                    FileSource::Secret(name) => {
                        info!("[file] write secret: {} -> {}", name, path);
                        let secret = ctx
                            .secrets()
                            .get(&name)
                            .ok_or_else(|| FileApplyError::MissingSecret { name: name.clone() })?;
                        WriteSource::SecretBytes(secret.expose_secret().as_bytes().to_vec())
                    }
                };
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
                    stdout,
                    stderr,
                ))
            }
            FileOperation::CreateSymlink { source, path } => {
                info!("[file] create symlink: {} -> {}", path, source);
                Ok((
                    Box::pin(async move {
                        fs::create_symlink_atomic(source.as_path(), path.as_path()).await?;
                        Ok(())
                    }),
                    stdout,
                    stderr,
                ))
            }
            FileOperation::Remove { path } => {
                info!("[file] remove file: {}", path);
                Ok((
                    Box::pin(async move {
                        fs::remove_file(path.as_path()).await?;
                        Ok(())
                    }),
                    stdout,
                    stderr,
                ))
            }
            FileOperation::ChangeMode { path, mode } => {
                info!("[file] change mode: {} -> {}", path, mode);
                Ok((
                    Box::pin(async move {
                        fs::change_mode(path.as_path(), mode.as_u32()).await?;
                        Ok(())
                    }),
                    stdout,
                    stderr,
                ))
            }
            FileOperation::ChangeOwner { path, user, group } => {
                info!(
                    "[file] change user: {} -> user {:?} + group {:?}",
                    path, user, group
                );
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
                    stdout,
                    stderr,
                ))
            }
        }
    }
}
