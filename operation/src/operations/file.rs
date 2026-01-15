use async_trait::async_trait;
use lusid_ctx::Context;
use lusid_fs::{self as fs, FsError};
use std::{fmt::Display, path::Path, pin::Pin};
use tokio::io::AsyncRead;
use tracing::info;

use crate::OperationType;

#[derive(Debug, Clone)]
pub enum FileSource {
    Contents(Vec<u8>),
    Path(FilePath),
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
    WriteFile {
        path: FilePath,
        source: FileSource,
    },
    CopyFile {
        source: FilePath,
        destination: FilePath,
    },
    MoveFile {
        source: FilePath,
        destination: FilePath,
    },
    RemoveFile {
        path: FilePath,
    },
    CreateDirectory {
        path: FilePath,
    },
    RemoveDirectory {
        path: FilePath,
    },
    CreateSymlink {
        path: FilePath,
        target: FilePath,
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
            FileOperation::WriteFile { path, source } => match source {
                FileSource::Contents(contents) => write!(
                    f,
                    "File::WriteFile(path = {}, source = Contents({} bytes))",
                    path,
                    contents.len()
                ),
                FileSource::Path(source_path) => write!(
                    f,
                    "File::WriteFile(path = {}, source = Path({}))",
                    path, source_path
                ),
            },
            FileOperation::CopyFile {
                source,
                destination,
            } => write!(
                f,
                "File::CopyFile(source = {}, destination = {})",
                source, destination
            ),
            FileOperation::MoveFile {
                source,
                destination,
            } => write!(
                f,
                "File::MoveFile(source = {}, destination = {})",
                source, destination
            ),
            FileOperation::RemoveFile { path } => write!(f, "File::RemoveFile(path = {})", path),
            FileOperation::CreateDirectory { path } => {
                write!(f, "File::CreateDirectory(path = {})", path)
            }
            FileOperation::RemoveDirectory { path } => {
                write!(f, "File::RemoveDirectory(path = {})", path)
            }
            FileOperation::CreateSymlink { path, target } => write!(
                f,
                "File::CreateSymlink(path = {}, target = {})",
                path, target
            ),
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

#[derive(Debug, Clone)]
pub struct File;

#[async_trait]
impl OperationType for File {
    type Operation = FileOperation;

    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation> {
        operations
    }

    type ApplyOutput = Pin<Box<dyn Future<Output = Result<(), Self::ApplyError>> + Send + 'static>>;
    type ApplyError = FsError;

    type ApplyStdout = Pin<Box<dyn AsyncRead + Send + 'static>>;
    type ApplyStderr = Pin<Box<dyn AsyncRead + Send + 'static>>;

    async fn apply(
        _ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError> {
        let stdout = Box::pin(tokio::io::empty());
        let stderr = Box::pin(tokio::io::empty());

        match operation.clone() {
            FileOperation::WriteFile { path, source } => {
                info!("[file] write file: {}", path);
                Ok((
                    Box::pin(async move {
                        match source {
                            FileSource::Contents(contents) => {
                                fs::write_file_atomic(path.as_path(), &contents).await
                            }
                            FileSource::Path(source) => {
                                fs::copy_file_atomic(source.as_path(), path.as_path()).await
                            }
                        }
                    }),
                    stdout,
                    stderr,
                ))
            }
            FileOperation::CopyFile {
                source,
                destination,
            } => {
                info!("[file] copy file: {} -> {}", source, destination);
                Ok((
                    Box::pin(async move {
                        fs::copy_file_atomic(destination.as_path(), source.as_path()).await
                    }),
                    stdout,
                    stderr,
                ))
            }
            FileOperation::MoveFile {
                source,
                destination,
            } => {
                info!("[file] move file: {} -> {}", source, destination);
                Ok((
                    Box::pin(async move {
                        fs::rename_file(source.as_path(), destination.as_path()).await
                    }),
                    stdout,
                    stderr,
                ))
            }
            FileOperation::RemoveFile { path } => {
                info!("[file] remove file: {}", path);
                Ok((
                    Box::pin(async move { fs::remove_file(path.as_path()).await }),
                    stdout,
                    stderr,
                ))
            }
            FileOperation::CreateDirectory { path } => {
                info!("[file] create directory: {}", path);
                Ok((
                    Box::pin(async move { fs::create_dir(path.as_path()).await }),
                    stdout,
                    stderr,
                ))
            }
            FileOperation::RemoveDirectory { path } => {
                info!("[file] remove directory: {}", path);
                Ok((
                    Box::pin(async move { fs::remove_dir(path.as_path()).await }),
                    stdout,
                    stderr,
                ))
            }
            FileOperation::CreateSymlink { path, target } => {
                info!("[file] create symlink: {} -> {}", path, target);
                Ok((
                    Box::pin(
                        async move { fs::create_symlink(path.as_path(), target.as_path()).await },
                    ),
                    stdout,
                    stderr,
                ))
            }
            FileOperation::ChangeMode { path, mode } => {
                info!("[file] change mode: {} -> {}", path, mode);
                Ok((
                    Box::pin(async move { fs::change_mode(path.as_path(), mode.as_u32()).await }),
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
                            user.as_ref().map(|g| g.as_str()),
                        )
                        .await
                    }),
                    stdout,
                    stderr,
                ))
            }
        }
    }
}
