use std::{fmt::Display, path::PathBuf, pin::Pin};

use async_trait::async_trait;
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_fs::{self as fs, FsError};
use lusid_http::HttpError;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use tokio::process::{ChildStderr, ChildStdout};
use tracing::info;

use crate::OperationType;
use crate::operations::file::FilePath;

const STAGE_SUBDIR: &str = "apt-repo";

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AptRepoOperation {
    /// Create `/etc/apt/keyrings` (mode 0755) on the target. Idempotent -
    /// `install -d` is a no-op when the directory already exists.
    EnsureKeyringsDir { path: FilePath },

    /// Stream `url` into a user-writable cache, then `sudo install` it to `path`
    /// with mode 0644 so apt can read it under any sudo umask.
    DownloadKey {
        name: String,
        url: String,
        path: FilePath,
    },

    /// Stage `content` to a user-writable cache, then `sudo install` it to
    /// `path` with mode 0644.
    WriteSources {
        name: String,
        path: FilePath,
        content: String,
    },
}

impl Display for AptRepoOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AptRepoOperation::EnsureKeyringsDir { path } => {
                write!(f, "AptRepo::EnsureKeyringsDir(path = {path})")
            }
            AptRepoOperation::DownloadKey { name, url, path } => write!(
                f,
                "AptRepo::DownloadKey(name = {name}, url = {url}, path = {path})"
            ),
            AptRepoOperation::WriteSources {
                name,
                path,
                content,
            } => write!(
                f,
                "AptRepo::WriteSources(name = {name}, path = {path}, {} bytes)",
                content.len()
            ),
        }
    }
}

#[derive(Error, Debug)]
pub enum AptRepoApplyError {
    #[error(transparent)]
    Command(#[from] CommandError),

    #[error(transparent)]
    Fs(#[from] FsError),

    #[error(transparent)]
    Http(#[from] HttpError),
}

#[derive(Debug, Clone)]
pub struct AptRepo;

// Note(cc): `merge()` is a no-op for v1 - see the parallel comment in
// `git.rs`. Two apt-repo resources in one epoch will both emit
// `EnsureKeyringsDir { path: /etc/apt/keyrings }`, but `install -d` is
// already idempotent so the duplicate is just a wasted sudo round-trip.
// Worth deduping by path if it ever shows up in profiles.
#[async_trait]
impl OperationType for AptRepo {
    type Operation = AptRepoOperation;

    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation> {
        operations
    }

    type ApplyOutput = Pin<Box<dyn Future<Output = Result<(), Self::ApplyError>> + Send + 'static>>;
    type ApplyError = AptRepoApplyError;
    type ApplyStdout = ChildStdout;
    type ApplyStderr = ChildStderr;

    async fn apply(
        ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError> {
        match operation {
            AptRepoOperation::EnsureKeyringsDir { path } => {
                info!(path = %path, "[apt-repo] ensure keyrings dir");
                let mut cmd = Command::new("install");
                cmd.arg("-d").arg("-m").arg("0755").arg(path.as_path());
                let output = cmd.sudo().output().await?;
                Ok((
                    Box::pin(async move {
                        output.status.await?;
                        Ok(())
                    }),
                    output.stdout,
                    output.stderr,
                ))
            }
            AptRepoOperation::DownloadKey { name, url, path } => {
                info!(name = %name, url = %url, path = %path, "[apt-repo] download key");

                let stage_path = stage_path_for(ctx, name, "asc").await?;
                // HttpClient::download_file no-ops if the destination already
                // exists; a stale stage from a prior failed run would silently
                // be reused, so clear it first.
                if fs::path_exists(&stage_path).await? {
                    fs::remove_file(&stage_path).await?;
                }
                ctx.http_client().download_file(url, &stage_path).await?;

                let mut cmd = Command::new("install");
                cmd.arg("-m")
                    .arg("0644")
                    .arg(&stage_path)
                    .arg(path.as_path());
                let output = cmd.sudo().output().await?;
                let cleanup_path = stage_path.clone();
                Ok((
                    Box::pin(async move {
                        let result = output.status.await;
                        // Best-effort cleanup of the cache stage even if the
                        // sudo install failed - the stage is in a user-owned
                        // XDG dir and never references state another op needs.
                        let _ = tokio::fs::remove_file(&cleanup_path).await;
                        result?;
                        Ok(())
                    }),
                    output.stdout,
                    output.stderr,
                ))
            }
            AptRepoOperation::WriteSources {
                name,
                path,
                content,
            } => {
                info!(name = %name, path = %path, "[apt-repo] write sources");

                let stage_path = stage_path_for(ctx, name, "sources").await?;
                fs::write_file_atomic(&stage_path, content.as_bytes()).await?;

                let mut cmd = Command::new("install");
                cmd.arg("-m")
                    .arg("0644")
                    .arg(&stage_path)
                    .arg(path.as_path());
                let output = cmd.sudo().output().await?;
                let cleanup_path = stage_path.clone();
                Ok((
                    Box::pin(async move {
                        let result = output.status.await;
                        let _ = tokio::fs::remove_file(&cleanup_path).await;
                        result?;
                        Ok(())
                    }),
                    output.stdout,
                    output.stderr,
                ))
            }
        }
    }
}

async fn stage_path_for(
    ctx: &Context,
    name: &str,
    extension: &str,
) -> Result<PathBuf, AptRepoApplyError> {
    let stage_dir = ctx.paths().cache_dir().join(STAGE_SUBDIR);
    fs::create_dir(&stage_dir).await?;
    Ok(stage_dir.join(format!("{name}.{extension}")))
}

#[cfg(test)]
mod serde_tests {
    use super::*;

    fn round_trip(op: AptRepoOperation) {
        let json = serde_json::to_string(&op).unwrap();
        let back: AptRepoOperation = serde_json::from_str(&json).unwrap();
        assert_eq!(json, serde_json::to_string(&back).unwrap());
    }

    #[test]
    fn round_trip_ensure_keyrings_dir() {
        round_trip(AptRepoOperation::EnsureKeyringsDir {
            path: FilePath::new("/etc/apt/keyrings"),
        });
    }

    #[test]
    fn round_trip_download_key() {
        round_trip(AptRepoOperation::DownloadKey {
            name: "docker".into(),
            url: "https://download.docker.com/linux/debian/gpg".into(),
            path: FilePath::new("/etc/apt/keyrings/docker.asc"),
        });
    }

    #[test]
    fn round_trip_write_sources() {
        round_trip(AptRepoOperation::WriteSources {
            name: "docker".into(),
            path: FilePath::new("/etc/apt/sources.list.d/docker.sources"),
            content: "Types: deb\nURIs: https://download.docker.com/linux/debian\n".into(),
        });
    }
}
