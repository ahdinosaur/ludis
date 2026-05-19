use async_trait::async_trait;
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use serde::{Deserialize, Serialize};
use std::{fmt::Display, pin::Pin};
use thiserror::Error;
use tokio::process::{ChildStderr, ChildStdout};
use tracing::info;

use crate::OperationType;

use crate::operations::file::FilePath;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GitOperation {
    Clone {
        repo: String,
        path: FilePath,
    },
    Fetch {
        path: FilePath,
    },
    Checkout {
        path: FilePath,
        version: String,
        force: bool,
    },
    Pull {
        path: FilePath,
    },
}

impl Display for GitOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GitOperation::Clone { repo, path } => {
                write!(f, "Git::Clone(repo = {}, path = {})", repo, path)
            }
            GitOperation::Fetch { path } => write!(f, "Git::Fetch(path = {})", path),
            GitOperation::Checkout {
                path,
                version,
                force,
            } => write!(
                f,
                "Git::Checkout(path = {}, version = {}, force = {})",
                path, version, force
            ),
            GitOperation::Pull { path } => write!(f, "Git::Pull(path = {})", path),
        }
    }
}

#[derive(Error, Debug)]
pub enum GitApplyError {
    #[error(transparent)]
    Command(#[from] CommandError),
}

#[derive(Debug, Clone)]
pub struct Git;

#[async_trait]
impl OperationType for Git {
    type Operation = GitOperation;

    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation> {
        operations
    }

    type ApplyOutput = Pin<Box<dyn Future<Output = Result<(), Self::ApplyError>> + Send + 'static>>;
    type ApplyError = GitApplyError;
    type ApplyStdout = ChildStdout;
    type ApplyStderr = ChildStderr;

    async fn apply(
        _ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError> {
        match operation {
            GitOperation::Clone { repo, path } => {
                info!("[git] clone: {} -> {}", repo, path);
                let mut cmd = Command::new("git");
                cmd.arg("clone").arg(repo).arg(path.as_path());
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
            GitOperation::Fetch { path } => {
                info!("[git] fetch: {}", path);
                let mut cmd = Command::new("git");
                cmd.arg("-C")
                    .arg(path.as_path())
                    .args(["fetch", "--all", "--prune"]);
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
            GitOperation::Checkout {
                path,
                version,
                force,
            } => {
                info!("[git] checkout: {} -> {}", path, version);
                let mut cmd = Command::new("git");
                cmd.arg("-C").arg(path.as_path()).arg("checkout");
                if *force {
                    cmd.arg("-f");
                }
                cmd.arg(version);
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
            GitOperation::Pull { path } => {
                info!("[git] pull: {}", path);
                let mut cmd = Command::new("git");
                cmd.arg("-C")
                    .arg(path.as_path())
                    .args(["pull", "--ff-only"]);
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
        }
    }
}
