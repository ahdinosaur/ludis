use async_trait::async_trait;
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use std::{collections::BTreeSet, fmt::Display, pin::Pin};
use thiserror::Error;
use tokio::process::{ChildStderr, ChildStdout};
use tracing::info;

use crate::OperationType;

#[derive(Debug, Clone)]
pub enum PacmanOperation {
    Upgrade,
    Install { packages: Vec<String> },
}

impl Display for PacmanOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PacmanOperation::Upgrade => write!(f, "Pacman::Upgrade"),
            PacmanOperation::Install { packages } => {
                write!(f, "Pacman::Install(packages = [{}])", packages.join(", "))
            }
        }
    }
}

#[derive(Error, Debug)]
pub enum PacmanApplyError {
    #[error(transparent)]
    Command(#[from] CommandError),
}

#[derive(Debug, Clone)]
pub struct Pacman;

#[async_trait]
impl OperationType for Pacman {
    type Operation = PacmanOperation;

    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation> {
        let mut upgrade = false;
        let mut install: BTreeSet<String> = BTreeSet::new();

        for operation in operations {
            match operation {
                PacmanOperation::Upgrade => {
                    upgrade = true;
                }
                PacmanOperation::Install { packages } => {
                    for package in packages {
                        install.insert(package);
                    }
                }
            }
        }

        let mut operations = Vec::new();
        if upgrade {
            operations.push(PacmanOperation::Upgrade);
        }
        if !install.is_empty() {
            operations.push(PacmanOperation::Install {
                packages: install.into_iter().collect(),
            })
        }
        operations
    }

    type ApplyOutput = Pin<Box<dyn Future<Output = Result<(), Self::ApplyError>> + Send + 'static>>;
    type ApplyError = PacmanApplyError;
    type ApplyStdout = ChildStdout;
    type ApplyStderr = ChildStderr;

    async fn apply(
        _ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError> {
        match operation {
            PacmanOperation::Upgrade => {
                info!("[pacman] upgrade");
                let mut cmd = Command::new("pacman");
                cmd.arg("-Syu").arg("--noconfirm").arg("--color=never");
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
            PacmanOperation::Install { packages } => {
                info!("[pacman] install: {}", packages.join(", "));
                let mut cmd = Command::new("pacman");
                cmd.arg("-S")
                    .arg("--noconfirm")
                    .arg("--needed")
                    .arg("--color=never")
                    .arg("--")
                    .args(packages);
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
        }
    }
}
