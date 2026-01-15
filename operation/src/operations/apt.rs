use async_trait::async_trait;
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use std::{collections::BTreeSet, fmt::Display, pin::Pin};
use thiserror::Error;
use tokio::process::{ChildStderr, ChildStdout};
use tracing::info;

use crate::OperationType;

#[derive(Debug, Clone)]
pub enum AptOperation {
    Update,
    Install { packages: Vec<String> },
}

impl Display for AptOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AptOperation::Update => write!(f, "Apt::Update"),
            AptOperation::Install { packages } => {
                write!(f, "Apt::Install(packages = [{}])", packages.join(", "))
            }
        }
    }
}

#[derive(Error, Debug)]
pub enum AptApplyError {
    #[error(transparent)]
    Command(#[from] CommandError),
}

#[derive(Debug, Clone)]
pub struct Apt;

#[async_trait]
impl OperationType for Apt {
    type Operation = AptOperation;

    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation> {
        let mut update = false;
        let mut install: BTreeSet<String> = BTreeSet::new();

        for operation in operations {
            match operation {
                AptOperation::Update => {
                    update = true;
                }
                AptOperation::Install { packages } => {
                    for package in packages {
                        install.insert(package);
                    }
                }
            }
        }

        let mut operations = Vec::new();
        if update {
            operations.push(AptOperation::Update);
        }
        if !install.is_empty() {
            operations.push(AptOperation::Install {
                packages: install.into_iter().collect(),
            })
        }
        operations
    }

    type ApplyOutput = Pin<Box<dyn Future<Output = Result<(), Self::ApplyError>> + Send + 'static>>;
    type ApplyError = AptApplyError;
    type ApplyStdout = ChildStdout;
    type ApplyStderr = ChildStderr;

    async fn apply(
        _ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError> {
        match operation {
            AptOperation::Update => {
                info!("[apt] update");
                let mut cmd = Command::new("apt-get");
                cmd.env("DEBIAN_FRONTEND", "noninteractive").arg("update");
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
            AptOperation::Install { packages } => {
                info!("[apt] install: {}", packages.join(", "));
                let mut cmd = Command::new("apt-get");
                cmd.env("DEBIAN_FRONTEND", "noninteractive")
                    .arg("install")
                    .arg("-y")
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
