use async_trait::async_trait;
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use serde::{Deserialize, Serialize};
use std::{fmt::Display, pin::Pin};
use thiserror::Error;
use tokio::process::{ChildStderr, ChildStdout};
use tracing::info;

use crate::OperationType;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GroupOperation {
    Add {
        name: String,
        gid: Option<u32>,
        system: bool,
    },
    Modify {
        name: String,
        gid: Option<u32>,
    },
    /// Append a single user as a supplementary member of `name`.
    ///
    /// Uses `gpasswd -a`, which appends without touching other members. Users
    /// whose *primary* group is this one (set via `/etc/passwd`) are unaffected
    /// - `gpasswd` only edits `/etc/group`.
    AddUser {
        name: String,
        user: String,
    },
    Delete {
        name: String,
    },
}

impl Display for GroupOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GroupOperation::Add { name, .. } => write!(f, "Group::Add(name = {name})"),
            GroupOperation::Modify { name, .. } => write!(f, "Group::Modify(name = {name})"),
            GroupOperation::AddUser { name, user } => {
                write!(f, "Group::AddUser(name = {name}, user = {user})")
            }
            GroupOperation::Delete { name } => write!(f, "Group::Delete(name = {name})"),
        }
    }
}

#[derive(Error, Debug)]
pub enum GroupApplyError {
    #[error(transparent)]
    Command(#[from] CommandError),
}

#[derive(Debug, Clone)]
pub struct Group;

#[async_trait]
impl OperationType for Group {
    type Operation = GroupOperation;

    // Note(cc): group operations mutate a single named group per call. As with
    // `UserOperation::merge`, ordering of add/modify/delete/add-user for the
    // same name would need to be preserved; not worth coalescing.
    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation> {
        operations
    }

    type ApplyOutput = Pin<Box<dyn Future<Output = Result<(), Self::ApplyError>> + Send + 'static>>;
    type ApplyError = GroupApplyError;
    type ApplyStdout = ChildStdout;
    type ApplyStderr = ChildStderr;

    async fn apply(
        _ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError> {
        match operation {
            GroupOperation::Add { name, gid, system } => {
                info!("[group] add: {}", name);
                let mut cmd = Command::new("groupadd");
                if let Some(gid) = gid {
                    cmd.arg("-g").arg(gid.to_string());
                }
                if *system {
                    cmd.arg("-r");
                }
                cmd.arg("--").arg(name);
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
            GroupOperation::Modify { name, gid } => {
                info!("[group] modify: {}", name);
                let mut cmd = Command::new("groupmod");
                if let Some(gid) = gid {
                    cmd.arg("-g").arg(gid.to_string());
                }
                cmd.arg("--").arg(name);
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
            GroupOperation::AddUser { name, user } => {
                info!("[group] add user: {} <- {}", name, user);
                let mut cmd = Command::new("gpasswd");
                cmd.arg("-a").arg(user).arg("--").arg(name);
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
            GroupOperation::Delete { name } => {
                info!("[group] delete: {}", name);
                let mut cmd = Command::new("groupdel");
                cmd.arg("--").arg(name);
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
