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
pub enum UserOperation {
    Add {
        name: String,
        uid: Option<u32>,
        primary_group: Option<String>,
        /// Supplementary groups to set at creation via `useradd -G`. Since the
        /// account doesn't exist yet there's nothing to preserve, so this is
        /// just the initial set.
        append_groups: Vec<String>,
        comment: Option<String>,
        home: Option<FilePath>,
        shell: Option<String>,
        system: bool,
        create_home: bool,
    },
    Modify {
        name: String,
        uid: Option<u32>,
        primary_group: Option<String>,
        /// Supplementary groups to append via `usermod -aG`, leaving any groups
        /// not listed here untouched. `None` skips the group flag entirely.
        append_groups: Option<Vec<String>>,
        comment: Option<String>,
        home: Option<FilePath>,
        shell: Option<String>,
    },
    Delete {
        name: String,
        remove_home: bool,
    },
}

impl Display for UserOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UserOperation::Add { name, .. } => write!(f, "User::Add(name = {name})"),
            UserOperation::Modify { name, .. } => write!(f, "User::Modify(name = {name})"),
            UserOperation::Delete { name, remove_home } => {
                write!(
                    f,
                    "User::Delete(name = {name}, remove_home = {remove_home})"
                )
            }
        }
    }
}

#[derive(Error, Debug)]
pub enum UserApplyError {
    #[error(transparent)]
    Command(#[from] CommandError),
}

#[derive(Debug, Clone)]
pub struct User;

#[async_trait]
impl OperationType for User {
    type Operation = UserOperation;

    // Note(cc): user operations mutate a single named account per call. Merging across
    // operations would require reasoning about ordering of add/modify/delete for the same
    // name - not worth the complexity, so leave each operation standalone.
    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation> {
        operations
    }

    type ApplyOutput = Pin<Box<dyn Future<Output = Result<(), Self::ApplyError>> + Send + 'static>>;
    type ApplyError = UserApplyError;
    type ApplyStdout = ChildStdout;
    type ApplyStderr = ChildStderr;

    async fn apply(
        _ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError> {
        match operation {
            UserOperation::Add {
                name,
                uid,
                primary_group,
                append_groups,
                comment,
                home,
                shell,
                system,
                create_home,
            } => {
                info!("[user] add: {}", name);
                let mut cmd = Command::new("useradd");
                if let Some(uid) = uid {
                    cmd.arg("-u").arg(uid.to_string());
                }
                if let Some(group) = primary_group {
                    cmd.arg("-g").arg(group);
                }
                if !append_groups.is_empty() {
                    cmd.arg("-G").arg(append_groups.join(","));
                }
                if let Some(comment) = comment {
                    cmd.arg("-c").arg(comment);
                }
                if let Some(home) = home {
                    cmd.arg("-d").arg(home.as_path());
                }
                if let Some(shell) = shell {
                    cmd.arg("-s").arg(shell);
                }
                if *system {
                    cmd.arg("-r");
                }
                if *create_home {
                    cmd.arg("-m");
                } else {
                    cmd.arg("-M");
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
            UserOperation::Modify {
                name,
                uid,
                primary_group,
                append_groups,
                comment,
                home,
                shell,
            } => {
                info!("[user] modify: {}", name);
                let mut cmd = Command::new("usermod");
                if let Some(uid) = uid {
                    cmd.arg("-u").arg(uid.to_string());
                }
                if let Some(group) = primary_group {
                    cmd.arg("-g").arg(group);
                }
                if let Some(groups) = append_groups {
                    // `-aG` appends rather than replacing: groups the user is already
                    // a member of are untouched, including ones not listed here.
                    cmd.arg("-aG").arg(groups.join(","));
                }
                if let Some(comment) = comment {
                    cmd.arg("-c").arg(comment);
                }
                if let Some(home) = home {
                    // Note(cc): we set the home path in /etc/passwd but don't pass `-m` to
                    // move the existing home contents - that would touch user data, which is
                    // out of scope for a declarative "this is the home dir" statement.
                    cmd.arg("-d").arg(home.as_path());
                }
                if let Some(shell) = shell {
                    cmd.arg("-s").arg(shell);
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
            UserOperation::Delete { name, remove_home } => {
                info!("[user] delete: {} (remove_home = {})", name, remove_home);
                let mut cmd = Command::new("userdel");
                if *remove_home {
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
        }
    }
}

#[cfg(test)]
mod serde_tests {
    use super::*;

    fn round_trip(op: UserOperation) {
        let json = serde_json::to_string(&op).unwrap();
        let back: UserOperation = serde_json::from_str(&json).unwrap();
        assert_eq!(json, serde_json::to_string(&back).unwrap());
    }

    #[test]
    fn round_trip_add() {
        round_trip(UserOperation::Add {
            name: "alice".into(),
            uid: Some(1001),
            primary_group: Some("alice".into()),
            append_groups: vec!["wheel".into(), "docker".into()],
            comment: Some("Alice".into()),
            home: Some(FilePath::new("/home/alice")),
            shell: Some("/bin/zsh".into()),
            system: false,
            create_home: true,
        });
    }

    #[test]
    fn round_trip_modify_with_none() {
        round_trip(UserOperation::Modify {
            name: "alice".into(),
            uid: None,
            primary_group: None,
            append_groups: None,
            comment: None,
            home: None,
            shell: None,
        });
    }

    #[test]
    fn round_trip_modify_with_some() {
        // The `Some` arm of `append_groups` drives the `usermod -aG` branch in
        // apply; round-trip it so an Option<Vec<_>> shape change can't sneak past.
        round_trip(UserOperation::Modify {
            name: "alice".into(),
            uid: Some(1002),
            primary_group: Some("staff".into()),
            append_groups: Some(vec!["wheel".into(), "docker".into()]),
            comment: Some("Alice".into()),
            home: Some(FilePath::new("/home/alice")),
            shell: Some("/bin/zsh".into()),
        });
    }

    #[test]
    fn round_trip_delete() {
        round_trip(UserOperation::Delete {
            name: "alice".into(),
            remove_home: true,
        });
    }
}
