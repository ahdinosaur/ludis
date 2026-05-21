use std::{fmt::Display, str::FromStr};

use async_trait::async_trait;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_cmd::{Command as RunCommand, CommandError as RunCommandError};
use lusid_ctx::Context;
use lusid_operation::{
    Operation,
    operations::command::{CommandExecutor, CommandOperation},
};
use lusid_params::{ParseError, ParseParams, StructFields};
use rimu::{Spanned, Value};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::{ChangeKind, ResourceChangeTrait, ResourceType};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CommandParams {
    Install {
        is_installed: Option<String>,
        install: String,
        uninstall: Option<String>,
        /// When set, both the `is_installed` probe and the `install` /
        /// `uninstall` shell-outs run under `sudo -n`. Mirrors the
        /// `sudo: true` opt-in on `@resource/file` /
        /// `@resource/directory` for plans that need to `sed -i` a
        /// root-owned config or `timedatectl set-timezone` without
        /// wrapping every command string by hand.
        ///
        /// Probe asymmetry: `is_installed` runs through `Direct`
        /// (argv-split, no shell), so under sudo it becomes
        /// `sudo -n <prog> <args>`. Plan authors who want shell
        /// features in the probe (`&&`, pipes, globs) must write
        /// `sh -c "..."` themselves. The install/uninstall side runs
        /// through `Shell` and gets `sudo -n sh -c "..."`.
        sudo: bool,
    },
    Uninstall {
        is_installed: Option<String>,
        install: Option<String>,
        uninstall: String,
        /// See [`CommandParams::Install::sudo`].
        sudo: bool,
    },
}

impl ParseParams for CommandParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let status = fields.take_discriminator("status", &["install", "uninstall"])?;
        let out = match status {
            "install" => CommandParams::Install {
                is_installed: fields.optional_string("is_installed")?,
                install: fields.required_string("install")?,
                uninstall: fields.optional_string("uninstall")?,
                sudo: fields.optional_bool("sudo")?.unwrap_or(false),
            },
            "uninstall" => CommandParams::Uninstall {
                is_installed: fields.optional_string("is_installed")?,
                install: fields.optional_string("install")?,
                uninstall: fields.required_string("uninstall")?,
                sudo: fields.optional_bool("sudo")?.unwrap_or(false),
            },
            _ => unreachable!(),
        };
        fields.finish()?;
        Ok(out)
    }
}

impl Display for CommandParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = |sudo: bool| if sudo { "[sudo] " } else { "" };
        match self {
            CommandParams::Install {
                is_installed,
                install,
                uninstall,
                sudo,
            } => {
                write!(
                    f,
                    "{}Command::Install(is_installed = {:?}, install = {}, uninstall = \
                     {:?})",
                    prefix(*sudo),
                    is_installed,
                    install,
                    uninstall
                )
            }
            CommandParams::Uninstall {
                is_installed,
                install,
                uninstall,
                sudo,
            } => {
                write!(
                    f,
                    "{}Command::Uninstall(is_installed = {:?}, install = {:?}, uninstall = \
                     {})",
                    prefix(*sudo),
                    is_installed,
                    install,
                    uninstall
                )
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CommandStatus {
    Install,
    Uninstall,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CommandResource {
    pub status: CommandStatus,
    pub is_installed: Option<String>,
    pub install: Option<String>,
    pub uninstall: Option<String>,
    /// See [`CommandParams::Install::sudo`]. Propagated into the probe
    /// command in `state()` and into the emitted [`CommandOperation`] so
    /// both the `is_installed` check and the install/uninstall shell-out
    /// run under `sudo -n`.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub sudo: bool,
}

impl Display for CommandResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            status,
            is_installed,
            install,
            uninstall,
            sudo,
        } = self;

        let status = match status {
            CommandStatus::Install => "Install",
            CommandStatus::Uninstall => "Uninstall",
        };
        let prefix = if *sudo { "[sudo] " } else { "" };

        write!(
            f,
            "{prefix}Command::{status}(is_installed = {:?}, install = {:?}, uninstall \
             = {:?})",
            is_installed, install, uninstall
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CommandState {
    Installed,
    NotInstalled,
    Unknown,
}

impl Display for CommandState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandState::NotInstalled => write!(f, "Command::NotInstalled"),
            CommandState::Installed => write!(f, "Command::Installed"),
            CommandState::Unknown => write!(f, "Command::Unknown"),
        }
    }
}

#[derive(Error, Debug)]
pub enum CommandStateError {
    #[error(transparent)]
    Command(#[from] RunCommandError),

    #[error("failed to parse command: {0}")]
    ParseCommand(#[source] <RunCommand as FromStr>::Err),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CommandChange {
    Install {
        command: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    Uninstall {
        command: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
}

impl Display for CommandChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = |sudo: bool| if sudo { "[sudo] " } else { "" };
        match self {
            CommandChange::Install { command, sudo } => {
                write!(f, "{}Command::Install({command})", prefix(*sudo))
            }
            CommandChange::Uninstall { command, sudo } => {
                write!(f, "{}Command::Uninstall({command})", prefix(*sudo))
            }
        }
    }
}

impl ResourceChangeTrait for CommandChange {
    fn kind(&self) -> ChangeKind {
        match self {
            CommandChange::Install { .. } => ChangeKind::Added,
            CommandChange::Uninstall { .. } => ChangeKind::Removed,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Command;

#[async_trait]
impl ResourceType for Command {
    const ID: &'static str = "command";

    type Params = CommandParams;
    type Resource = CommandResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        let resource = match params {
            CommandParams::Install {
                is_installed,
                install,
                uninstall,
                sudo,
            } => CommandResource {
                status: CommandStatus::Install,
                is_installed,
                install: Some(install),
                uninstall,
                sudo,
            },
            CommandParams::Uninstall {
                is_installed,
                install,
                uninstall,
                sudo,
            } => CommandResource {
                status: CommandStatus::Uninstall,
                is_installed,
                install,
                uninstall: Some(uninstall),
                sudo,
            },
        };

        vec![CausalityTree::leaf(CausalityMeta::default(), resource)]
    }

    type State = CommandState;
    type StateError = CommandStateError;

    async fn state(
        _ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        let Some(ref is_installed) = resource.is_installed else {
            return Ok(CommandState::Unknown);
        };

        if is_installed.trim().is_empty() {
            return Ok(CommandState::Unknown);
        };

        let cmd = RunCommand::from_str(is_installed).map_err(CommandStateError::ParseCommand)?;
        let mut cmd = if resource.sudo { cmd.sudo() } else { cmd };
        let output = cmd.output().await?;
        let status = output.status.await?;
        let state = if status.success() {
            CommandState::Installed
        } else {
            CommandState::NotInstalled
        };
        Ok(state)
    }

    type Change = CommandChange;

    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        let sudo = resource.sudo;
        match (&resource.status, state) {
            (CommandStatus::Install, CommandState::Installed) => None,
            (CommandStatus::Install, CommandState::NotInstalled) => resource
                .install
                .clone()
                .map(|command| CommandChange::Install { command, sudo }),
            (CommandStatus::Uninstall, CommandState::NotInstalled) => None,
            (CommandStatus::Uninstall, CommandState::Installed) => resource
                .uninstall
                .clone()
                .map(|command| CommandChange::Uninstall { command, sudo }),
            (_, CommandState::Unknown) => None,
        }
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        match change {
            CommandChange::Install { command, sudo }
            | CommandChange::Uninstall { command, sudo } => {
                vec![CausalityTree::leaf(
                    CausalityMeta::default(),
                    Operation::Command(CommandOperation {
                        command,
                        executor: CommandExecutor::Shell,
                        sudo,
                    }),
                )]
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn change_propagates_sudo_into_command_change() {
        let resource = CommandResource {
            status: CommandStatus::Install,
            is_installed: Some("test -f /etc/foo".into()),
            install: Some("touch /etc/foo".into()),
            uninstall: None,
            sudo: true,
        };
        let change =
            Command::change(&resource, &CommandState::NotInstalled).expect("Some change");
        match change {
            CommandChange::Install { sudo, .. } => {
                assert!(sudo, "CommandChange::Install should carry sudo:true")
            }
            other => panic!("expected Install, got {other:?}"),
        }
    }

    #[test]
    fn operations_propagates_sudo_into_command_operation() {
        let change = CommandChange::Install {
            command: "touch /etc/foo".into(),
            sudo: true,
        };
        let ops = Command::operations(change);
        assert_eq!(ops.len(), 1);
        let op = match &ops[0] {
            CausalityTree::Leaf { node, .. } => node,
            _ => panic!("expected leaf"),
        };
        match op {
            Operation::Command(CommandOperation { sudo, .. }) => {
                assert!(*sudo, "CommandOperation should carry sudo:true")
            }
            other => panic!("expected Command, got {other:?}"),
        }
    }

    /// `serde(default)` on `sudo` lets older apply-stdio payloads (pre-sudo
    /// wire) round-trip through the new types as `sudo: false`. Pins the
    /// back-compat contract for `CommandResource` and `CommandChange`; the
    /// contract is the same on every other variant by construction.
    #[test]
    fn command_resource_back_compat_deserializes_missing_sudo_to_false() {
        let json = r#"{"status":"Install","is_installed":null,"install":"touch /tmp/foo","uninstall":null}"#;
        let resource: CommandResource = serde_json::from_str(json).expect("parse old payload");
        assert!(!resource.sudo);
    }

    #[test]
    fn command_change_back_compat_deserializes_missing_sudo_to_false() {
        let json = r#"{"Install":{"command":"touch /tmp/foo"}}"#;
        let change: CommandChange = serde_json::from_str(json).expect("parse old payload");
        match change {
            CommandChange::Install { sudo, .. } => assert!(!sudo),
            other => panic!("expected Install, got {other:?}"),
        }
    }
}
