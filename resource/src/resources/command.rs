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
use lusid_view::impl_display_render;
use rimu::{Spanned, Value};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::ResourceType;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CommandParams {
    Install {
        is_installed: Option<String>,
        install: String,
        uninstall: Option<String>,
    },
    Uninstall {
        is_installed: Option<String>,
        install: Option<String>,
        uninstall: String,
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
            },
            "uninstall" => CommandParams::Uninstall {
                is_installed: fields.optional_string("is_installed")?,
                install: fields.optional_string("install")?,
                uninstall: fields.required_string("uninstall")?,
            },
            _ => unreachable!(),
        };
        fields.finish()?;
        Ok(out)
    }
}

impl Display for CommandParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandParams::Install {
                is_installed,
                install,
                uninstall,
            } => {
                write!(
                    f,
                    "Command::Install(is_installed = {:?}, install = {}, uninstall = \
                     {:?})",
                    is_installed, install, uninstall
                )
            }
            CommandParams::Uninstall {
                is_installed,
                install,
                uninstall,
            } => {
                write!(
                    f,
                    "Command::Uninstall(is_installed = {:?}, install = {:?}, uninstall = \
                     {})",
                    is_installed, install, uninstall
                )
            }
        }
    }
}

impl_display_render!(CommandParams);

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
}

impl Display for CommandResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            status,
            is_installed,
            install,
            uninstall,
        } = self;

        let status = match status {
            CommandStatus::Install => "Install",
            CommandStatus::Uninstall => "Uninstall",
        };

        write!(
            f,
            "Command::{status}(is_installed = {:?}, install = {:?}, uninstall \
             = {:?})",
            is_installed, install, uninstall
        )
    }
}

impl_display_render!(CommandResource);

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

impl_display_render!(CommandState);

#[derive(Error, Debug)]
pub enum CommandStateError {
    #[error(transparent)]
    Command(#[from] RunCommandError),

    #[error("failed to parse command: {0}")]
    ParseCommand(#[source] <RunCommand as FromStr>::Err),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CommandChange {
    Install { command: String },
    Uninstall { command: String },
}

impl Display for CommandChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandChange::Install { command } => write!(f, "Command::Install({command})"),
            CommandChange::Uninstall { command } => write!(f, "Command::Uninstall({command})"),
        }
    }
}

impl_display_render!(CommandChange);

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
            } => CommandResource {
                status: CommandStatus::Install,
                is_installed,
                install: Some(install),
                uninstall,
            },
            CommandParams::Uninstall {
                is_installed,
                install,
                uninstall,
            } => CommandResource {
                status: CommandStatus::Uninstall,
                is_installed,
                install,
                uninstall: Some(uninstall),
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

        let mut cmd =
            RunCommand::from_str(is_installed).map_err(CommandStateError::ParseCommand)?;
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
        match (&resource.status, state) {
            (CommandStatus::Install, CommandState::Installed) => None,
            (CommandStatus::Install, CommandState::NotInstalled) => resource
                .install
                .clone()
                .map(|command| CommandChange::Install { command }),
            (CommandStatus::Uninstall, CommandState::NotInstalled) => None,
            (CommandStatus::Uninstall, CommandState::Installed) => resource
                .uninstall
                .clone()
                .map(|command| CommandChange::Uninstall { command }),
            (_, CommandState::Unknown) => None,
        }
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        match change {
            CommandChange::Install { command } | CommandChange::Uninstall { command } => {
                vec![CausalityTree::leaf(
                    CausalityMeta::default(),
                    Operation::Command(CommandOperation {
                        command,
                        executor: CommandExecutor::Shell,
                    }),
                )]
            }
        }
    }
}

#[cfg(test)]
mod serde_tests {
    use super::*;

    fn round_trip<T: serde::Serialize + serde::de::DeserializeOwned>(value: &T) {
        let json = serde_json::to_string(value).unwrap();
        let back: T = serde_json::from_str(&json).unwrap();
        assert_eq!(json, serde_json::to_string(&back).unwrap());
    }

    #[test]
    fn params_round_trip_covers_every_variant() {
        round_trip(&CommandParams::Install {
            is_installed: Some("test -f /usr/local/bin/foo".into()),
            install: "install-foo".into(),
            uninstall: Some("uninstall-foo".into()),
        });
        round_trip(&CommandParams::Uninstall {
            is_installed: Some("test -f /usr/local/bin/foo".into()),
            install: Some("install-foo".into()),
            uninstall: "uninstall-foo".into(),
        });
    }

    #[test]
    fn status_round_trip_covers_every_variant() {
        round_trip(&CommandStatus::Install);
        round_trip(&CommandStatus::Uninstall);
    }

    #[test]
    fn resource_round_trip() {
        round_trip(&CommandResource {
            status: CommandStatus::Install,
            is_installed: Some("test -f /usr/local/bin/foo".into()),
            install: Some("install-foo".into()),
            uninstall: Some("uninstall-foo".into()),
        });
    }

    #[test]
    fn state_round_trip_covers_every_variant() {
        round_trip(&CommandState::Installed);
        round_trip(&CommandState::NotInstalled);
        round_trip(&CommandState::Unknown);
    }

    #[test]
    fn change_round_trip_covers_every_variant() {
        round_trip(&CommandChange::Install {
            command: "install-foo".into(),
        });
        round_trip(&CommandChange::Uninstall {
            command: "uninstall-foo".into(),
        });
    }
}
