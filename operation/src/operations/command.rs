use async_trait::async_trait;
use lusid_cmd::{Command as RunCommand, CommandError as RunCommandError};
use lusid_ctx::Context;
use lusid_params::{ParseError, ParseParams, StructFields};
use rimu::{Spanned, Value};
use serde::{Deserialize, Serialize};
use std::{collections::HashSet, fmt::Display, pin::Pin, str::FromStr};
use thiserror::Error;
use tokio::process::{ChildStderr, ChildStdout};
use tracing::info;

use crate::OperationType;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum CommandExecutor {
    Direct,
    Shell,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct CommandOperation {
    pub command: String,
    pub executor: CommandExecutor,
}

impl ParseParams for CommandOperation {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let command = fields.required_string("command")?;
        let executor = fields
            .optional("executor", parse_executor)?
            .unwrap_or(CommandExecutor::Shell);
        fields.finish()?;
        Ok(CommandOperation { command, executor })
    }
}

fn parse_executor(value: Spanned<Value>) -> Result<CommandExecutor, Spanned<ParseError>> {
    let (inner, span) = value.take();
    let Value::String(got) = inner else {
        return Err(Spanned::new(
            ParseError::TypeMismatch {
                expected: "string (\"shell\" or \"direct\")",
                got: Box::new(inner),
            },
            span,
        ));
    };
    match got.as_str() {
        "shell" => Ok(CommandExecutor::Shell),
        "direct" => Ok(CommandExecutor::Direct),
        _ => Err(Spanned::new(
            ParseError::UnknownDiscriminator {
                key: "executor",
                got: Box::new(Value::String(got)),
                expected: vec!["shell", "direct"],
            },
            span,
        )),
    }
}

impl Display for CommandOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let CommandOperation { command, .. } = self;
        write!(f, "Command({command})")
    }
}

#[derive(Error, Debug)]
pub enum CommandApplyError {
    #[error("failed to parse command: {0}")]
    ParseCommand(#[source] <RunCommand as FromStr>::Err),

    #[error(transparent)]
    RunCommand(#[from] RunCommandError),
}

#[derive(Debug, Clone)]
pub struct Command;

#[async_trait]
impl OperationType for Command {
    type Operation = CommandOperation;

    // Dedup identical `(command, executor)` ops within an epoch. This is the
    // mechanism by which N inline `on_change: @operation/command` ops fanning
    // out from sibling resources collapse to a single execution. Order is
    // preserved across distinct ops (first-seen wins for duplicates).
    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation> {
        let mut seen = HashSet::new();
        let mut out = Vec::with_capacity(operations.len());
        for op in operations {
            if seen.insert(op.clone()) {
                out.push(op);
            }
        }
        out
    }

    type ApplyOutput = Pin<Box<dyn Future<Output = Result<(), Self::ApplyError>> + Send + 'static>>;
    type ApplyError = CommandApplyError;
    type ApplyStdout = ChildStdout;
    type ApplyStderr = ChildStderr;

    async fn apply(
        _ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError> {
        let CommandOperation { command, executor } = operation;
        info!("[command] run: {command}");

        let mut cmd = match executor {
            CommandExecutor::Direct => {
                RunCommand::from_str(command).map_err(CommandApplyError::ParseCommand)
            }
            CommandExecutor::Shell => Ok(RunCommand::new_sh(command)),
        }?;
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn merge_dedups_identical_commands() {
        let op = CommandOperation {
            command: "echo hi".to_string(),
            executor: CommandExecutor::Shell,
        };
        let out = Command::merge(vec![op.clone(), op.clone(), op]);
        assert_eq!(out.len(), 1);
    }

    #[test]
    fn merge_distinguishes_executor() {
        // Same command string under different executors are different ops.
        let shell = CommandOperation {
            command: "ls".to_string(),
            executor: CommandExecutor::Shell,
        };
        let direct = CommandOperation {
            command: "ls".to_string(),
            executor: CommandExecutor::Direct,
        };
        assert_eq!(Command::merge(vec![shell, direct]).len(), 2);
    }
}

#[cfg(test)]
mod serde_tests {
    use super::*;

    #[test]
    fn round_trip_both_executors() {
        for executor in [CommandExecutor::Direct, CommandExecutor::Shell] {
            let op = CommandOperation {
                command: "ls -la".into(),
                executor,
            };
            let json = serde_json::to_string(&op).unwrap();
            let back: CommandOperation = serde_json::from_str(&json).unwrap();
            assert_eq!(op, back);
        }
    }

    #[test]
    fn executor_serializes_lowercase() {
        assert_eq!(
            serde_json::to_string(&CommandExecutor::Direct).unwrap(),
            "\"direct\""
        );
        assert_eq!(
            serde_json::to_string(&CommandExecutor::Shell).unwrap(),
            "\"shell\""
        );
    }
}
