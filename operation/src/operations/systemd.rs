use async_trait::async_trait;
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_view::impl_display_render;
use std::{fmt::Display, pin::Pin};
use thiserror::Error;
use tokio::process::{ChildStderr, ChildStdout};
use tracing::info;

use crate::OperationType;

#[derive(Debug, Clone)]
pub enum SystemdOperation {
    Enable { name: String, user: bool },
    Disable { name: String, user: bool },
    Start { name: String, user: bool },
    Stop { name: String, user: bool },
}

impl Display for SystemdOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (verb, name, user) = match self {
            SystemdOperation::Enable { name, user } => ("Enable", name, user),
            SystemdOperation::Disable { name, user } => ("Disable", name, user),
            SystemdOperation::Start { name, user } => ("Start", name, user),
            SystemdOperation::Stop { name, user } => ("Stop", name, user),
        };
        let scope = if *user { " --user" } else { "" };
        write!(f, "Systemd::{verb}({name}){scope}")
    }
}

impl_display_render!(SystemdOperation);

#[derive(Error, Debug)]
pub enum SystemdApplyError {
    #[error(transparent)]
    Command(#[from] CommandError),
}

#[derive(Debug, Clone)]
pub struct Systemd;

#[async_trait]
impl OperationType for Systemd {
    type Operation = SystemdOperation;

    // Note(cc): merge is a no-op. `systemctl enable|start` accepts multiple units but
    // the operations here are per-verb-per-unit; coalescing would save at most a fork
    // per unit, which isn't worth the extra complexity while plans manage a handful
    // of units at a time. Revisit if plans start listing dozens of systemd units.
    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation> {
        operations
    }

    type ApplyOutput = Pin<Box<dyn Future<Output = Result<(), Self::ApplyError>> + Send + 'static>>;
    type ApplyError = SystemdApplyError;
    type ApplyStdout = ChildStdout;
    type ApplyStderr = ChildStderr;

    async fn apply(
        _ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError> {
        let (verb, name, user) = match operation {
            SystemdOperation::Enable { name, user } => ("enable", name, *user),
            SystemdOperation::Disable { name, user } => ("disable", name, *user),
            SystemdOperation::Start { name, user } => ("start", name, *user),
            SystemdOperation::Stop { name, user } => ("stop", name, *user),
        };
        info!(user, "[systemd] {verb}: {name}");

        // `--no-ask-password` is kept for both buses: on the system bus it prevents
        // sudo/polkit from blocking on a tty prompt; on the user bus it's a no-op
        // because the per-user systemd instance never asks for a password.
        let mut cmd = Command::new("systemctl");
        if user {
            cmd.arg("--user");
        }
        cmd.arg("--no-ask-password").arg(verb).arg(name);
        // User-instance commands talk to `$XDG_RUNTIME_DIR/systemd/private` as the
        // invoking user — wrapping in `sudo` would target root's user instance (or
        // fail entirely without a session bus), which is the opposite of what we want.
        if !user {
            cmd = cmd.sudo();
        }
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
