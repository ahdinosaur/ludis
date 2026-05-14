use async_trait::async_trait;
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_params::{ParseError, ParseParams, StructFields};
use lusid_view::impl_display_render;
use rimu::{Spanned, Value};
use std::{collections::HashSet, fmt::Display, pin::Pin};
use thiserror::Error;
use tokio::process::{ChildStderr, ChildStdout};
use tracing::info;

use crate::OperationType;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SystemdOperation {
    Enable { name: String, user: bool },
    Disable { name: String, user: bool },
    Start { name: String, user: bool },
    Stop { name: String, user: bool },
    Restart { name: String, user: bool },
    Reload { name: String, user: bool },
}

impl Display for SystemdOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (verb, name, user) = match self {
            SystemdOperation::Enable { name, user } => ("Enable", name, user),
            SystemdOperation::Disable { name, user } => ("Disable", name, user),
            SystemdOperation::Start { name, user } => ("Start", name, user),
            SystemdOperation::Stop { name, user } => ("Stop", name, user),
            SystemdOperation::Restart { name, user } => ("Restart", name, user),
            SystemdOperation::Reload { name, user } => ("Reload", name, user),
        };
        let scope = if *user { " --user" } else { "" };
        write!(f, "Systemd::{verb}({name}){scope}")
    }
}

impl_display_render!(SystemdOperation);

impl ParseParams for SystemdOperation {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let action = fields.take_discriminator(
            "action",
            &["enable", "disable", "start", "stop", "restart", "reload"],
        )?;
        let name = fields.required_string("name")?;
        let user = fields.optional_bool("user")?.unwrap_or(false);
        fields.finish()?;
        Ok(match action {
            "enable" => SystemdOperation::Enable { name, user },
            "disable" => SystemdOperation::Disable { name, user },
            "start" => SystemdOperation::Start { name, user },
            "stop" => SystemdOperation::Stop { name, user },
            "restart" => SystemdOperation::Restart { name, user },
            "reload" => SystemdOperation::Reload { name, user },
            _ => unreachable!(),
        })
    }
}

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

    // Dedup identical `(verb, name, user)` ops within an epoch. This is the
    // mechanism by which N inline `on_change: @operation/systemd { action: ... }`
    // ops fanning out from sibling resources collapse to a single invocation
    // (the canonical case: ten config files all reloading nginx → one reload).
    // Safe because enable/disable/start/stop/restart/reload are all idempotent
    // for a given unit within a single apply. Cross-epoch dedup is deliberately
    // not handled here — see AGENTS.md "Install hooks" limitations.
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
            SystemdOperation::Restart { name, user } => ("restart", name, *user),
            SystemdOperation::Reload { name, user } => ("reload", name, *user),
        };
        info!(user, "[systemd] {verb}: {name}");

        let mut cmd = Command::new("systemctl");

        if user {
            cmd.arg("--user");
        }

        // `--no-ask-password` is kept for both buses: on the system bus it prevents
        // sudo/polkit from blocking on a tty prompt; on the user bus it's a no-op
        // because the per-user systemd instance never asks for a password.
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
