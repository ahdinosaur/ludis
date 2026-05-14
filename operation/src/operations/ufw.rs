use async_trait::async_trait;
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_view::impl_display_render;
use std::{fmt::Display, pin::Pin};
use thiserror::Error;
use tokio::process::{ChildStderr, ChildStdout};
use tracing::info;

use crate::OperationType;

/// Verbs that can prefix a single ufw rule (`ufw allow 22`, `ufw deny ...`, etc.).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UfwAction {
    Allow,
    Deny,
    Reject,
    Limit,
}

impl UfwAction {
    pub fn as_str(self) -> &'static str {
        match self {
            UfwAction::Allow => "allow",
            UfwAction::Deny => "deny",
            UfwAction::Reject => "reject",
            UfwAction::Limit => "limit",
        }
    }
}

impl Display for UfwAction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Allowed values for `ufw default <policy>`. Note: `Limit` is a per-rule verb
/// only — it is not a valid default policy and is intentionally absent here.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UfwPolicy {
    Allow,
    Deny,
    Reject,
}

impl UfwPolicy {
    pub fn as_str(self) -> &'static str {
        match self {
            UfwPolicy::Allow => "allow",
            UfwPolicy::Deny => "deny",
            UfwPolicy::Reject => "reject",
        }
    }
}

impl Display for UfwPolicy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// The three default-policy "directions" ufw supports.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UfwDefaultDirection {
    Incoming,
    Outgoing,
    Routed,
}

impl UfwDefaultDirection {
    pub fn as_str(self) -> &'static str {
        match self {
            UfwDefaultDirection::Incoming => "incoming",
            UfwDefaultDirection::Outgoing => "outgoing",
            UfwDefaultDirection::Routed => "routed",
        }
    }
}

impl Display for UfwDefaultDirection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Per-rule direction. Defaults to `In` if omitted in a plan.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UfwDirection {
    In,
    Out,
}

impl UfwDirection {
    pub fn as_str(self) -> &'static str {
        match self {
            UfwDirection::In => "in",
            UfwDirection::Out => "out",
        }
    }
}

impl Display for UfwDirection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UfwProtocol {
    Tcp,
    Udp,
}

impl UfwProtocol {
    pub fn as_str(self) -> &'static str {
        match self {
            UfwProtocol::Tcp => "tcp",
            UfwProtocol::Udp => "udp",
        }
    }
}

impl Display for UfwProtocol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// One ufw firewall rule.
///
/// The fields mirror the full `ufw allow [in|out] [from FROM [port FROM_PORT]] [to
/// TO [port TO_PORT]] [proto PROTO] [comment 'C']` syntax. Empty values mean
/// "any" (which ufw treats as the absence of a constraint), so [`UfwRule::Eq`]
/// works as set membership: two rules are the same rule iff every field matches
/// after canonicalisation.
///
/// Canonical form: `from`/`to` strings of `"any"` or `"anywhere"` are normalised
/// to `None`; `direction` defaults to [`UfwDirection::In`]. See
/// [`UfwRule::canonical`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UfwRule {
    pub action: UfwAction,
    pub direction: UfwDirection,
    pub from: Option<String>,
    pub from_port: Option<u32>,
    pub to: Option<String>,
    pub to_port: Option<u32>,
    pub proto: Option<UfwProtocol>,
    pub comment: Option<String>,
}

impl UfwRule {
    /// Collapse equivalent surface forms into one canonical representation so
    /// `derive(PartialEq, Eq, Hash)` treats them as the same rule:
    ///
    /// - `from`/`to` of `"any"` or `"anywhere"` (case-insensitive) → `None`.
    /// - Empty `from`/`to` strings → `None`.
    ///
    /// Comments are kept verbatim. The comment is treated as part of the rule's
    /// identity because ufw's CLI does — `ufw delete allow 22/tcp` will not
    /// match a rule added with a different comment, since comments form part
    /// of the rule signature in the iptables backend.
    pub fn canonical(mut self) -> Self {
        fn normalise(s: Option<String>) -> Option<String> {
            let s = s?;
            if s.is_empty() {
                return None;
            }
            let lower = s.to_ascii_lowercase();
            if lower == "any" || lower == "anywhere" {
                None
            } else {
                Some(s)
            }
        }
        self.from = normalise(self.from);
        self.to = normalise(self.to);
        self
    }

    /// Render the rule as the trailing argv to `ufw allow|deny|reject|limit ...`
    /// (or to `ufw delete allow|deny|reject|limit ...`). The leading action
    /// verb is *not* included — the caller adds it.
    ///
    /// Whenever possible the short form is emitted (`22/tcp`) so the resulting
    /// rule line matches what plans typically write.
    fn argv_after_action(&self) -> Vec<String> {
        let mut args: Vec<String> = Vec::new();

        // The compact form `ufw <action> <port>[/proto]` only applies when the
        // rule has no source/dest/from-port and the direction is the default.
        let is_simple_port = self.direction == UfwDirection::In
            && self.from.is_none()
            && self.from_port.is_none()
            && self.to.is_none()
            && self.to_port.is_some();

        if is_simple_port {
            let port = self.to_port.unwrap();
            if let Some(proto) = self.proto {
                args.push(format!("{port}/{proto}"));
            } else {
                args.push(port.to_string());
            }
        } else {
            // Long form. We always include the direction word so the produced
            // line is unambiguous.
            args.push(self.direction.as_str().to_string());

            if let Some(proto) = self.proto {
                args.push("proto".to_string());
                args.push(proto.as_str().to_string());
            }

            if let Some(from) = &self.from {
                args.push("from".to_string());
                args.push(from.clone());
                if let Some(port) = self.from_port {
                    args.push("port".to_string());
                    args.push(port.to_string());
                }
            } else if self.from_port.is_some() {
                // from_port with no from: treat from as "any" explicitly so
                // ufw accepts the rule.
                args.push("from".to_string());
                args.push("any".to_string());
                args.push("port".to_string());
                args.push(self.from_port.unwrap().to_string());
            }

            if let Some(to) = &self.to {
                args.push("to".to_string());
                args.push(to.clone());
                if let Some(port) = self.to_port {
                    args.push("port".to_string());
                    args.push(port.to_string());
                }
            } else if let Some(port) = self.to_port {
                args.push("to".to_string());
                args.push("any".to_string());
                args.push("port".to_string());
                args.push(port.to_string());
            }
        }

        if let Some(comment) = &self.comment {
            args.push("comment".to_string());
            args.push(comment.clone());
        }

        args
    }
}

impl Display for UfwRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.action)?;
        for arg in self.argv_after_action() {
            write!(f, " {arg}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum UfwOperation {
    Enable,

    Disable,

    SetDefault {
        direction: UfwDefaultDirection,
        policy: UfwPolicy,
    },

    AddRule(UfwRule),

    DeleteRule(UfwRule),
}

impl Display for UfwOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UfwOperation::Enable => write!(f, "Ufw::Enable"),
            UfwOperation::Disable => write!(f, "Ufw::Disable"),
            UfwOperation::SetDefault { direction, policy } => {
                write!(f, "Ufw::SetDefault({direction} = {policy})")
            }
            UfwOperation::AddRule(rule) => write!(f, "Ufw::AddRule({rule})"),
            UfwOperation::DeleteRule(rule) => write!(f, "Ufw::DeleteRule({rule})"),
        }
    }
}

impl_display_render!(UfwOperation);

#[derive(Error, Debug)]
pub enum UfwApplyError {
    #[error(transparent)]
    Command(#[from] CommandError),
}

#[derive(Debug, Clone)]
pub struct Ufw;

#[async_trait]
impl OperationType for Ufw {
    type Operation = UfwOperation;

    // Note(cc): merge is a no-op. ufw's CLI is one verb per invocation; there's
    // no batch form to fold sibling rule additions into, and the per-rule cost
    // (a fork + a small iptables update) is small enough that coalescing
    // wouldn't pay back the complexity.
    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation> {
        operations
    }

    type ApplyOutput = Pin<Box<dyn Future<Output = Result<(), Self::ApplyError>> + Send + 'static>>;
    type ApplyError = UfwApplyError;
    type ApplyStdout = ChildStdout;
    type ApplyStderr = ChildStderr;

    async fn apply(
        _ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError> {
        // `ufw` reads stdin for `enable`/`disable`/`reset` (and prints a "command may
        // disrupt existing ssh connections. Proceed with operation (y|n)?" prompt for
        // enable). `--force` skips every such prompt; harmless on other verbs.
        let mut cmd = Command::new("ufw");
        cmd.arg("--force");

        match operation {
            UfwOperation::Enable => {
                info!("[ufw] enable");
                cmd.arg("enable");
            }
            UfwOperation::Disable => {
                info!("[ufw] disable");
                cmd.arg("disable");
            }
            UfwOperation::SetDefault { direction, policy } => {
                info!(direction = %direction, policy = %policy, "[ufw] default");
                cmd.arg("default")
                    .arg(policy.as_str())
                    .arg(direction.as_str());
            }
            UfwOperation::AddRule(rule) => {
                info!(rule = %rule, "[ufw] add rule");
                cmd.arg(rule.action.as_str()).args(rule.argv_after_action());
            }
            UfwOperation::DeleteRule(rule) => {
                info!(rule = %rule, "[ufw] delete rule");
                cmd.arg("delete")
                    .arg(rule.action.as_str())
                    .args(rule.argv_after_action());
            }
        }

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

#[cfg(test)]
mod tests {
    use super::*;

    fn rule(action: UfwAction, to_port: Option<u32>, proto: Option<UfwProtocol>) -> UfwRule {
        UfwRule {
            action,
            direction: UfwDirection::In,
            from: None,
            from_port: None,
            to: None,
            to_port,
            proto,
            comment: None,
        }
    }

    #[test]
    fn simple_port_renders_short_form() {
        let r = rule(UfwAction::Allow, Some(22), Some(UfwProtocol::Tcp));
        assert_eq!(r.argv_after_action(), vec!["22/tcp"]);
    }

    #[test]
    fn simple_port_no_proto_renders_bare_number() {
        let r = rule(UfwAction::Allow, Some(22), None);
        assert_eq!(r.argv_after_action(), vec!["22"]);
    }

    #[test]
    fn from_renders_long_form_with_direction() {
        let r = UfwRule {
            action: UfwAction::Deny,
            direction: UfwDirection::In,
            from: Some("10.0.0.0/8".into()),
            from_port: None,
            to: None,
            to_port: None,
            proto: None,
            comment: None,
        };
        assert_eq!(r.argv_after_action(), vec!["in", "from", "10.0.0.0/8"]);
    }

    #[test]
    fn full_rule_renders_in_canonical_order() {
        let r = UfwRule {
            action: UfwAction::Allow,
            direction: UfwDirection::In,
            from: Some("192.168.1.0/24".into()),
            from_port: None,
            to: None,
            to_port: Some(80),
            proto: Some(UfwProtocol::Tcp),
            comment: Some("web".into()),
        };
        assert_eq!(
            r.argv_after_action(),
            vec![
                "in",
                "proto",
                "tcp",
                "from",
                "192.168.1.0/24",
                "to",
                "any",
                "port",
                "80",
                "comment",
                "web",
            ]
        );
    }

    #[test]
    fn out_direction_breaks_simple_form() {
        let r = UfwRule {
            action: UfwAction::Allow,
            direction: UfwDirection::Out,
            from: None,
            from_port: None,
            to: None,
            to_port: Some(80),
            proto: Some(UfwProtocol::Tcp),
            comment: None,
        };
        assert_eq!(
            r.argv_after_action(),
            vec!["out", "proto", "tcp", "to", "any", "port", "80"]
        );
    }

    #[test]
    fn canonical_normalises_anywhere_to_none() {
        let r = UfwRule {
            action: UfwAction::Allow,
            direction: UfwDirection::In,
            from: Some("Anywhere".into()),
            from_port: None,
            to: Some("any".into()),
            to_port: Some(22),
            proto: Some(UfwProtocol::Tcp),
            comment: None,
        }
        .canonical();
        assert!(r.from.is_none());
        assert!(r.to.is_none());
    }
}
