//! `@core/ufw` — declarative management of the Uncomplicated Firewall.
//!
//! Plans declare the desired enabled state, default policies for each
//! direction, and a list of rules. The resource probes ufw's current state
//! (`ufw status verbose` for enabled+defaults, `ufw show added` for rule list)
//! and emits operations to converge.
//!
//! Apply order — picked to avoid locking out an active firewall while it is
//! being reconciled:
//! 1. `SetDefault` for each direction (cheap; idempotent when already set).
//! 2. Add rules declared in the plan but missing from the running config.
//! 3. Delete rules present in the running config but no longer declared.
//! 4. Toggle enable / disable last.
//!
//! When transitioning from disabled to enabled the new rules are installed
//! before the firewall comes up, so a user who declares `allow 22/tcp`
//! alongside `incoming: deny` keeps SSH open through the transition.

use std::collections::HashSet;
use std::fmt::Display;

use async_trait::async_trait;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_operation::{
    Operation,
    operations::ufw::{
        UfwAction, UfwDefaultDirection, UfwDirection, UfwOperation, UfwPolicy, UfwProtocol, UfwRule,
    },
};
use lusid_params::{ParseError, ParseParams, StructFields, parse_list, parse_string};
use lusid_view::impl_display_render;
use rimu::{Spanned, Value};
use thiserror::Error;

use crate::ResourceType;

#[derive(Debug, Clone)]
pub struct UfwParams {
    pub enabled: Option<bool>,
    pub incoming: Option<UfwPolicy>,
    pub outgoing: Option<UfwPolicy>,
    /// `None` means "leave the routed default alone" — not "set to disabled".
    /// ufw has no clean way to clear a routed default short of `ufw reset`,
    /// so we never try.
    pub routed: Option<UfwPolicy>,
    pub rules: Option<Vec<UfwRule>>,
}

impl ParseParams for UfwParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let enabled = fields.optional_bool("enabled")?;
        let incoming = fields.optional("incoming", parse_policy)?;
        let outgoing = fields.optional("outgoing", parse_policy)?;
        let routed = fields.optional("routed", parse_policy)?;
        let rules = fields.optional("rules", |value| parse_list(value, parse_rule_value))?;
        fields.finish()?;
        Ok(UfwParams {
            enabled,
            incoming,
            outgoing,
            routed,
            rules,
        })
    }
}

impl Display for UfwParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            enabled,
            incoming,
            outgoing,
            routed,
            rules,
        } = self;
        let rule_count = rules.as_ref().map(|r| r.len()).unwrap_or(0);
        write!(
            f,
            "Ufw(enabled = {enabled:?}, incoming = {incoming:?}, outgoing = {outgoing:?}, routed = {routed:?}, rules = {rule_count})"
        )
    }
}

impl_display_render!(UfwParams);

#[derive(Debug, Clone)]
pub struct UfwResource {
    pub enabled: bool,
    pub incoming: UfwPolicy,
    pub outgoing: UfwPolicy,
    pub routed: Option<UfwPolicy>,
    pub rules: Vec<UfwRule>,
}

impl Display for UfwResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            enabled,
            incoming,
            outgoing,
            routed,
            rules,
        } = self;
        write!(
            f,
            "Ufw(enabled = {enabled}, incoming = {incoming}, outgoing = {outgoing}, routed = {routed:?}, rules = {})",
            rules.len()
        )
    }
}

impl_display_render!(UfwResource);

/// Observed defaults block from `ufw status verbose`.
///
/// `routed` is optional because ufw renders `disabled (routed)` when no
/// routed default policy is in effect — distinct from "couldn't read".
#[derive(Debug, Clone)]
pub struct UfwDefaults {
    pub incoming: UfwPolicy,
    pub outgoing: UfwPolicy,
    pub routed: Option<UfwPolicy>,
}

#[derive(Debug, Clone)]
pub struct UfwState {
    pub enabled: bool,
    /// `None` means we could not read the defaults from `ufw status verbose`
    /// (typical for an inactive firewall on older ufw versions, which only
    /// emits `Status: inactive`). The change computation treats this as
    /// "always emit SetDefault" — harmless because ufw's default-setting
    /// command is idempotent.
    pub defaults: Option<UfwDefaults>,
    pub rules: Vec<UfwRule>,
}

impl Display for UfwState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            enabled,
            defaults,
            rules,
        } = self;
        write!(
            f,
            "Ufw(enabled = {enabled}, defaults = {defaults:?}, rules = {})",
            rules.len()
        )
    }
}

impl_display_render!(UfwState);

#[derive(Error, Debug)]
pub enum UfwStateError {
    #[error(transparent)]
    Command(#[from] CommandError),

    #[error("unknown ufw policy: {policy}")]
    UnknownPolicy { policy: String },

    #[error("unknown ufw action: {action}")]
    UnknownAction { action: String },

    #[error("unknown ufw protocol: {proto}")]
    UnknownProto { proto: String },

    #[error("failed to parse port `{value}`: {source}")]
    ParsePort {
        value: String,
        #[source]
        source: std::num::ParseIntError,
    },

    #[error("failed to tokenise ufw rule line `{line}`: {source}")]
    TokenizeRule {
        line: String,
        #[source]
        source: shell_words::ParseError,
    },

    #[error("ufw rule line truncated: missing value for keyword `{keyword}` in `{line}`")]
    TruncatedRule { keyword: &'static str, line: String },

    #[error("unrecognised token `{token}` in ufw rule `{line}`")]
    UnknownToken { token: String, line: String },
}

#[derive(Debug, Clone)]
pub struct UfwChange {
    pub enable: Option<bool>,
    pub incoming: Option<UfwPolicy>,
    pub outgoing: Option<UfwPolicy>,
    pub routed: Option<UfwPolicy>,
    pub add: Vec<UfwRule>,
    pub remove: Vec<UfwRule>,
}

impl Display for UfwChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            enable,
            incoming,
            outgoing,
            routed,
            add,
            remove,
        } = self;
        let mut parts: Vec<String> = Vec::new();
        if let Some(b) = enable {
            parts.push(format!("enable = {b}"));
        }
        if let Some(p) = incoming {
            parts.push(format!("incoming = {p}"));
        }
        if let Some(p) = outgoing {
            parts.push(format!("outgoing = {p}"));
        }
        if let Some(p) = routed {
            parts.push(format!("routed = {p}"));
        }
        if !add.is_empty() {
            parts.push(format!("add = {}", add.len()));
        }
        if !remove.is_empty() {
            parts.push(format!("remove = {}", remove.len()));
        }
        write!(f, "Ufw::Change({})", parts.join(", "))
    }
}

impl_display_render!(UfwChange);

#[derive(Debug, Clone)]
pub struct Ufw;

#[async_trait]
impl ResourceType for Ufw {
    const ID: &'static str = "ufw";

    type Params = UfwParams;
    type Resource = UfwResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        let resource = UfwResource {
            enabled: params.enabled.unwrap_or(true),
            incoming: params.incoming.unwrap_or(UfwPolicy::Deny),
            outgoing: params.outgoing.unwrap_or(UfwPolicy::Allow),
            routed: params.routed,
            rules: params
                .rules
                .unwrap_or_default()
                .into_iter()
                .map(UfwRule::canonical)
                .collect(),
        };
        vec![CausalityTree::leaf(CausalityMeta::default(), resource)]
    }

    type State = UfwState;
    type StateError = UfwStateError;

    async fn state(
        _ctx: &mut Context,
        _resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        // `ufw status verbose` requires root. `Command::sudo()` is non-interactive (`-n`),
        // so this fails fast if passwordless sudo isn't set up for the operator.
        let mut status_cmd = Command::new("ufw");
        status_cmd.arg("status").arg("verbose");
        let status_output = status_cmd.sudo().run().await?;
        let status_text = String::from_utf8_lossy(&status_output);
        let (enabled, defaults) = parse_status_verbose(&status_text)?;

        let mut added_cmd = Command::new("ufw");
        added_cmd.arg("show").arg("added");
        let added_output = added_cmd.sudo().run().await?;
        let added_text = String::from_utf8_lossy(&added_output);
        let rules = parse_show_added(&added_text)?;

        Ok(UfwState {
            enabled,
            defaults,
            rules,
        })
    }

    type Change = UfwChange;

    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        let enable = (resource.enabled != state.enabled).then_some(resource.enabled);

        let (incoming, outgoing, routed) = match &state.defaults {
            None => (
                Some(resource.incoming),
                Some(resource.outgoing),
                resource.routed,
            ),
            Some(state_defaults) => {
                let inc =
                    (resource.incoming != state_defaults.incoming).then_some(resource.incoming);
                let out =
                    (resource.outgoing != state_defaults.outgoing).then_some(resource.outgoing);
                let routed_change = match (resource.routed, state_defaults.routed) {
                    (None, _) => None,
                    (Some(r), Some(s)) if r == s => None,
                    (Some(r), _) => Some(r),
                };
                (inc, out, routed_change)
            }
        };

        let state_rule_set: HashSet<&UfwRule> = state.rules.iter().collect();
        let resource_rule_set: HashSet<&UfwRule> = resource.rules.iter().collect();
        let add: Vec<UfwRule> = resource
            .rules
            .iter()
            .filter(|rule| !state_rule_set.contains(*rule))
            .cloned()
            .collect();
        let remove: Vec<UfwRule> = state
            .rules
            .iter()
            .filter(|rule| !resource_rule_set.contains(*rule))
            .cloned()
            .collect();

        if enable.is_none()
            && incoming.is_none()
            && outgoing.is_none()
            && routed.is_none()
            && add.is_empty()
            && remove.is_empty()
        {
            None
        } else {
            Some(UfwChange {
                enable,
                incoming,
                outgoing,
                routed,
                add,
                remove,
            })
        }
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        let UfwChange {
            enable,
            incoming,
            outgoing,
            routed,
            add,
            remove,
        } = change;

        let default_ops: Vec<_> = [
            incoming.map(|policy| (UfwDefaultDirection::Incoming, policy)),
            outgoing.map(|policy| (UfwDefaultDirection::Outgoing, policy)),
            routed.map(|policy| (UfwDefaultDirection::Routed, policy)),
        ]
        .into_iter()
        .flatten()
        .map(|(direction, policy)| {
            CausalityTree::leaf(
                CausalityMeta::default(),
                Operation::Ufw(UfwOperation::SetDefault { direction, policy }),
            )
        })
        .collect();

        let add_ops: Vec<_> = add
            .into_iter()
            .map(|rule| {
                CausalityTree::leaf(
                    CausalityMeta::default(),
                    Operation::Ufw(UfwOperation::AddRule(rule)),
                )
            })
            .collect();

        let remove_ops: Vec<_> = remove
            .into_iter()
            .map(|rule| {
                CausalityTree::leaf(
                    CausalityMeta::default(),
                    Operation::Ufw(UfwOperation::DeleteRule(rule)),
                )
            })
            .collect();

        // Each phase is a named branch. Branch ids are scoped to this resource
        // invocation (see `plan::tree::map_plan_subitems`), so the literal
        // strings here can't collide with another ufw resource's ids.
        // Empty branches are legal — `compute_epochs` records the id with no
        // leaves and dependents simply get no edges to it.
        let defaults_branch =
            CausalityTree::branch(CausalityMeta::id("defaults".into()), default_ops);
        let add_branch = CausalityTree::branch(
            CausalityMeta {
                id: Some("add-rules".into()),
                requires: vec!["defaults".into()],
                required_by: vec![],
            },
            add_ops,
        );
        let remove_branch = CausalityTree::branch(
            CausalityMeta {
                id: Some("delete-rules".into()),
                requires: vec!["add-rules".into()],
                required_by: vec![],
            },
            remove_ops,
        );

        let mut out = vec![defaults_branch, add_branch, remove_branch];

        if let Some(enable) = enable {
            let op = if enable {
                UfwOperation::Enable
            } else {
                UfwOperation::Disable
            };
            out.push(CausalityTree::leaf(
                CausalityMeta {
                    id: None,
                    requires: vec!["delete-rules".into()],
                    required_by: vec![],
                },
                Operation::Ufw(op),
            ));
        }

        out
    }
}

// ---------- params parsing helpers ----------

fn parse_policy(value: Spanned<Value>) -> Result<UfwPolicy, Spanned<ParseError>> {
    let span = value.span().clone();
    let s = parse_string(value)?;
    match s.as_str() {
        "allow" => Ok(UfwPolicy::Allow),
        "deny" => Ok(UfwPolicy::Deny),
        "reject" => Ok(UfwPolicy::Reject),
        _ => Err(Spanned::new(
            ParseError::TypeMismatch {
                expected: "ufw policy (allow|deny|reject)",
                got: Box::new(Value::String(s)),
            },
            span,
        )),
    }
}

fn parse_action(value: Spanned<Value>) -> Result<UfwAction, Spanned<ParseError>> {
    let span = value.span().clone();
    let s = parse_string(value)?;
    match s.as_str() {
        "allow" => Ok(UfwAction::Allow),
        "deny" => Ok(UfwAction::Deny),
        "reject" => Ok(UfwAction::Reject),
        "limit" => Ok(UfwAction::Limit),
        _ => Err(Spanned::new(
            ParseError::TypeMismatch {
                expected: "ufw action (allow|deny|reject|limit)",
                got: Box::new(Value::String(s)),
            },
            span,
        )),
    }
}

fn parse_proto_field(value: Spanned<Value>) -> Result<UfwProtocol, Spanned<ParseError>> {
    let span = value.span().clone();
    let s = parse_string(value)?;
    match s.as_str() {
        "tcp" => Ok(UfwProtocol::Tcp),
        "udp" => Ok(UfwProtocol::Udp),
        _ => Err(Spanned::new(
            ParseError::TypeMismatch {
                expected: "ufw protocol (tcp|udp)",
                got: Box::new(Value::String(s)),
            },
            span,
        )),
    }
}

fn parse_direction(value: Spanned<Value>) -> Result<UfwDirection, Spanned<ParseError>> {
    let span = value.span().clone();
    let s = parse_string(value)?;
    match s.as_str() {
        "in" => Ok(UfwDirection::In),
        "out" => Ok(UfwDirection::Out),
        _ => Err(Spanned::new(
            ParseError::TypeMismatch {
                expected: "ufw direction (in|out)",
                got: Box::new(Value::String(s)),
            },
            span,
        )),
    }
}

fn parse_rule_value(value: Spanned<Value>) -> Result<UfwRule, Spanned<ParseError>> {
    let mut fields = StructFields::new(value)?;
    let action = fields.required("action", parse_action)?;
    let direction = fields
        .optional("direction", parse_direction)?
        .unwrap_or(UfwDirection::In);
    let from = fields.optional_string("from")?;
    let from_port = fields.optional_u32("from_port")?;
    let to = fields.optional_string("to")?;
    let to_port = fields.optional_u32("port")?;
    let proto = fields.optional("proto", parse_proto_field)?;
    let comment = fields.optional_string("comment")?;
    fields.finish()?;
    Ok(UfwRule {
        action,
        direction,
        from,
        from_port,
        to,
        to_port,
        proto,
        comment,
    }
    .canonical())
}

// ---------- state output parsing ----------

/// Parse `ufw status verbose` output into `(enabled, defaults)`.
///
/// The format is loosely:
/// ```text
/// Status: active
/// Logging: on (low)
/// Default: deny (incoming), allow (outgoing), disabled (routed)
/// ...
/// ```
///
/// When `Status: inactive`, the Default line is omitted on older ufw versions
/// and `defaults` returns `None`.
fn parse_status_verbose(text: &str) -> Result<(bool, Option<UfwDefaults>), UfwStateError> {
    let mut enabled = false;
    let mut defaults_line: Option<&str> = None;

    for line in text.lines() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("Status:") {
            enabled = rest.trim() == "active";
        } else if let Some(rest) = trimmed.strip_prefix("Default:") {
            defaults_line = Some(rest.trim());
        }
    }

    let defaults = match defaults_line {
        None => None,
        Some(line) => Some(parse_defaults_line(line)?),
    };
    Ok((enabled, defaults))
}

/// Parse one `Default:` line body, e.g. `deny (incoming), allow (outgoing),
/// disabled (routed)`. Order isn't guaranteed; we pick out each direction by
/// its parenthesised tag.
fn parse_defaults_line(line: &str) -> Result<UfwDefaults, UfwStateError> {
    // ufw may omit one or more directions on older versions; default to "deny" /
    // "allow" / disabled to match ufw's compiled-in defaults so a partial line
    // doesn't fail.
    let mut incoming = UfwPolicy::Deny;
    let mut outgoing = UfwPolicy::Allow;
    let mut routed: Option<UfwPolicy> = None;

    for part in line.split(',') {
        let part = part.trim();
        let Some((word, rest)) = part.split_once(' ') else {
            continue;
        };
        let direction = rest.trim().trim_start_matches('(').trim_end_matches(')');
        let policy_opt = parse_default_policy_word(word)?;
        match direction {
            "incoming" => {
                if let Some(p) = policy_opt {
                    incoming = p;
                }
            }
            "outgoing" => {
                if let Some(p) = policy_opt {
                    outgoing = p;
                }
            }
            "routed" => routed = policy_opt,
            _ => {}
        }
    }

    Ok(UfwDefaults {
        incoming,
        outgoing,
        routed,
    })
}

/// Map one of ufw's default-policy words to a [`UfwPolicy`]. `disabled` is
/// distinct from a missing policy: it only appears in the `routed` slot and
/// means "no routed default in effect".
fn parse_default_policy_word(word: &str) -> Result<Option<UfwPolicy>, UfwStateError> {
    match word {
        "allow" => Ok(Some(UfwPolicy::Allow)),
        "deny" => Ok(Some(UfwPolicy::Deny)),
        "reject" => Ok(Some(UfwPolicy::Reject)),
        "disabled" => Ok(None),
        other => Err(UfwStateError::UnknownPolicy {
            policy: other.to_string(),
        }),
    }
}

/// Parse `ufw show added` output into a list of canonicalised rules.
///
/// The output looks like:
/// ```text
/// Added user rules (see 'ufw status' for running firewall):
///
/// ufw allow 22/tcp
/// ufw allow from 10.0.0.0/8 to any port 80 proto tcp comment 'web'
/// ```
///
/// With no rules: a `(None)` placeholder is emitted instead of rule lines.
fn parse_show_added(text: &str) -> Result<Vec<UfwRule>, UfwStateError> {
    let mut rules: Vec<UfwRule> = Vec::new();
    for line in text.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        if line.starts_with("Added user rules") {
            continue;
        }
        if line == "(None)" {
            continue;
        }
        // Defensive: lines that aren't actually `ufw ...` directives are skipped.
        // This guards against stray annotations a future ufw might add to the
        // output.
        let Some(rest) = line.strip_prefix("ufw ") else {
            continue;
        };
        let tokens = shell_words::split(rest).map_err(|source| UfwStateError::TokenizeRule {
            line: line.to_string(),
            source,
        })?;
        if tokens.is_empty() {
            continue;
        }
        let action = match tokens[0].as_str() {
            "allow" => UfwAction::Allow,
            "deny" => UfwAction::Deny,
            "reject" => UfwAction::Reject,
            "limit" => UfwAction::Limit,
            other => {
                return Err(UfwStateError::UnknownAction {
                    action: other.to_string(),
                });
            }
        };
        rules.push(parse_rule_args(action, &tokens[1..], line)?.canonical());
    }
    Ok(rules)
}

/// Tokenised rule body (everything after the action verb). Recognises:
/// - leading direction (`in` / `out`)
/// - shorthand port form (`22` or `22/tcp`)
/// - keyword pairs (`from`, `to`, `port`, `proto`, `comment`)
fn parse_rule_args(
    action: UfwAction,
    tokens: &[String],
    raw_line: &str,
) -> Result<UfwRule, UfwStateError> {
    let mut direction = UfwDirection::In;
    let mut from: Option<String> = None;
    let mut from_port: Option<u32> = None;
    let mut to: Option<String> = None;
    let mut to_port: Option<u32> = None;
    let mut proto: Option<UfwProtocol> = None;
    let mut comment: Option<String> = None;

    let mut i = 0;

    // Optional explicit direction.
    if let Some(token) = tokens.first() {
        match token.as_str() {
            "in" => {
                direction = UfwDirection::In;
                i = 1;
            }
            "out" => {
                direction = UfwDirection::Out;
                i = 1;
            }
            _ => {}
        }
    }

    // Shorthand port form: `22` or `22/tcp` as a single token. Triggers only
    // when the token starts with a digit, so we don't misinterpret IP addresses
    // (which can't appear here because in the shorthand form `from`/`to`
    // keywords haven't been seen yet).
    if let Some(token) = tokens.get(i)
        && token
            .chars()
            .next()
            .map(|c| c.is_ascii_digit())
            .unwrap_or(false)
        && !token.contains('.')
        && !token.contains(':')
    {
        if let Some((port_str, proto_str)) = token.split_once('/') {
            let port = port_str
                .parse::<u32>()
                .map_err(|source| UfwStateError::ParsePort {
                    value: port_str.to_string(),
                    source,
                })?;
            to_port = Some(port);
            proto = Some(parse_proto_word(proto_str)?);
        } else {
            let port = token
                .parse::<u32>()
                .map_err(|source| UfwStateError::ParsePort {
                    value: token.clone(),
                    source,
                })?;
            to_port = Some(port);
        }
        i += 1;
    }

    // Keyword args. `port` attaches to whichever of `from`/`to` was named most
    // recently — ufw's own grammar. Until a `from` or `to` is seen, a bare
    // `port` keyword attaches to the destination (`to_port`).
    enum LastClause {
        None,
        From,
        To,
    }
    let mut last = LastClause::None;

    while i < tokens.len() {
        let keyword = &tokens[i];
        match keyword.as_str() {
            "from" => {
                let value =
                    tokens
                        .get(i + 1)
                        .cloned()
                        .ok_or_else(|| UfwStateError::TruncatedRule {
                            keyword: "from",
                            line: raw_line.to_string(),
                        })?;
                from = Some(value);
                last = LastClause::From;
                i += 2;
            }
            "to" => {
                let value =
                    tokens
                        .get(i + 1)
                        .cloned()
                        .ok_or_else(|| UfwStateError::TruncatedRule {
                            keyword: "to",
                            line: raw_line.to_string(),
                        })?;
                to = Some(value);
                last = LastClause::To;
                i += 2;
            }
            "port" => {
                let value = tokens
                    .get(i + 1)
                    .ok_or_else(|| UfwStateError::TruncatedRule {
                        keyword: "port",
                        line: raw_line.to_string(),
                    })?;
                let port = value
                    .parse::<u32>()
                    .map_err(|source| UfwStateError::ParsePort {
                        value: value.clone(),
                        source,
                    })?;
                match last {
                    LastClause::From => from_port = Some(port),
                    LastClause::To | LastClause::None => to_port = Some(port),
                }
                i += 2;
            }
            "proto" => {
                let value = tokens
                    .get(i + 1)
                    .ok_or_else(|| UfwStateError::TruncatedRule {
                        keyword: "proto",
                        line: raw_line.to_string(),
                    })?;
                proto = Some(parse_proto_word(value)?);
                i += 2;
            }
            "comment" => {
                let value =
                    tokens
                        .get(i + 1)
                        .cloned()
                        .ok_or_else(|| UfwStateError::TruncatedRule {
                            keyword: "comment",
                            line: raw_line.to_string(),
                        })?;
                comment = Some(value);
                i += 2;
            }
            other => {
                return Err(UfwStateError::UnknownToken {
                    token: other.to_string(),
                    line: raw_line.to_string(),
                });
            }
        }
    }

    Ok(UfwRule {
        action,
        direction,
        from,
        from_port,
        to,
        to_port,
        proto,
        comment,
    })
}

fn parse_proto_word(word: &str) -> Result<UfwProtocol, UfwStateError> {
    match word {
        "tcp" => Ok(UfwProtocol::Tcp),
        "udp" => Ok(UfwProtocol::Udp),
        other => Err(UfwStateError::UnknownProto {
            proto: other.to_string(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_status_active_with_defaults() {
        let text = "\
Status: active
Logging: on (low)
Default: deny (incoming), allow (outgoing), disabled (routed)
New profiles: skip
";
        let (enabled, defaults) = parse_status_verbose(text).expect("ok");
        assert!(enabled);
        let defaults = defaults.expect("defaults present");
        assert_eq!(defaults.incoming, UfwPolicy::Deny);
        assert_eq!(defaults.outgoing, UfwPolicy::Allow);
        assert!(defaults.routed.is_none());
    }

    #[test]
    fn parses_status_inactive_omits_defaults() {
        let (enabled, defaults) = parse_status_verbose("Status: inactive\n").expect("ok");
        assert!(!enabled);
        assert!(defaults.is_none());
    }

    #[test]
    fn parses_routed_policy_when_set() {
        let text = "\
Status: active
Default: deny (incoming), allow (outgoing), reject (routed)
";
        let (_, defaults) = parse_status_verbose(text).expect("ok");
        let defaults = defaults.expect("present");
        assert_eq!(defaults.routed, Some(UfwPolicy::Reject));
    }

    #[test]
    fn parses_simple_port_rule() {
        let text = "\
Added user rules (see 'ufw status' for running firewall):

ufw allow 22/tcp
";
        let rules = parse_show_added(text).expect("ok");
        assert_eq!(rules.len(), 1);
        let r = &rules[0];
        assert_eq!(r.action, UfwAction::Allow);
        assert_eq!(r.direction, UfwDirection::In);
        assert_eq!(r.to_port, Some(22));
        assert_eq!(r.proto, Some(UfwProtocol::Tcp));
        assert!(r.from.is_none());
        assert!(r.to.is_none());
    }

    #[test]
    fn parses_full_form_rule_with_comment() {
        let text = "\
Added user rules (see 'ufw status' for running firewall):

ufw allow from 10.0.0.0/8 to any port 80 proto tcp comment 'web'
";
        let rules = parse_show_added(text).expect("ok");
        assert_eq!(rules.len(), 1);
        let r = &rules[0];
        assert_eq!(r.from.as_deref(), Some("10.0.0.0/8"));
        // "any" collapsed to None by canonicalisation.
        assert!(r.to.is_none());
        assert_eq!(r.to_port, Some(80));
        assert_eq!(r.proto, Some(UfwProtocol::Tcp));
        assert_eq!(r.comment.as_deref(), Some("web"));
    }

    #[test]
    fn parses_none_marker_as_empty_rule_set() {
        let text = "\
Added user rules (see 'ufw status' for running firewall):

(None)
";
        let rules = parse_show_added(text).expect("ok");
        assert!(rules.is_empty());
    }

    #[test]
    fn from_port_attaches_to_from_clause() {
        let text = "\
Added user rules:

ufw allow from any port 5000 to any port 80 proto tcp
";
        let rules = parse_show_added(text).expect("ok");
        let r = &rules[0];
        assert_eq!(r.from_port, Some(5000));
        assert_eq!(r.to_port, Some(80));
    }

    #[test]
    fn unknown_keyword_in_rule_errors() {
        let text = "ufw allow magic 22";
        let mut wrapped = String::from("Added user rules:\n\n");
        wrapped.push_str(text);
        let err = parse_show_added(&wrapped).unwrap_err();
        assert!(matches!(err, UfwStateError::UnknownToken { .. }));
    }

    #[test]
    fn truncated_keyword_errors() {
        let text = "Added user rules:\n\nufw allow from";
        let err = parse_show_added(text).unwrap_err();
        assert!(matches!(
            err,
            UfwStateError::TruncatedRule {
                keyword: "from",
                ..
            }
        ));
    }

    /// Round-trip: rendering a rule and re-parsing the result yields the same
    /// canonical rule. Guarantees that whatever we add via `ufw allow ...`
    /// will be found again next time we probe state.
    #[test]
    fn render_roundtrip_simple_port() {
        let rule = UfwRule {
            action: UfwAction::Allow,
            direction: UfwDirection::In,
            from: None,
            from_port: None,
            to: None,
            to_port: Some(22),
            proto: Some(UfwProtocol::Tcp),
            comment: None,
        }
        .canonical();
        let line = format!("ufw {rule}");
        let mut wrapped = String::from("Added user rules:\n\n");
        wrapped.push_str(&line);
        let parsed = &parse_show_added(&wrapped).expect("ok")[0];
        assert_eq!(parsed, &rule);
    }

    #[test]
    fn render_roundtrip_full_form() {
        let rule = UfwRule {
            action: UfwAction::Allow,
            direction: UfwDirection::In,
            from: Some("192.168.1.0/24".into()),
            from_port: None,
            to: None,
            to_port: Some(80),
            proto: Some(UfwProtocol::Tcp),
            comment: Some("web".into()),
        }
        .canonical();
        let line = format!("ufw {rule}");
        let mut wrapped = String::from("Added user rules:\n\n");
        wrapped.push_str(&line);
        let parsed = &parse_show_added(&wrapped).expect("ok")[0];
        assert_eq!(parsed, &rule);
    }
}
