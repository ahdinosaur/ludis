//! `@core/ufw_rule` — one ufw firewall rule per resource atom, applied
//! additively.
//!
//! Each plan item produces a single rule (allow / deny / reject / limit, plus
//! optional direction / from / to / port / proto / comment). State probing
//! reads `ufw show added` and looks for a rule whose canonical form matches;
//! a missing rule becomes a single `AddRule` operation. Rules are never
//! deleted — removing the resource from the plan leaves the rule in place
//! until the operator cleans up manually with `ufw delete …`.
//!
//! ## Why additive
//!
//! Multiple `@core/ufw_rule` resources can declare different rules without
//! fighting: each only adds its own rule, and `ufw allow …` is idempotent
//! ("Skipping adding existing rule"). A diff-and-remove design would require
//! every rule resource to know every other rule in the plan, which doesn't
//! compose. The trade-off is that removing a rule from a plan does not
//! retract it from the running firewall — declare a new plan, drop the
//! rule, and clean up by hand.

use std::fmt::Display;

use async_trait::async_trait;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_operation::{
    Operation,
    operations::ufw::{UfwAction, UfwDirection, UfwOperation, UfwProtocol, UfwRule as UfwRuleSpec},
};
use lusid_params::{ParseError, ParseParams, StructFields, parse_string};
use lusid_view::impl_display_render;
use rimu::{Spanned, Value};
use thiserror::Error;

use crate::ResourceType;

#[derive(Debug, Clone)]
pub struct UfwRuleParams {
    pub action: UfwAction,
    pub direction: Option<UfwDirection>,
    pub from: Option<String>,
    pub from_port: Option<u32>,
    pub to: Option<String>,
    pub port: Option<u32>,
    pub proto: Option<UfwProtocol>,
    pub comment: Option<String>,
}

impl ParseParams for UfwRuleParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let action = fields.required("action", parse_action)?;
        let direction = fields.optional("direction", parse_direction)?;
        let from = fields.optional_string("from")?;
        let from_port = fields.optional_u32("from_port")?;
        let to = fields.optional_string("to")?;
        let port = fields.optional_u32("port")?;
        let proto = fields.optional("proto", parse_proto_field)?;
        let comment = fields.optional_string("comment")?;
        fields.finish()?;
        Ok(UfwRuleParams {
            action,
            direction,
            from,
            from_port,
            to,
            port,
            proto,
            comment,
        })
    }
}

impl Display for UfwRuleParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UfwRule({})", build_spec_from_params(self))
    }
}

impl_display_render!(UfwRuleParams);

#[derive(Debug, Clone)]
pub struct UfwRuleResource {
    pub spec: UfwRuleSpec,
}

impl Display for UfwRuleResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UfwRule({})", self.spec)
    }
}

impl_display_render!(UfwRuleResource);

#[derive(Debug, Clone)]
pub enum UfwRuleState {
    Present,
    Absent,
}

impl Display for UfwRuleState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UfwRuleState::Present => write!(f, "UfwRule::Present"),
            UfwRuleState::Absent => write!(f, "UfwRule::Absent"),
        }
    }
}

impl_display_render!(UfwRuleState);

#[derive(Error, Debug)]
pub enum UfwRuleStateError {
    #[error(transparent)]
    Command(#[from] CommandError),

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
pub struct UfwRuleChange {
    pub spec: UfwRuleSpec,
}

impl Display for UfwRuleChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UfwRule::Add({})", self.spec)
    }
}

impl_display_render!(UfwRuleChange);

#[derive(Debug, Clone)]
pub struct UfwRule;

#[async_trait]
impl ResourceType for UfwRule {
    const ID: &'static str = "ufw_rule";

    type Params = UfwRuleParams;
    type Resource = UfwRuleResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        let spec = build_spec_from_params(&params);
        vec![CausalityTree::leaf(
            CausalityMeta::default(),
            UfwRuleResource { spec },
        )]
    }

    type State = UfwRuleState;
    type StateError = UfwRuleStateError;

    async fn state(
        _ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        // Each rule atom re-runs `ufw show added`. Multiple `@core/ufw_rule`
        // resources therefore make N invocations of the same command. ufw
        // itself is cheap — a fork + a small file read — and lusid's state
        // probe phase is sequential per leaf, so coalescing was not worth
        // the cross-resource caching layer it would require.
        let mut cmd = Command::new("ufw");
        cmd.arg("show").arg("added");
        let output = cmd.sudo().run().await?;
        let text = String::from_utf8_lossy(&output);
        let rules = parse_show_added(&text)?;
        if rules.iter().any(|r| r == &resource.spec) {
            Ok(UfwRuleState::Present)
        } else {
            Ok(UfwRuleState::Absent)
        }
    }

    type Change = UfwRuleChange;

    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        match state {
            UfwRuleState::Present => None,
            UfwRuleState::Absent => Some(UfwRuleChange {
                spec: resource.spec.clone(),
            }),
        }
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        vec![CausalityTree::leaf(
            CausalityMeta::default(),
            Operation::Ufw(UfwOperation::AddRule(change.spec)),
        )]
    }
}

/// Lower user-facing params into the canonical operation-layer rule spec.
fn build_spec_from_params(params: &UfwRuleParams) -> UfwRuleSpec {
    UfwRuleSpec {
        action: params.action,
        direction: params.direction.unwrap_or(UfwDirection::In),
        from: params.from.clone(),
        from_port: params.from_port,
        to: params.to.clone(),
        to_port: params.port,
        proto: params.proto,
        comment: params.comment.clone(),
    }
    .canonical()
}

// ---------- field parsers ----------

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

// ---------- `ufw show added` parsing ----------

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
fn parse_show_added(text: &str) -> Result<Vec<UfwRuleSpec>, UfwRuleStateError> {
    let mut rules: Vec<UfwRuleSpec> = Vec::new();
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
        let tokens =
            shell_words::split(rest).map_err(|source| UfwRuleStateError::TokenizeRule {
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
                return Err(UfwRuleStateError::UnknownAction {
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
) -> Result<UfwRuleSpec, UfwRuleStateError> {
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
                .map_err(|source| UfwRuleStateError::ParsePort {
                    value: port_str.to_string(),
                    source,
                })?;
            to_port = Some(port);
            proto = Some(parse_proto_word(proto_str)?);
        } else {
            let port = token
                .parse::<u32>()
                .map_err(|source| UfwRuleStateError::ParsePort {
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
                        .ok_or_else(|| UfwRuleStateError::TruncatedRule {
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
                        .ok_or_else(|| UfwRuleStateError::TruncatedRule {
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
                    .ok_or_else(|| UfwRuleStateError::TruncatedRule {
                        keyword: "port",
                        line: raw_line.to_string(),
                    })?;
                let port = value
                    .parse::<u32>()
                    .map_err(|source| UfwRuleStateError::ParsePort {
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
                    .ok_or_else(|| UfwRuleStateError::TruncatedRule {
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
                        .ok_or_else(|| UfwRuleStateError::TruncatedRule {
                            keyword: "comment",
                            line: raw_line.to_string(),
                        })?;
                comment = Some(value);
                i += 2;
            }
            other => {
                return Err(UfwRuleStateError::UnknownToken {
                    token: other.to_string(),
                    line: raw_line.to_string(),
                });
            }
        }
    }

    Ok(UfwRuleSpec {
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

fn parse_proto_word(word: &str) -> Result<UfwProtocol, UfwRuleStateError> {
    match word {
        "tcp" => Ok(UfwProtocol::Tcp),
        "udp" => Ok(UfwProtocol::Udp),
        other => Err(UfwRuleStateError::UnknownProto {
            proto: other.to_string(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        assert!(matches!(err, UfwRuleStateError::UnknownToken { .. }));
    }

    #[test]
    fn truncated_keyword_errors() {
        let text = "Added user rules:\n\nufw allow from";
        let err = parse_show_added(text).unwrap_err();
        assert!(matches!(
            err,
            UfwRuleStateError::TruncatedRule {
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
        let rule = UfwRuleSpec {
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
        let rule = UfwRuleSpec {
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

    /// `change()` returns `Add` when the rule isn't already in the firewall.
    #[test]
    fn change_returns_add_when_absent() {
        let resource = UfwRuleResource {
            spec: UfwRuleSpec {
                action: UfwAction::Allow,
                direction: UfwDirection::In,
                from: None,
                from_port: None,
                to: None,
                to_port: Some(22),
                proto: Some(UfwProtocol::Tcp),
                comment: None,
            },
        };
        let change = UfwRule::change(&resource, &UfwRuleState::Absent).expect("some");
        assert_eq!(change.spec.to_port, Some(22));
    }

    /// Already present: no change, no operation. This is the additive
    /// semantics — repeated applies are no-ops.
    #[test]
    fn change_returns_none_when_present() {
        let resource = UfwRuleResource {
            spec: UfwRuleSpec {
                action: UfwAction::Allow,
                direction: UfwDirection::In,
                from: None,
                from_port: None,
                to: None,
                to_port: Some(22),
                proto: Some(UfwProtocol::Tcp),
                comment: None,
            },
        };
        assert!(UfwRule::change(&resource, &UfwRuleState::Present).is_none());
    }
}
