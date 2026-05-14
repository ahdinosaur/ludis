//! `@core/ufw` — singleton management of the ufw firewall's lifecycle.
//!
//! This resource owns three things:
//! - whether the firewall is enabled,
//! - the default policy for each direction (incoming / outgoing / routed).
//!
//! It does **not** manage individual rules — those live in [`@core/ufw-rule`]
//! ([`crate::resources::ufw_rule`]), one rule per resource atom, applied
//! additively. The split exists because the firewall's rule list is a single
//! global mutable structure: a single `@core/ufw` per machine declaring "these
//! are all the rules" would force every service plan to know every other
//! service's rules, and N independent `@core/ufw` resources each claiming
//! full ownership would thrash each other on every apply (each one would
//! compute "remove anything not in my list"). Splitting lets services declare
//! just the rules they need without fighting.
//!
//! Plans should declare **at most one** `@core/ufw` per machine. Multiple
//! `@core/ufw-rule` resources can — and typically should — coexist.
//!
//! State probe reads `ufw status verbose` for the enabled flag and the
//! default policies. Operations emitted: `SetDefault` per direction
//! sequenced before `Enable` / `Disable`, so a fresh-install transition
//! reaches the firewall with the right defaults in place rather than ufw's
//! compiled-in ones.
//!
//! ## Ordering with `@core/ufw-rule`
//!
//! By default lusid does not order `@core/ufw-rule` operations against
//! this resource's `Enable` operation. On a fresh install where `enabled`
//! transitions from false → true with `incoming: deny`, an unlucky merge
//! order can run `Enable` before the rule that allows SSH, dropping the
//! operator's session. To avoid that, plan authors should declare an
//! explicit `requires:` from this resource to the relevant ufw_rule ids,
//! e.g.
//!
//! ```yaml
//! - module: "@core/ufw-rule"
//!   id: ufw-allow-ssh
//!   params:
//!     action: allow
//!     port: 22
//!     proto: tcp
//! - module: "@core/ufw"
//!   requires: [ufw-allow-ssh]
//!   params:
//!     enabled: true
//!     incoming: deny
//! ```

use std::fmt::Display;

use async_trait::async_trait;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_operation::{
    Operation,
    operations::ufw::{UfwDefaultDirection, UfwOperation, UfwPolicy},
};
use lusid_params::{ParseError, ParseParams, StructFields, parse_string};
use lusid_view::impl_display_render;
use rimu::{Spanned, Value};
use thiserror::Error;

use crate::ResourceType;

#[derive(Debug, Clone)]
pub struct UfwParams {
    pub enabled: Option<bool>,
    /// `None` falls back to ufw's safe default of `Deny`. Declaring `@core/ufw`
    /// at all is taken as opting in to managing the incoming default — there
    /// is no "leave alone" form for this direction. If you need that, omit
    /// the resource entirely.
    pub incoming: Option<UfwPolicy>,
    /// `None` falls back to ufw's safe default of `Allow`. Same opt-in
    /// semantics as [`Self::incoming`].
    pub outgoing: Option<UfwPolicy>,
    /// `None` means "leave the routed default alone" — *not* "set to
    /// disabled". Asymmetric with `incoming`/`outgoing` because ufw has no
    /// clean way to clear a routed default short of `ufw reset`, and routed
    /// defaults are uncommon enough that an opt-in form is the safer choice.
    pub routed: Option<UfwPolicy>,
}

impl ParseParams for UfwParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let enabled = fields.optional_bool("enabled")?;
        let incoming = fields.optional("incoming", parse_policy)?;
        let outgoing = fields.optional("outgoing", parse_policy)?;
        let routed = fields.optional("routed", parse_policy)?;
        fields.finish()?;
        Ok(UfwParams {
            enabled,
            incoming,
            outgoing,
            routed,
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
        } = self;
        write!(
            f,
            "Ufw(enabled = {enabled:?}, incoming = {incoming:?}, outgoing = {outgoing:?}, routed = {routed:?})"
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
}

impl Display for UfwResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            enabled,
            incoming,
            outgoing,
            routed,
        } = self;
        write!(
            f,
            "Ufw(enabled = {enabled}, incoming = {incoming}, outgoing = {outgoing}, routed = {routed:?})"
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
}

impl Display for UfwState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { enabled, defaults } = self;
        write!(f, "Ufw(enabled = {enabled}, defaults = {defaults:?})")
    }
}

impl_display_render!(UfwState);

#[derive(Error, Debug)]
pub enum UfwStateError {
    #[error(transparent)]
    Command(#[from] CommandError),

    #[error("unknown ufw policy: {policy}")]
    UnknownPolicy { policy: String },
}

#[derive(Debug, Clone)]
pub struct UfwChange {
    pub enable: Option<bool>,
    pub incoming: Option<UfwPolicy>,
    pub outgoing: Option<UfwPolicy>,
    pub routed: Option<UfwPolicy>,
}

impl Display for UfwChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            enable,
            incoming,
            outgoing,
            routed,
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
        Ok(UfwState { enabled, defaults })
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

        if enable.is_none() && incoming.is_none() && outgoing.is_none() && routed.is_none() {
            None
        } else {
            Some(UfwChange {
                enable,
                incoming,
                outgoing,
                routed,
            })
        }
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        let UfwChange {
            enable,
            incoming,
            outgoing,
            routed,
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

        // Group defaults under a named branch so the toggle below can `requires`
        // them — the branch id is scoped to this resource via
        // `plan::tree::map_plan_subitems`, so it can't collide.
        let defaults_branch =
            CausalityTree::branch(CausalityMeta::id("defaults".into()), default_ops);

        let mut out = vec![defaults_branch];

        if let Some(enable) = enable {
            let op = if enable {
                UfwOperation::Enable
            } else {
                UfwOperation::Disable
            };
            out.push(CausalityTree::leaf(
                CausalityMeta {
                    id: None,
                    requires: vec!["defaults".into()],
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
        // Tolerate runs of spaces or tabs between the policy word and the
        // direction tag — ufw output has been single-space historically, but
        // we don't want a future formatting tweak to silently skip a slot.
        let mut tokens = part.split_whitespace();
        let Some(word) = tokens.next() else { continue };
        let Some(direction_token) = tokens.next() else {
            continue;
        };
        let direction = direction_token
            .trim_start_matches('(')
            .trim_end_matches(')');
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
    fn change_none_when_state_matches_resource() {
        let resource = UfwResource {
            enabled: true,
            incoming: UfwPolicy::Deny,
            outgoing: UfwPolicy::Allow,
            routed: None,
        };
        let state = UfwState {
            enabled: true,
            defaults: Some(UfwDefaults {
                incoming: UfwPolicy::Deny,
                outgoing: UfwPolicy::Allow,
                routed: None,
            }),
        };
        assert!(Ufw::change(&resource, &state).is_none());
    }

    /// When `state.defaults` is `None` (firewall inactive on older ufw),
    /// we don't know the on-disk defaults, so emit `SetDefault` unconditionally
    /// to converge them.
    #[test]
    fn change_emits_defaults_when_state_unknown() {
        let resource = UfwResource {
            enabled: false,
            incoming: UfwPolicy::Deny,
            outgoing: UfwPolicy::Allow,
            routed: None,
        };
        let state = UfwState {
            enabled: false,
            defaults: None,
        };
        let change = Ufw::change(&resource, &state).expect("some");
        assert_eq!(change.incoming, Some(UfwPolicy::Deny));
        assert_eq!(change.outgoing, Some(UfwPolicy::Allow));
        assert!(change.enable.is_none());
    }
}
