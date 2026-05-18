//! `@operation/<id>` is the imperative-action namespace. Operations live only
//! inside an `on_change` block on a `@resource/*` plan item - they're the
//! actions to run when the resource has a non-empty state diff.
//!
//! This module mirrors [`crate::resource`] for that surface: it routes an
//! `@operation/<id>` module string to the typed [`Operation`] value parsed
//! from the plan's Rimu params.

use lusid_operation::{
    Operation,
    operations::{command::CommandOperation, systemd::SystemdOperation},
};
use lusid_params::ParseParams;
use rimu::{Span, Spanned, Value};

use crate::PlanItemToResourceError;

/// Operations exposed in the `@operation/<id>` namespace.
///
/// Single source of truth - the dispatcher in [`operation_module`] matches
/// against these, and the [`PlanItemToResourceError::UnsupportedOperationModuleId`]
/// error message reads from here so the two never drift.
///
/// TODO(cc): more operation modules. Each apt/pacman/file/etc. family
/// exists internally as the result of a resource's change ops; promoting
/// any of them to author-facing means adding the id here, a parser arm in
/// `operation_module`, and a doc page. Workaround today is
/// `@operation/command "sudo apt update"` and friends.
pub const AVAILABLE_OPERATION_MODULES: &[&str] = &["command", "systemd"];

/// Returns the operation id (e.g. `"systemd"`) if `module` uses the
/// `@operation/<id>` prefix, otherwise `None`.
pub fn is_operation_module(module: &Spanned<String>) -> Option<&str> {
    module.inner().strip_prefix("@operation/")
}

/// Parse `params` into a typed [`Operation`] for the given operation id, or
/// error with a span pointing at the module string.
pub fn operation_module(
    operation_module_id: &str,
    params: Option<Spanned<Value>>,
    span: Span,
) -> Result<Operation, PlanItemToResourceError> {
    match operation_module_id {
        "command" => parse::<CommandOperation>(params).map(Operation::Command),
        "systemd" => parse::<SystemdOperation>(params).map(Operation::Systemd),
        other => Err(PlanItemToResourceError::UnsupportedOperationModuleId {
            id: other.to_string(),
            available: AVAILABLE_OPERATION_MODULES.join(", "),
            span,
        }),
    }
}

fn parse<O: ParseParams>(params: Option<Spanned<Value>>) -> Result<O, PlanItemToResourceError> {
    let value = params.ok_or(PlanItemToResourceError::MissingParams)?;
    O::parse_params(value).map_err(PlanItemToResourceError::Parse)
}
