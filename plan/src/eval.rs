//! Evaluate a plan's `setup(params, system)` Rimu function into a list of `PlanItem`s.

use displaydoc::Display;
use lusid_system::System;
use rimu::{SourceId, Span, Spanned, Value, call};
use rimu_interop::{FromRimu, to_rimu};
use thiserror::Error;

use crate::model::{IntoPlanItemError, PlanItem, SetupFunction};

#[derive(Debug, Error, Display)]
pub enum EvalError {
    /// Converting system to rimu value failed: {0}
    System(#[from] rimu_interop::ToRimuError),

    /// Calling setup function failed: {0}
    RimuCall(#[from] Box<rimu::EvalError>),

    /// Setup returned a non-list value
    ReturnedNotList,

    /// Invalid PlanItem value: {0}
    InvalidPlanItem(Box<Spanned<IntoPlanItemError>>),
}

/// Call the plan's `setup` function with `(params, system)` and parse its returned list
/// into [`PlanItem`]s.
///
/// `params_value` is `None` when the caller provided no params - in that case the first
/// arg is `Null` (rather than e.g. an empty object, to match what a plan's `setup` sees
/// for "no params given"). When provided, the value is passed through as-is.
///
/// The caller (`plan_recursive`) is responsible for running the params through
/// [`lusid_params::validate`] first, which both checks the shape *and* coerces
/// string-shaped paths into the typed Rimu variants. So `setup` sees a
/// `Value::HostPath` for a `host-path` param, and forwarding such a param to a
/// sub-plan is just a typed pass-through - the sub-plan's `validate` doesn't
/// need to re-resolve anything.
pub(crate) fn evaluate(
    setup: Spanned<SetupFunction>,
    params_value: Option<Spanned<Value>>,
    system: &System,
) -> Result<Vec<Spanned<PlanItem>>, EvalError> {
    let (setup, setup_span) = setup.take();

    let system_value = to_rimu(system, SourceId::empty())?;

    let args = match params_value {
        None => vec![
            Spanned::new(Value::Null, Span::new(SourceId::empty(), 0, 0)),
            system_value,
        ],
        Some(params_value) => vec![params_value, system_value],
    };

    let result = call(setup_span, setup.0, &args).map_err(Box::new)?;
    let (result, _result_span) = result.take();

    let Value::List(items) = result else {
        return Err(EvalError::ReturnedNotList);
    };

    let mut out = Vec::with_capacity(items.len());
    for item in items {
        let call = PlanItem::from_rimu_spanned(item)
            .map_err(|error| EvalError::InvalidPlanItem(Box::new(error)))?;
        out.push(call)
    }
    Ok(out)
}
