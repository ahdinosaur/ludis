//! Planning: turn a `.lusid` plan (written in Rimu) into a tree of typed resource params.
//!
//! The entry point is [`plan`]. Given a root [`PlanId`] (local path, eventually also git),
//! optional Rimu params, and a reference to the current [`System`], it:
//!
//! 1. Reads the plan source from the [`Store`].
//! 2. Parses + evaluates Rimu into a [`Plan`] (via [`load::load`]).
//! 3. Validates user params against the plan's `params` schema.
//! 4. Invokes the plan's `setup(params, system)` function to get a list of `PlanItem`s.
//! 5. For each item, either:
//!    - If `module` starts with `@resource/<id>` → convert to [`ResourceParams`] (a leaf).
//!    - Otherwise → resolve the module as a sibling `.lusid` file, recurse, and attach
//!      as a subtree (a branch).
//!
//! The result is a [`PlanTree<ResourceParams>`] whose branch/leaf metadata carries the
//! [`PlanNodeId`] identifiers used by causality scheduling downstream.

use displaydoc::Display;
use lusid_params::{ParamsContext, ParamsValidationError, ParseError, validate};
use lusid_resource::ResourceParams;
use lusid_store::{Store, StoreError, StoreItemId};
use lusid_system::System;
use rimu::{Span, Spanned, Value};
use std::{path::PathBuf, string::FromUtf8Error};
use thiserror::Error;

mod eval;
mod id;
mod load;
mod model;
mod operation;
mod resource;
mod tree;

pub use crate::id::{PlanId, PlanNodeId};
pub use crate::tree::*;
use crate::{
    eval::{EvalError, evaluate},
    load::{LoadError, load},
    model::Plan,
    operation::is_operation_module,
    resource::{is_resource_module, resource_module},
};

#[derive(Debug, Error, Display)]
pub enum PlanError {
    /// Failed to read plan source from store for id {id:?}: {source}
    StoreRead {
        id: StoreItemId,
        #[source]
        source: StoreError,
    },

    /// Failed to decode plan source as UTF-8: {0}
    InvalidUtf8(#[from] FromUtf8Error),

    /// Failed to load plan source: {0}
    Load(#[from] LoadError),

    /// Parameter validation failed: {0}
    Validate(#[from] ParamsValidationError),

    /// Failed to evaluate plan setup: {0}
    Eval(#[from] EvalError),

    /// Failed to convert plan item to resource: {0}
    PlanItemToResource(#[from] PlanItemToResourceError),
}

/// Plan a `.lusid` file recursively, producing a tree of typed resource params.
///
/// Wraps the recursive subplan in a root [`PlanTree::Branch`] with default metadata so
/// callers always get a tree (never a bare list).
///
/// `ctx` carries the fallback root path used to resolve relative `host-path`
/// strings — typically the project root. The same `ctx` is shared across the
/// whole plan tree: each plan's `validate` rewrites string-shaped paths into
/// the typed Rimu variants before forwarding, so a sub-plan only ever sees a
/// `Value::String` for a `host-path` field if a literal one was written
/// in-source (in which case the literal's span source anchors the resolution
/// directly, not `ctx`).
#[tracing::instrument(skip_all)]
pub async fn plan(
    plan_id: PlanId,
    params_value: Option<Spanned<Value>>,
    ctx: &ParamsContext,
    store: &mut Store,
    system: &System,
) -> Result<PlanTree<ResourceParams>, PlanError> {
    tracing::debug!("Plan {plan_id:?} with params {params_value:?}");
    let children = plan_recursive(plan_id, params_value, ctx, store, system).await?;
    let tree = PlanTree::Branch {
        children,
        meta: PlanMeta::default(),
    };
    tracing::trace!("Planned resource tree: {:?}", tree);
    Ok(tree)
}

/// Inner recursive routine. Each call handles exactly one `.lusid` source: load, validate
/// params, evaluate `setup`, and convert each returned item into a subtree.
async fn plan_recursive(
    plan_id: PlanId,
    params_value: Option<Spanned<Value>>,
    ctx: &ParamsContext,
    store: &mut Store,
    system: &System,
) -> Result<Vec<PlanTree<ResourceParams>>, PlanError> {
    let store_item_id: StoreItemId = plan_id.clone().into();
    let bytes = store
        .read(&store_item_id)
        .await
        .map_err(|source| PlanError::StoreRead {
            id: store_item_id.clone(),
            source,
        })?;
    let code = String::from_utf8(bytes)?;
    let plan = load(&code, &plan_id)?;

    let Plan {
        name: _,
        version: _,
        params: param_types,
        setup,
    } = plan.into_inner();

    // `validate` returns the coerced params value: relative `host-path`
    // strings have been rewritten into `Value::HostPath`, etc. Feeding the
    // coerced value into `evaluate` is what makes parent → sub-plan
    // forwarding work — by the time a forwarded value reaches a sub-plan's
    // `validate`, it's already typed and just passes through.
    let coerced_params = validate(param_types.as_ref(), params_value, ctx)?;

    let plan_items = evaluate(setup, coerced_params, system)?;

    let mut resources = Vec::with_capacity(plan_items.len());
    for plan_item in plan_items {
        let node = Box::pin(plan_item_to_resource(
            plan_item, &plan_id, ctx, store, system,
        ))
        .await?;
        resources.push(node);
    }

    Ok(resources)
}

#[derive(Debug, Error, Display)]
pub enum PlanItemToResourceError {
    /// Missing required parameters in plan item
    MissingParams,

    /// Failed to parse parameters for resource: {0}
    Parse(Spanned<ParseError>),

    /// unknown @resource/ module: \"{id}\"
    UnsupportedResourceModuleId { id: String },

    /// operations cannot appear at the top level — `@operation/{id}` is only valid inside `on_change`. To run an action when a resource changes, attach it via `on_change` on the relevant `@resource/*`. For idempotent imperative actions at the top level, see `@resource/command`.
    OperationModuleAsTopLevel { id: String, span: Span },

    /// `on_change` is only valid on `@resource/*` plan items, got `{module}`
    OnChangeOnNonResource { module: String, span: Span },

    /// `on_change` items must be `@operation/<id>`, got `{module}`. Resources describe desired state; operations describe imperative actions.
    OnChangeItemModuleNotAnOperation { module: String, span: Span },

    /// unknown @operation/ module: `{id}`. Available: {available}
    UnsupportedOperationModuleId {
        id: String,
        available: String,
        span: Span,
    },

    /// Failed to compute subtree for nested plan: {0}
    PlanSubtree(#[from] Box<PlanError>),
}

/// Lower a single `PlanItem` to a subtree. Resource modules produce a leaf with
/// [`ResourceParams`]; every other module name is treated as a path relative to the
/// parent plan and recursed into as a branch.
async fn plan_item_to_resource(
    plan_item: Spanned<crate::model::PlanItem>,
    current_plan_id: &PlanId,
    ctx: &ParamsContext,
    store: &mut Store,
    system: &System,
) -> Result<PlanTree<ResourceParams>, PlanItemToResourceError> {
    let (plan_item, _span) = plan_item.take();
    let crate::model::PlanItem {
        id: item_id,
        module,
        params: params_value,
        requires,
        required_by,
        on_change,
    } = plan_item;

    // `@operation/*` only lives inside `on_change`; reject as a top-level item.
    if let Some(op_id) = is_operation_module(&module) {
        return Err(PlanItemToResourceError::OperationModuleAsTopLevel {
            id: op_id.to_string(),
            span: module.span(),
        });
    }

    // Parse on_change. Rejected for non-`@resource/*` plan items.
    let handlers = if on_change.is_empty() {
        Vec::new()
    } else {
        if is_resource_module(&module).is_none() {
            return Err(PlanItemToResourceError::OnChangeOnNonResource {
                module: module.inner().to_string(),
                span: module.span(),
            });
        }
        parse_on_change(on_change)?
    };

    let id = item_id.map(|id| PlanNodeId::PlanItem {
        plan_id: current_plan_id.clone(),
        item_id: id.into_inner(),
    });
    let requires = requires
        .into_iter()
        .map(|v| v.into_inner())
        .map(|item_id| PlanNodeId::PlanItem {
            plan_id: current_plan_id.clone(),
            item_id,
        })
        .collect();
    let required_by = required_by
        .into_iter()
        .map(|v| v.into_inner())
        .map(|item_id| PlanNodeId::PlanItem {
            plan_id: current_plan_id.clone(),
            item_id,
        })
        .collect();

    if let Some(resource_module_id) = is_resource_module(&module) {
        let params = resource_module(resource_module_id, params_value)?;
        Ok(PlanTree::Leaf {
            meta: PlanMeta {
                id,
                requires,
                required_by,
                handlers,
            },
            node: params,
        })
    } else {
        // Nested plan path. on_change already rejected above.
        let path = PathBuf::from(module.inner());
        let plan_id = current_plan_id.join(path);
        let children = plan_recursive(plan_id, params_value, ctx, store, system)
            .await
            .map_err(Box::new)?;
        Ok(PlanTree::Branch {
            meta: PlanMeta {
                id,
                requires,
                required_by,
                handlers: Vec::new(),
            },
            children,
        })
    }
}

/// Lower a parsed `on_change` list to a flat vector of typed [`Operation`]s.
///
/// Each entry must use a `@operation/<id>` module string; any other prefix
/// (nested plan path, `@resource/...`, or a legacy `@core/...`) surfaces as
/// `OnChangeItemModuleNotAnOperation`.
fn parse_on_change(
    items: Vec<Spanned<crate::model::InlineOperation>>,
) -> Result<Vec<lusid_operation::Operation>, PlanItemToResourceError> {
    let mut out = Vec::with_capacity(items.len());
    for spanned in items {
        let (item, _item_span) = spanned.take();
        let op_id = operation::is_operation_module(&item.module).ok_or_else(|| {
            PlanItemToResourceError::OnChangeItemModuleNotAnOperation {
                module: item.module.inner().to_string(),
                span: item.module.span(),
            }
        })?;
        let op = operation::operation_module(op_id, item.params, item.module.span())?;
        out.push(op);
    }
    Ok(out)
}
