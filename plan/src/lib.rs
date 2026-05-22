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
/// strings - typically the project root. The same `ctx` is shared across the
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
    let children = plan_recursive(plan_id, &[], params_value, ctx, store, system).await?;
    let tree = PlanTree::Branch {
        children,
        meta: PlanMeta::default(),
    };
    tracing::trace!("Planned resource tree: {:?}", tree);
    Ok(tree)
}

/// Recurse into a plan, producing its item subtrees. `scope_path` accumulates
/// the chain of outer invocation ids when descending into nested plan
/// invocations - empty at the top-level call, one element deeper per nesting.
/// Items inside this plan inherit `scope_path` for their PlanNodeId so two
/// invocations of the same subplan don't collide on their inner item ids.
async fn plan_recursive(
    plan_id: PlanId,
    scope_path: &[String],
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
    // forwarding work - by the time a forwarded value reaches a sub-plan's
    // `validate`, it's already typed and just passes through.
    let coerced_params = validate(param_types.as_ref(), params_value, ctx)?;

    let plan_items = evaluate(setup, coerced_params, system)?;

    let mut resources = Vec::with_capacity(plan_items.len());
    for plan_item in plan_items {
        let node = Box::pin(plan_item_to_resource(
            plan_item, &plan_id, scope_path, ctx, store, system,
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

    /// operations cannot appear at the top level - `@operation/{id}` is only valid inside `on_change`. To run an action when a resource changes, attach it via `on_change` on the relevant `@resource/*`. For idempotent imperative actions at the top level, see `@resource/command`.
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
///
/// `scope_path` is the chain of outer invocation ids of the current plan
/// (empty at the top level). It's used to scope this item's PlanNodeId so
/// items inside two invocations of the same subplan don't collide. When
/// this item is itself a nested-plan invocation, its outer id (or a
/// cuid2-minted fallback when no id is declared) is appended before
/// recursing so the inner items pick up the deeper scope.
async fn plan_item_to_resource(
    plan_item: Spanned<crate::model::PlanItem>,
    current_plan_id: &PlanId,
    scope_path: &[String],
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

    if let Some(op_id) = is_operation_module(&module) {
        return Err(PlanItemToResourceError::OperationModuleAsTopLevel {
            id: op_id.to_string(),
            span: module.span(),
        });
    }

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

    let item_id_str = item_id.map(|id| id.into_inner());
    let id = item_id_str.clone().map(|item_id| PlanNodeId::PlanItem {
        scope_path: scope_path.to_vec(),
        plan_id: current_plan_id.clone(),
        item_id,
    });
    let requires = requires
        .into_iter()
        .map(|v| v.into_inner())
        .map(|item_id| PlanNodeId::PlanItem {
            scope_path: scope_path.to_vec(),
            plan_id: current_plan_id.clone(),
            item_id,
        })
        .collect();
    let required_by = required_by
        .into_iter()
        .map(|v| v.into_inner())
        .map(|item_id| PlanNodeId::PlanItem {
            scope_path: scope_path.to_vec(),
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
        let path = PathBuf::from(module.inner());
        let plan_id = current_plan_id.join(path);
        // Descending into a nested invocation: extend the scope chain so the
        // subplan's items get a unique scope. Prefer the user-declared `id`
        // for readability; mint a cuid2 only when no id is declared (which
        // also implies the invocation can't be referenced by `requires:` -
        // anonymous invocations are leaves of the dependency graph).
        let mut child_scope = scope_path.to_vec();
        child_scope.push(item_id_str.unwrap_or_else(cuid2::create_id));
        let children = plan_recursive(plan_id, &child_scope, params_value, ctx, store, system)
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
/// (nested plan path, `@resource/...`) surfaces as
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
