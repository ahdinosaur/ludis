use displaydoc::Display;
use lusid_params::{validate, ParamValuesFromRimuError, ParamsValidationError};
use lusid_resource::ResourceParams;
use lusid_store::{Store, StoreError, StoreItemId};
use rimu::{Spanned, Value};
use std::{path::PathBuf, string::FromUtf8Error};
use thiserror::Error;

mod core;
mod eval;
mod id;
mod load;
mod model;
mod tree;

pub use crate::id::{PlanId, PlanNodeId};
pub use crate::tree::*;
use crate::{
    core::{core_module, is_core_module},
    eval::{evaluate, EvalError},
    load::{load, LoadError},
    model::Plan,
};

#[derive(Debug, Error, Display)]
pub enum PlanError {
    /// Failed to read plan source from store for id {id:?}
    StoreRead {
        id: StoreItemId,
        #[source]
        source: StoreError,
    },

    /// Failed to decode plan source as UTF-8
    InvalidUtf8(#[from] FromUtf8Error),

    /// Failed to load plan source
    Load(#[from] LoadError),

    /// Parameter validation failed
    Validate(#[from] ParamsValidationError),

    /// Failed to evaluate plan setup
    Eval(#[from] EvalError),

    /// Failed to convert plan item to resource
    PlanItemToResource(#[from] PlanItemToResourceError),
}

/// Top-level planning routine: load plan, validate parameters, and evaluate to
/// a CausalityTree<Resource>.
#[tracing::instrument(skip_all)]
pub async fn plan(
    plan_id: PlanId,
    params_value: Option<Spanned<Value>>,
    store: &mut Store,
) -> Result<PlanTree<ResourceParams>, PlanError> {
    tracing::debug!("Plan {plan_id:?} with params {params_value:?}");
    let children = plan_recursive(plan_id, params_value.as_ref(), store).await?;
    let tree = PlanTree::Branch {
        children,
        meta: PlanMeta::default(),
    };
    tracing::trace!("Planned resource tree: {:?}", tree);
    Ok(tree)
}

async fn plan_recursive(
    plan_id: PlanId,
    params_value: Option<&Spanned<Value>>,
    store: &mut Store,
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

    let params_struct = validate(param_types.as_ref(), params_value)?;

    let plan_items = evaluate(setup, params_value.cloned(), params_struct)?;

    let mut resources = Vec::with_capacity(plan_items.len());
    for plan_item in plan_items {
        let node = Box::pin(plan_item_to_resource(plan_item, &plan_id, store)).await?;
        resources.push(node);
    }

    Ok(resources)
}

#[derive(Debug, Error, Display)]
pub enum PlanItemToResourceError {
    /// Missing required parameters in plan item
    MissingParams,

    /// Parameters validation for resource failed
    ParamsValidation(#[from] ParamsValidationError),

    /// Parameters value from rimu value for resource failed
    ParamsValueFromRimu(Spanned<ParamValuesFromRimuError>),

    /// Failed to convert parameter values to resource params
    SerdeValue(#[from] rimu::SerdeValueError),

    /// Unsupported core module id \"{id}\"
    UnsupportedCoreModuleId { id: String },

    /// Failed to compute subtree for nested plan
    PlanSubtree(#[from] Box<PlanError>),
}

async fn plan_item_to_resource(
    plan_item: Spanned<crate::model::PlanItem>,
    current_plan_id: &PlanId,
    store: &mut Store,
) -> Result<PlanTree<ResourceParams>, PlanItemToResourceError> {
    let (plan_item, _span) = plan_item.take();
    let crate::model::PlanItem {
        id: item_id,
        ref module,
        params: params_value,
        before,
        after,
    } = plan_item;

    let id = item_id.map(|id| PlanNodeId::PlanItem {
        plan_id: current_plan_id.clone(),
        item_id: id.into_inner(),
    });
    let before = before
        .into_iter()
        .map(|v| v.into_inner())
        .map(|item_id| PlanNodeId::PlanItem {
            plan_id: current_plan_id.clone(),
            item_id,
        })
        .collect();
    let after = after
        .into_iter()
        .map(|v| v.into_inner())
        .map(|item_id| PlanNodeId::PlanItem {
            plan_id: current_plan_id.clone(),
            item_id,
        })
        .collect();

    if let Some(core_module_id) = is_core_module(module) {
        let params = core_module(core_module_id, params_value)?;
        Ok(PlanTree::Leaf {
            meta: PlanMeta { id, before, after },
            node: params,
        })
    } else {
        let path = PathBuf::from(module.inner());
        let plan_id = current_plan_id.join(path);
        let children = plan_recursive(plan_id, params_value.as_ref(), store)
            .await
            .map_err(Box::new)?;
        Ok(PlanTree::Branch {
            meta: PlanMeta { id, before, after },
            children,
        })
    }
}
