use std::fmt::Display;

pub use crate::resources::*;

use async_trait::async_trait;
use lusid_causality::CausalityTree;
use lusid_ctx::Context;
use lusid_operation::Operation;
use lusid_params::ParamTypes;
use lusid_view::Render;
use rimu::Spanned;
use serde::de::DeserializeOwned;
use thiserror::Error;

mod resources;

use crate::resources::apt::AptParams;
use crate::resources::apt::{Apt, AptChange, AptResource, AptState};

/// ResourceType:
/// - ParamTypes for Rimu schema
/// - Resource (atom)
/// - State (current)
/// - Change (delta needed)
/// - Conversion from Change -> Operation(s)
#[async_trait]
pub trait ResourceType {
    const ID: &'static str;

    /// Schema for resource params.
    fn param_types() -> Option<Spanned<ParamTypes>>;

    /// Resource params (friendly user definition).
    type Params: Render + DeserializeOwned;

    /// Resource atom (indivisible system definition).
    type Resource: Render;

    /// Create resource atom from params.
    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>>;

    /// Current state of resource on machine.
    type State: Render;

    /// Possible error when fetching current state of resource on machine.
    type StateError;

    /// Fetch current state of resource on machine.
    async fn state(
        ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError>;

    /// A change from current state.
    type Change: Render;

    /// Get change atomic resource from current state to intended state.
    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change>;

    // Convert atomic resource change into operations (mutations).
    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>>;
}

#[derive(Debug, Clone)]
pub enum ResourceParams {
    Apt(AptParams),
}

impl Display for ResourceParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ResourceParams::*;
        match self {
            Apt(apt) => apt.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Resource {
    Apt(AptResource),
}

impl Display for Resource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Resource::*;
        match self {
            Apt(apt) => apt.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ResourceState {
    Apt(AptState),
}

impl Display for ResourceState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ResourceState::*;
        match self {
            Apt(apt) => apt.fmt(f),
        }
    }
}

#[derive(Error, Debug)]
pub enum ResourceStateError {
    #[error("apt state error: {0}")]
    Apt(#[from] <Apt as ResourceType>::StateError),
}

#[derive(Debug, Clone)]
pub enum ResourceChange {
    Apt(AptChange),
}

impl Display for ResourceChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ResourceChange::*;
        match self {
            Apt(apt) => apt.fmt(f),
        }
    }
}

impl ResourceParams {
    pub fn resources(self) -> Vec<CausalityTree<Resource>> {
        fn typed<R: ResourceType>(
            params: R::Params,
            map: impl Fn(R::Resource) -> Resource + Copy,
        ) -> Vec<CausalityTree<Resource>> {
            R::resources(params)
                .into_iter()
                .map(|tree| tree.map(map))
                .collect()
        }

        match self {
            ResourceParams::Apt(params) => typed::<Apt>(params, Resource::Apt),
        }
    }
}

impl Resource {
    pub async fn state(&self, ctx: &mut Context) -> Result<ResourceState, ResourceStateError> {
        async fn typed<R: ResourceType>(
            ctx: &mut Context,
            resource: &R::Resource,
            map: impl Fn(R::State) -> ResourceState,
            map_err: impl Fn(R::StateError) -> ResourceStateError,
        ) -> Result<ResourceState, ResourceStateError> {
            R::state(ctx, resource).await.map(map).map_err(map_err)
        }

        match self {
            Resource::Apt(resource) => {
                typed::<Apt>(ctx, resource, ResourceState::Apt, ResourceStateError::Apt).await
            }
        }
    }

    pub fn change(&self, state: &ResourceState) -> Option<ResourceChange> {
        fn typed<R: ResourceType>(
            resource: &R::Resource,
            state: &R::State,
            map: impl Fn(R::Change) -> ResourceChange,
        ) -> Option<ResourceChange> {
            R::change(resource, state).map(map)
        }

        // TODO (mw): remove #[allow(unreachable_patterns)] once we have more resources
        #[allow(unreachable_patterns)]
        match (self, state) {
            (Resource::Apt(resource), ResourceState::Apt(state)) => {
                typed::<Apt>(resource, state, ResourceChange::Apt)
            }
            _ => {
                // Programmer error, should never happen, or if it does should be immediately obvious.
                panic!("Unmatched resource and state")
            }
        }
    }
}

impl ResourceChange {
    pub fn operations(self) -> Vec<CausalityTree<Operation>> {
        match self {
            ResourceChange::Apt(change) => Apt::operations(change),
        }
    }
}
