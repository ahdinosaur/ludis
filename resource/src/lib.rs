use std::fmt::Display;

use crate::resources::file::FileParams;
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

use crate::resources::apt::{Apt, AptChange, AptParams, AptResource, AptState};
use crate::resources::file::{File, FileChange, FileResource, FileState};
use crate::resources::pacman::{Pacman, PacmanChange, PacmanParams, PacmanResource, PacmanState};

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
    File(FileParams),
    Pacman(PacmanParams),
}

impl Display for ResourceParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ResourceParams::*;
        match self {
            Apt(params) => params.fmt(f),
            File(params) => params.fmt(f),
            Pacman(params) => params.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Resource {
    Apt(AptResource),
    File(FileResource),
    Pacman(PacmanResource),
}

impl Display for Resource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Resource::*;
        match self {
            Apt(apt) => apt.fmt(f),
            File(file) => file.fmt(f),
            Pacman(pacman) => pacman.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ResourceState {
    Apt(AptState),
    File(FileState),
    Pacman(PacmanState),
}

impl Display for ResourceState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ResourceState::*;
        match self {
            Apt(apt) => apt.fmt(f),
            File(file) => file.fmt(f),
            Pacman(pacman) => pacman.fmt(f),
        }
    }
}

#[derive(Error, Debug)]
pub enum ResourceStateError {
    #[error("apt state error: {0}")]
    Apt(#[from] <Apt as ResourceType>::StateError),
    #[error("file state error: {0}")]
    File(#[from] <File as ResourceType>::StateError),
    #[error("pacman state error: {0}")]
    Pacman(#[from] <Pacman as ResourceType>::StateError),
}

#[derive(Debug, Clone)]
pub enum ResourceChange {
    Apt(AptChange),
    File(FileChange),
    Pacman(PacmanChange),
}

impl Display for ResourceChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ResourceChange::*;
        match self {
            Apt(apt) => apt.fmt(f),
            File(file) => file.fmt(f),
            Pacman(pacman) => pacman.fmt(f),
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
            ResourceParams::File(params) => typed::<File>(params, Resource::File),
            ResourceParams::Pacman(params) => typed::<Pacman>(params, Resource::Pacman),
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
            Resource::File(resource) => {
                typed::<File>(ctx, resource, ResourceState::File, ResourceStateError::File).await
            }
            Resource::Pacman(resource) => {
                typed::<Pacman>(
                    ctx,
                    resource,
                    ResourceState::Pacman,
                    ResourceStateError::Pacman,
                )
                .await
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
            (Resource::File(resource), ResourceState::File(state)) => {
                typed::<File>(resource, state, ResourceChange::File)
            }
            (Resource::Pacman(resource), ResourceState::Pacman(state)) => {
                typed::<Pacman>(resource, state, ResourceChange::Pacman)
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
            ResourceChange::File(change) => File::operations(change),
            ResourceChange::Pacman(change) => Pacman::operations(change),
        }
    }
}
