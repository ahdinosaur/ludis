use std::fmt::Display;

use async_trait::async_trait;
use indexmap::indexmap;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_operation::{operations::pacman::PacmanOperation, Operation};
use lusid_params::{ParamField, ParamType, ParamTypes};
use rimu::{SourceId, Span, Spanned};
use serde::Deserialize;
use thiserror::Error;

use crate::ResourceType;

#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum PacmanParams {
    Package { package: String },
    Packages { packages: Vec<String> },
}

impl Display for PacmanParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PacmanParams::Package { package } => write!(f, "Pacman(package = {package})"),
            PacmanParams::Packages { packages } => {
                write!(f, "Pacman(packages = [{}])", packages.join(", "))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct PacmanResource {
    pub package: String,
}

impl Display for PacmanResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { package } = self;
        write!(f, "Pacman({package})")
    }
}

#[derive(Debug, Clone)]
pub enum PacmanState {
    NotInstalled,
    Installed,
}

impl Display for PacmanState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PacmanState::NotInstalled => write!(f, "Pacman::NotInstalled"),
            PacmanState::Installed => write!(f, "Pacman::Installed"),
        }
    }
}

#[derive(Error, Debug)]
pub enum PacmanStateError {
    #[error(transparent)]
    Command(#[from] CommandError),

    #[error("failed to determine package status: {output}")]
    ParseStatus { output: String },
}

#[derive(Debug, Clone)]
pub enum PacmanChange {
    Install { package: String },
}

impl Display for PacmanChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PacmanChange::Install { package } => write!(f, "Pacman::Installed({package})"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Pacman;

#[async_trait]
impl ResourceType for Pacman {
    const ID: &'static str = "pacman";

    fn param_types() -> Option<Spanned<ParamTypes>> {
        let span = Span::new(SourceId::empty(), 0, 0);
        Some(Spanned::new(
            ParamTypes::Union(vec![
                indexmap! {
                    "package".to_string() =>
                        Spanned::new(ParamField::new(ParamType::String), span.clone()),
                },
                indexmap! {
                    "packages".to_string() => Spanned::new(
                        ParamField::new(ParamType::List {
                            item: Box::new(Spanned::new(ParamType::String, span.clone())),
                        }),
                        span.clone(),
                    ),
                },
            ]),
            span,
        ))
    }

    type Params = PacmanParams;
    type Resource = PacmanResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        match params {
            PacmanParams::Package { package } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                PacmanResource { package },
            )],
            PacmanParams::Packages { packages } => packages
                .into_iter()
                .map(|package| {
                    CausalityTree::leaf(CausalityMeta::default(), PacmanResource { package })
                })
                .collect(),
        }
    }

    type State = PacmanState;
    type StateError = PacmanStateError;
    async fn state(
        _ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        Command::new("pacman")
            .args(["-Q", &resource.package])
            .handle(
                |stdout| {
                    let stdout = String::from_utf8_lossy(stdout);
                    if stdout.trim().is_empty() {
                        Err(PacmanStateError::ParseStatus {
                            output: stdout.to_string(),
                        })
                    } else {
                        Ok(PacmanState::Installed)
                    }
                },
                |stderr| {
                    let stderr = String::from_utf8_lossy(stderr);
                    if stderr.contains("was not found") {
                        Ok(Some(PacmanState::NotInstalled))
                    } else {
                        Ok(None)
                    }
                },
            )
            .await?
    }

    type Change = PacmanChange;
    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        match state {
            PacmanState::Installed => None,
            PacmanState::NotInstalled => Some(PacmanChange::Install {
                package: resource.package.clone(),
            }),
        }
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        match change {
            PacmanChange::Install { package } => {
                vec![
                    CausalityTree::Leaf {
                        node: Operation::Pacman(PacmanOperation::Upgrade),
                        meta: CausalityMeta::id("upgrade".into()),
                    },
                    CausalityTree::Leaf {
                        node: Operation::Pacman(PacmanOperation::Install {
                            packages: vec![package],
                        }),
                        meta: CausalityMeta::before(vec!["upgrade".into()]),
                    },
                ]
            }
        }
    }
}
