use std::fmt::Display;

use async_trait::async_trait;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_operation::{Operation, operations::pacman::PacmanOperation};
use lusid_params::{ParseError, ParseParams, StructFields};
use lusid_view::impl_display_render;
use rimu::{Spanned, Value};
use thiserror::Error;

use crate::ResourceType;

#[derive(Debug, Clone)]
pub enum PacmanParams {
    Package { package: String },
    Packages { packages: Vec<String> },
}

impl ParseParams for PacmanParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let out = if fields.has("packages") {
            PacmanParams::Packages {
                packages: fields.required_string_list("packages")?,
            }
        } else {
            PacmanParams::Package {
                package: fields.required_string("package")?,
            }
        };
        fields.finish()?;
        Ok(out)
    }
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

impl_display_render!(PacmanParams);

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

impl_display_render!(PacmanResource);

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

impl_display_render!(PacmanState);

#[derive(Error, Debug)]
pub enum PacmanStateError {
    #[error(transparent)]
    Command(#[from] CommandError),

    #[error("failed to determine package status: {output}")]
    ParseStatus { output: String },
}

// TODO(cc): add an `Uninstall` variant - mirror image of the apt resource. A declared
// package cannot currently be retracted.
#[derive(Debug, Clone)]
pub enum PacmanChange {
    Install { package: String },
}

impl Display for PacmanChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PacmanChange::Install { package } => write!(f, "Pacman::Install({package})"),
        }
    }
}

impl_display_render!(PacmanChange);

#[derive(Debug, Clone)]
pub struct Pacman;

#[async_trait]
impl ResourceType for Pacman {
    const ID: &'static str = "pacman";

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
                        meta: CausalityMeta::requires(vec!["upgrade".into()]),
                    },
                ]
            }
        }
    }
}
