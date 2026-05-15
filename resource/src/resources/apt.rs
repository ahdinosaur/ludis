use std::fmt::Display;

use async_trait::async_trait;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_operation::{Operation, operations::apt::AptOperation};
use lusid_params::{ParseError, ParseParams, StructFields};
use lusid_view::impl_display_render;
use rimu::{Spanned, Value};
use thiserror::Error;

use crate::ResourceType;

#[derive(Debug, Clone)]
pub enum AptParams {
    Package { package: String },
    Packages { packages: Vec<String> },
}

impl ParseParams for AptParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        // Untagged dispatch: `packages` (the more specific list form) wins
        // when present; otherwise we read the singular `package` field.
        let out = if fields.has("packages") {
            AptParams::Packages {
                packages: fields.required_string_list("packages")?,
            }
        } else {
            AptParams::Package {
                package: fields.required_string("package")?,
            }
        };
        fields.finish()?;
        Ok(out)
    }
}

impl Display for AptParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AptParams::Package { package } => write!(f, "Apt(package = {package})"),
            AptParams::Packages { packages } => {
                write!(f, "Apt(packages = [{}])", packages.join(", "))
            }
        }
    }
}

impl_display_render!(AptParams);

#[derive(Debug, Clone)]
pub struct AptResource {
    pub package: String,
}

impl Display for AptResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { package } = self;
        write!(f, "Apt({package})")
    }
}

impl_display_render!(AptResource);

#[derive(Debug, Clone)]
pub enum AptState {
    NotInstalled,
    Installed,
}

impl Display for AptState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AptState::NotInstalled => write!(f, "Apt::NotInstalled"),
            AptState::Installed => write!(f, "Apt::Installed"),
        }
    }
}

impl_display_render!(AptState);

#[derive(Error, Debug)]
pub enum AptStateError {
    #[error(transparent)]
    Command(#[from] CommandError),

    #[error("failed to parse status: {status}")]
    ParseStatus { status: String },
}

// TODO(cc): add an `Uninstall` variant. Today a package can be declared but not
// retracted - removing it from the plan leaves it installed on the machine.
#[derive(Debug, Clone)]
pub enum AptChange {
    Install { package: String },
}

impl Display for AptChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AptChange::Install { package } => write!(f, "Apt::Install({package})"),
        }
    }
}

impl_display_render!(AptChange);

#[derive(Debug, Clone)]
pub struct Apt;

#[async_trait]
impl ResourceType for Apt {
    const ID: &'static str = "apt";

    type Params = AptParams;
    type Resource = AptResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        match params {
            AptParams::Package { package } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                AptResource { package },
            )],
            AptParams::Packages { packages } => packages
                .into_iter()
                .map(|package| {
                    CausalityTree::leaf(CausalityMeta::default(), AptResource { package })
                })
                .collect(),
        }
    }

    type State = AptState;
    type StateError = AptStateError;
    async fn state(
        _ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        Command::new("dpkg-query")
            .args(["-W", "-f='${Status}'", &resource.package])
            .handle(
                |stdout| {
                    let stdout = String::from_utf8_lossy(stdout);
                    let status_parts: Vec<_> = stdout.trim_matches('\'').split(" ").collect();
                    let Some(status) = status_parts.get(2) else {
                        return Err(AptStateError::ParseStatus {
                            status: stdout.to_string(),
                        });
                    };
                    match *status {
                        "not-installed" => Ok(AptState::NotInstalled),
                        "unpacked" => Ok(AptState::NotInstalled),
                        "half-installed" => Ok(AptState::NotInstalled),
                        "installed" => Ok(AptState::Installed),
                        "config-files" => Ok(AptState::NotInstalled),
                        _ => Err(AptStateError::ParseStatus {
                            status: stdout.to_string(),
                        }),
                    }
                },
                |stderr| {
                    let stderr = String::from_utf8_lossy(stderr);
                    if stderr.contains("no packages found matching") {
                        Ok(Some(AptState::NotInstalled))
                    } else {
                        Ok(None)
                    }
                },
            )
            .await?
    }

    type Change = AptChange;
    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        match state {
            AptState::Installed => None,
            AptState::NotInstalled => Some(AptChange::Install {
                package: resource.package.clone(),
            }),
        }
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        match change {
            AptChange::Install { package } => {
                vec![
                    CausalityTree::Leaf {
                        node: Operation::Apt(AptOperation::Update),
                        meta: CausalityMeta::id("update".into()),
                    },
                    CausalityTree::Leaf {
                        node: Operation::Apt(AptOperation::Install {
                            packages: vec![package],
                        }),
                        meta: CausalityMeta::requires(vec!["update".into()]),
                    },
                ]
            }
        }
    }
}
