use std::fmt::Display;

use async_trait::async_trait;
use indexmap::indexmap;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_operation::{operations::file::FileOperation, Operation};
use lusid_params::{ParamField, ParamType, ParamTypes};
use rimu::{SourceId, Span, Spanned};
use serde::Deserialize;
use thiserror::Error;

use crate::ResourceType;

#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum FileParams {
    Package { package: String },
    Packages { packages: Vec<String> },
}

impl Display for FileParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileParams::Package { package } => write!(f, "File(package = {package})"),
            FileParams::Packages { packages } => {
                write!(f, "File(packages = [{}])", packages.join(", "))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct FileResource {
    pub package: String,
}

impl Display for FileResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { package } = self;
        write!(f, "File({package})")
    }
}

#[derive(Debug, Clone)]
pub enum FileState {
    NotInstalled,
    Installed,
}

impl Display for FileState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileState::NotInstalled => write!(f, "File::NotInstalled"),
            FileState::Installed => write!(f, "File::Installed"),
        }
    }
}

#[derive(Error, Debug)]
pub enum FileStateError {
    #[error(transparent)]
    Command(#[from] CommandError),

    #[error("failed to parse status: {status}")]
    ParseStatus { status: String },
}

#[derive(Debug, Clone)]
pub enum FileChange {
    Install { package: String },
}

impl Display for FileChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileChange::Install { package } => write!(f, "File::Installed({package})"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct File;

#[async_trait]
impl ResourceType for File {
    const ID: &'static str = "apt";

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

    type Params = FileParams;
    type Resource = FileResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        match params {
            FileParams::Package { package } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                FileResource { package },
            )],
            FileParams::Packages { packages } => vec![CausalityTree::branch(
                CausalityMeta::default(),
                packages
                    .into_iter()
                    .map(|package| {
                        CausalityTree::leaf(CausalityMeta::default(), FileResource { package })
                    })
                    .collect(),
            )],
        }
    }

    type State = FileState;
    type StateError = FileStateError;
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
                        return Err(FileStateError::ParseStatus {
                            status: stdout.to_string(),
                        });
                    };
                    match *status {
                        "not-installed" => Ok(FileState::NotInstalled),
                        "unpacked" => Ok(FileState::NotInstalled),
                        "half-installed" => Ok(FileState::NotInstalled),
                        "installed" => Ok(FileState::Installed),
                        "config-files" => Ok(FileState::NotInstalled),
                        _ => Err(FileStateError::ParseStatus {
                            status: stdout.to_string(),
                        }),
                    }
                },
                |stderr| {
                    let stderr = String::from_utf8_lossy(stderr);
                    if stderr.contains("no packages found matching") {
                        Ok(Some(FileState::NotInstalled))
                    } else {
                        Ok(None)
                    }
                },
            )
            .await?
    }

    type Change = FileChange;
    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        match state {
            FileState::Installed => None,
            FileState::NotInstalled => Some(FileChange::Install {
                package: resource.package.clone(),
            }),
        }
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        match change {
            FileChange::Install { package } => {
                vec![
                    CausalityTree::Leaf {
                        node: Operation::File(FileOperation::Update),
                        meta: CausalityMeta {
                            id: Some("update".into()),
                            ..Default::default()
                        },
                    },
                    CausalityTree::Leaf {
                        node: Operation::File(FileOperation::Install {
                            packages: vec![package],
                        }),
                        meta: CausalityMeta {
                            id: None,
                            before: vec!["update".into()],
                            after: vec![],
                        },
                    },
                ]
            }
        }
    }
}
