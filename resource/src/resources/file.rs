use std::fmt::Display;

use async_trait::async_trait;
use indexmap::indexmap;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_operation::{
    operations::file::{FileGroup, FileMode, FileOperation, FilePath, FileUser},
    Operation,
};
use lusid_params::{ParamField, ParamType, ParamTypes};
use rimu::{SourceId, Span, Spanned};
use serde::Deserialize;
use thiserror::Error;

use crate::ResourceType;

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "type", rename_all = "kebab-case")]
pub enum FileParams {
    Source {
        source: FilePath,
        path: FilePath,
        mode: FileMode,
        user: FileUser,
        group: FileGroup,
    },
    File {
        path: FilePath,
        mode: FileMode,
        user: FileUser,
        group: FileGroup,
    },
    FileAbsent {
        path: FilePath,
    },
    Directory {
        path: FilePath,
        mode: FileMode,
        user: FileUser,
        group: FileGroup,
    },
    DirectoryFiles {
        path: FilePath,
        mode: FileMode,
        user: FileUser,
        group: FileGroup,
    },
    DirectoryAbsent {
        path: FilePath,
    },
}

impl Display for FileParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileParams::File { src, dest, .. } => write!(f, "File(src={src}, dest={dest})"),
            FileParams::Directory { src, dest, .. } => {
                write!(f, "Directory(src={src}, dest={dest})")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum FileResource {
    FileSource { source: FilePath, path: FilePath },
    FilePresent { path: FilePath },
    FileAbsent { path: FilePath },
    DirectoryPresent { path: FilePath },
    DirectoryAbsent { path: FilePath },
    FileMode { mode: FileMode },
    DirectoryFilesMode { mode: FileMode },
    User { user: FileGroup },
    DirectoryFilesUser { mode: FileMode },
    Group { group: FileGroup },
    DirectoryFilesGroup { mode: FileMode },
}

impl Display for FileResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { package } = self;
        write!(f, "File({package})")
    }
}

#[derive(Debug, Clone)]
pub enum FileState {
    FileSourced,
    FilePresent,
    FileAbsent,
    DirectoryPresent,
    DirectoryAbsent,
    ModeCorrect,
    ModeIncorrect,
    DirectoryFilesModeCorrect,
    DirectoryFilesModeIncorrect,
    UserCorrect,
    UserIncorrect,
    DirectoryFilesUserCorrect,
    DirectoryFilesUserIncorrect,
    GroupCorrect,
    GroupIncorrect,
    DirectoryFilesGroupCorrect,
    DirectoryFilesGroupIncorrect,
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
    const ID: &'static str = "file";

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

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {}

    type State = FileState;
    type StateError = FileStateError;
    async fn state(
        _ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
    }

    type Change = FileChange;
    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {}

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {}
}
