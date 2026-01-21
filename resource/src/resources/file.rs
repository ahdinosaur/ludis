use std::fmt::{self, Display};

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
    DirectoryAbsent {
        path: FilePath,
    },
}

impl Display for FileParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileParams::Source { source, path, .. } => {
                write!(f, "Source(source={source}, path={path})")
            }
            FileParams::File { path, .. } => {
                write!(f, "File(path={path})")
            }
            FileParams::FileAbsent { path } => {
                write!(f, "FileAbsent(path={path})")
            }
            FileParams::Directory { path, .. } => {
                write!(f, "Directory(path={path})")
            }
            FileParams::DirectoryAbsent { path } => {
                write!(f, "DirectoryAbsent(path={path})")
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
    Mode { path: FilePath, mode: FileMode },
    User { path: FilePath, user: FileUser },
    Group { path: FilePath, group: FileGroup },
}

impl Display for FileResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileResource::FileSource { source, path, .. } => {
                write!(f, "FileSource({source} -> {path})")
            }
            FileResource::FilePresent { path, .. } => write!(f, "FilePresent({path})"),
            FileResource::FileAbsent { path } => {
                write!(f, "FileAbsent({path})")
            }
            FileResource::DirectoryPresent { path, .. } => {
                write!(f, "DirectoryPresent({path})")
            }
            FileResource::DirectoryAbsent { path } => {
                write!(f, "DirectoryAbsent({path})")
            }
            FileResource::Mode { path, mode } => write!(f, "FileMode({path}, mode = {mode})"),
            FileResource::User { path, user } => write!(f, "FileUser({path}, user = {user})"),
            FileResource::Group { path, group } => write!(f, "FileGroup({path}, group = {group})"),
        }
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
    UserCorrect,
    UserIncorrect,
    GroupCorrect,
    GroupIncorrect,
}

impl Display for FileState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use FileState::*;
        let text = match self {
            FileSourced => "FileSourced",
            FilePresent => "FilePresent",
            FileAbsent => "FileAbsent",
            DirectoryPresent => "DirectoryPresent",
            DirectoryAbsent => "DirectoryAbsent",
            ModeCorrect => "ModeCorrect",
            ModeIncorrect => "ModeIncorrect",
            UserCorrect => "UserCorrect",
            UserIncorrect => "UserIncorrect",
            GroupCorrect => "GroupCorrect",
            GroupIncorrect => "GroupIncorrect",
        };

        write!(f, "{}", text)
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

        let path = |ty| Spanned::new(ParamField::new(ty), span.clone());

        Some(Spanned::new(
            ParamTypes::Union(vec![
                indexmap! {
                    "type".to_string() => path(ParamType::Literal("source".into())),
                    "source".to_string() => path(ParamType::String),
                    "path".to_string() => path(ParamType::String),
                    "mode".to_string() => path(ParamType::Number),
                    "user".to_string() => path(ParamType::String),
                    "group".to_string() => path(ParamType::String),
                },
                indexmap! {
                    "type".to_string() => path(ParamType::Literal("file".into())),
                    "path".to_string() => path(ParamType::String),
                    "mode".to_string() => path(ParamType::Number),
                    "user".to_string() => path(ParamType::String),
                    "group".to_string() => path(ParamType::String),
                },
                indexmap! {
                    "type".to_string() => path(ParamType::Literal("file-absent".into())),
                    "path".to_string() => path(ParamType::String),
                },
                indexmap! {
                    "type".to_string() => path(ParamType::Literal("directory".into())),
                    "path".to_string() => path(ParamType::String),
                    "mode".to_string() => path(ParamType::Number),
                    "user".to_string() => path(ParamType::String),
                    "group".to_string() => path(ParamType::String),
                },
                indexmap! {
                    "type".to_string() => path(ParamType::Literal("directory-files".into())),
                    "path".to_string() => path(ParamType::String),
                    "mode".to_string() => path(ParamType::Number),
                    "user".to_string() => path(ParamType::String),
                    "group".to_string() => path(ParamType::String),
                },
                indexmap! {
                    "type".to_string() => path(ParamType::Literal("directory-absent".into())),
                    "path".to_string() => path(ParamType::String),
                },
            ]),
            span,
        ))
    }

    type Params = FileParams;
    type Resource = FileResource;
    /*
    FileSource { source: FilePath, path: FilePath },
    FilePresent { path: FilePath },
    FileAbsent { path: FilePath },
    DirectoryPresent { path: FilePath },
    DirectoryAbsent { path: FilePath },
    Mode { path: FilePath, mode: FileMode },
    User { path: FilePath, user: FileGroup },
    Group { path: FilePath, group: FileGroup },
    */

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        match params {
            FileParams::Source {
                source,
                path,
                mode,
                user,
                group,
            } => vec![
                CausalityTree::leaf(
                    CausalityMeta::id("file".into()),
                    FileResource::FileSource { source, path },
                ),
                CausalityTree::leaf(
                    CausalityMeta::before(vec!["file".into()]),
                    FileResource::Mode { path, mode },
                ),
                CausalityTree::leaf(
                    CausalityMeta::before(vec!["file".into()]),
                    FileResource::User { path, user },
                ),
                CausalityTree::leaf(
                    CausalityMeta::before(vec!["file".into()]),
                    FileResource::Group { path, group },
                ),
            ],
            FileParams::File {
                path,
                mode,
                user,
                group,
            } => vec![
                CausalityTree::leaf(
                    CausalityMeta::id("file".into()),
                    FileResource::FilePresent { path },
                ),
                CausalityTree::leaf(
                    CausalityMeta::before(vec!["file".into()]),
                    FileResource::Mode { path, mode },
                ),
                CausalityTree::leaf(
                    CausalityMeta::before(vec!["file".into()]),
                    FileResource::User { path, user },
                ),
                CausalityTree::leaf(
                    CausalityMeta::before(vec!["file".into()]),
                    FileResource::Group { path, group },
                ),
            ],
            FileParams::FileAbsent { path } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                FileResource::FileAbsent { path },
            )],
            FileParams::Directory {
                path,
                mode,
                user,
                group,
            } => vec![
                CausalityTree::leaf(
                    CausalityMeta::id("directory".into()),
                    FileResource::DirectoryPresent { path },
                ),
                CausalityTree::leaf(
                    CausalityMeta::before(vec!["directory".into()]),
                    FileResource::Mode { path, mode },
                ),
                CausalityTree::leaf(
                    CausalityMeta::before(vec!["directory".into()]),
                    FileResource::User { path, user },
                ),
                CausalityTree::leaf(
                    CausalityMeta::before(vec!["directory".into()]),
                    FileResource::Group { path, group },
                ),
            ],
            FileParams::DirectoryAbsent { path } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                FileResource::DirectoryAbsent { path },
            )],
        }
    }

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
