use std::fmt::{self, Display};

use async_trait::async_trait;
use indexmap::indexmap;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_ctx::Context;
use lusid_fs::{self as fs, FsError};
use lusid_operation::{
    operations::file::{FileGroup, FileMode, FileOperation, FilePath, FileSource, FileUser},
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
        mode: Option<FileMode>,
        user: Option<FileUser>,
        group: Option<FileGroup>,
    },
    File {
        path: FilePath,
        mode: Option<FileMode>,
        user: Option<FileUser>,
        group: Option<FileGroup>,
    },
    FileAbsent {
        path: FilePath,
    },
    Directory {
        path: FilePath,
        mode: Option<FileMode>,
        user: Option<FileUser>,
        group: Option<FileGroup>,
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
            FileParams::File { path, .. } => write!(f, "File(path={path})"),
            FileParams::FileAbsent { path } => write!(f, "FileAbsent(path={path})"),
            FileParams::Directory { path, .. } => write!(f, "Directory(path={path})"),
            FileParams::DirectoryAbsent { path } => write!(f, "DirectoryAbsent(path={path})"),
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
            FileResource::FileSource { source, path } => {
                write!(f, "FileSource({source} -> {path})")
            }
            FileResource::FilePresent { path } => write!(f, "FilePresent({path})"),
            FileResource::FileAbsent { path } => write!(f, "FileAbsent({path})"),
            FileResource::DirectoryPresent { path } => write!(f, "DirectoryPresent({path})"),
            FileResource::DirectoryAbsent { path } => write!(f, "DirectoryAbsent({path})"),
            FileResource::Mode { path, mode } => write!(f, "FileMode({path}, mode = {mode})"),
            FileResource::User { path, user } => write!(f, "FileUser({path}, user = {user})"),
            FileResource::Group { path, group } => write!(f, "FileGroup({path}, group = {group})"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FileState {
    FileSourced,
    FileNotSourced,
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
            FileNotSourced => "FileNotSourced",
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
        write!(f, "{text}")
    }
}

#[derive(Error, Debug)]
pub enum FileStateError {
    #[error(transparent)]
    Fs(#[from] FsError),
}

#[derive(Debug, Clone)]
pub enum FileChange {
    WriteFile {
        path: FilePath,
        source: FileSource,
    },
    RemoveFile {
        path: FilePath,
    },
    CreateDirectory {
        path: FilePath,
    },
    RemoveDirectory {
        path: FilePath,
    },
    ChangeMode {
        path: FilePath,
        mode: FileMode,
    },
    ChangeOwner {
        path: FilePath,
        user: Option<FileUser>,
        group: Option<FileGroup>,
    },
}

impl Display for FileChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileChange::WriteFile { path, source } => match source {
                FileSource::Contents(contents) => write!(
                    f,
                    "File::WriteFile(path = {}, source = Contents({} bytes))",
                    path,
                    contents.len()
                ),
                FileSource::Path(source_path) => write!(
                    f,
                    "File::WriteFile(path = {}, source = Path({}))",
                    path, source_path
                ),
            },
            FileChange::RemoveFile { path } => write!(f, "File::RemoveFile(path = {path})"),
            FileChange::CreateDirectory { path } => {
                write!(f, "File::CreateDirectory(path = {path})")
            }
            FileChange::RemoveDirectory { path } => {
                write!(f, "File::RemoveDirectory(path = {path})")
            }
            FileChange::ChangeMode { path, mode } => {
                write!(f, "File::ChangeMode(path = {path}, mode = {mode})")
            }
            FileChange::ChangeOwner { path, user, group } => write!(
                f,
                "File::ChangeOwner(path = {path}, user = {user:?}, group = {group:?})"
            ),
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
        let field = |ty, required: bool| {
            let mut param = ParamField::new(ty);
            if !required {
                param = param.with_optional();
            }
            Spanned::new(param, span.clone())
        };

        Some(Spanned::new(
            ParamTypes::Union(vec![
                indexmap! {
                  "type".to_string() => field(ParamType::Literal("source".into()), true),
                  "source".to_string() => field(ParamType::HostPath, true),
                  "path".to_string() => field(ParamType::TargetPath, true),
                  "mode".to_string() => field(ParamType::Number, false),
                  "user".to_string() => field(ParamType::String, false),
                  "group".to_string() => field(ParamType::String, false),
                },
                indexmap! {
                  "type".to_string() => field(ParamType::Literal("file".into()), true),
                  "path".to_string() => field(ParamType::TargetPath, true),
                  "mode".to_string() => field(ParamType::Number, false),
                  "user".to_string() => field(ParamType::String, false),
                  "group".to_string() => field(ParamType::String, false),
                },
                indexmap! {
                  "type".to_string() => field(ParamType::Literal("file-absent".into()), true),
                  "path".to_string() => field(ParamType::TargetPath, true),
                },
                indexmap! {
                  "type".to_string() => field(ParamType::Literal("directory".into()), true),
                  "path".to_string() => field(ParamType::TargetPath, true),
                  "mode".to_string() => field(ParamType::Number, false),
                  "user".to_string() => field(ParamType::String, false),
                  "group".to_string() => field(ParamType::String, false),
                },
                indexmap! {
                  "type".to_string() => field(ParamType::Literal("directory-absent".into()), true),
                  "path".to_string() => field(ParamType::TargetPath, true),
                },
            ]),
            span,
        ))
    }

    type Params = FileParams;
    type Resource = FileResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        match params {
            FileParams::Source {
                source,
                path,
                mode,
                user,
                group,
            } => {
                let mut nodes = vec![CausalityTree::leaf(
                    CausalityMeta::id("file".into()),
                    FileResource::FileSource {
                        source,
                        path: path.clone(),
                    },
                )];

                if let Some(mode) = mode {
                    nodes.push(CausalityTree::leaf(
                        CausalityMeta::before(vec!["file".into()]),
                        FileResource::Mode {
                            path: path.clone(),
                            mode,
                        },
                    ));
                }

                if let Some(user) = user {
                    nodes.push(CausalityTree::leaf(
                        CausalityMeta::before(vec!["file".into()]),
                        FileResource::User {
                            path: path.clone(),
                            user,
                        },
                    ))
                }

                if let Some(group) = group {
                    nodes.push(CausalityTree::leaf(
                        CausalityMeta::before(vec!["file".into()]),
                        FileResource::Group { path, group },
                    ));
                }

                nodes
            }

            FileParams::File {
                path,
                mode,
                user,
                group,
            } => {
                let mut nodes = vec![CausalityTree::leaf(
                    CausalityMeta::id("file".into()),
                    FileResource::FilePresent { path: path.clone() },
                )];

                if let Some(mode) = mode {
                    nodes.push(CausalityTree::leaf(
                        CausalityMeta::before(vec!["file".into()]),
                        FileResource::Mode {
                            path: path.clone(),
                            mode,
                        },
                    ));
                }

                if let Some(user) = user {
                    nodes.push(CausalityTree::leaf(
                        CausalityMeta::before(vec!["file".into()]),
                        FileResource::User {
                            path: path.clone(),
                            user,
                        },
                    ));
                }

                if let Some(group) = group {
                    nodes.push(CausalityTree::leaf(
                        CausalityMeta::before(vec!["file".into()]),
                        FileResource::Group { path, group },
                    ));
                }

                nodes
            }

            FileParams::FileAbsent { path } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                FileResource::FileAbsent { path },
            )],

            FileParams::Directory {
                path,
                mode,
                user,
                group,
            } => {
                let mut nodes = vec![CausalityTree::leaf(
                    CausalityMeta::id("directory".into()),
                    FileResource::DirectoryPresent { path: path.clone() },
                )];

                if let Some(mode) = mode {
                    nodes.push(CausalityTree::leaf(
                        CausalityMeta::before(vec!["directory".into()]),
                        FileResource::Mode {
                            path: path.clone(),
                            mode,
                        },
                    ));
                }

                if let Some(user) = user {
                    nodes.push(CausalityTree::leaf(
                        CausalityMeta::before(vec!["directory".into()]),
                        FileResource::User {
                            path: path.clone(),
                            user,
                        },
                    ));
                }

                if let Some(group) = group {
                    nodes.push(CausalityTree::leaf(
                        CausalityMeta::before(vec!["directory".into()]),
                        FileResource::Group { path, group },
                    ));
                }

                nodes
            }

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
        let state = match resource {
            FileResource::FileSource { source, path } => {
                if !fs::path_exists(path.as_path()).await? {
                    FileState::FileNotSourced
                } else {
                    let source_contents = fs::read_file_to_string(source.as_path()).await?;
                    let path_contents = fs::read_file_to_string(path.as_path()).await?;
                    if source_contents == path_contents {
                        FileState::FileSourced
                    } else {
                        FileState::FileNotSourced
                    }
                }
            }

            FileResource::FilePresent { path } | FileResource::FileAbsent { path } => {
                if fs::path_exists(path.as_path()).await? {
                    FileState::FilePresent
                } else {
                    FileState::FileAbsent
                }
            }

            FileResource::DirectoryPresent { path } | FileResource::DirectoryAbsent { path } => {
                if fs::path_exists(path.as_path()).await? {
                    FileState::DirectoryPresent
                } else {
                    FileState::DirectoryAbsent
                }
            }

            FileResource::Mode { path, mode } => {
                if !fs::path_exists(path.as_path()).await? {
                    FileState::ModeIncorrect
                } else {
                    let actual_mode = fs::get_mode(path.as_path()).await?;
                    let actual_mode = actual_mode & 0o7777;
                    if actual_mode == mode.as_u32() {
                        FileState::ModeCorrect
                    } else {
                        FileState::ModeIncorrect
                    }
                }
            }

            FileResource::User { path, user } => {
                if !fs::path_exists(path.as_path()).await? {
                    FileState::UserIncorrect
                } else {
                    let actual_user = fs::get_owner_user(path.as_path()).await?;
                    let actual_user = actual_user.map(|u| u.name.to_string());
                    if actual_user.as_deref() == Some(user.as_str()) {
                        FileState::UserCorrect
                    } else {
                        FileState::UserIncorrect
                    }
                }
            }

            FileResource::Group { path, group } => {
                if !fs::path_exists(path.as_path()).await? {
                    FileState::GroupIncorrect
                } else {
                    let actual_group = fs::get_owner_group(path.as_path()).await?;
                    let actual_group = actual_group.map(|g| g.name.to_string());
                    if actual_group.as_deref() == Some(group.as_str()) {
                        FileState::GroupCorrect
                    } else {
                        FileState::GroupIncorrect
                    }
                }
            }
        };

        Ok(state)
    }

    type Change = FileChange;

    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        match (resource, state) {
            (FileResource::FileSource { source, path }, FileState::FileNotSourced) => {
                Some(FileChange::WriteFile {
                    path: path.clone(),
                    source: FileSource::Path(source.clone()),
                })
            }

            (FileResource::FileSource { .. }, FileState::FileSourced) => None,

            (FileResource::FilePresent { path }, FileState::FileAbsent) => {
                Some(FileChange::WriteFile {
                    path: path.clone(),
                    source: FileSource::Contents(Vec::new()),
                })
            }

            (FileResource::FilePresent { .. }, FileState::FilePresent) => None,

            (FileResource::FileAbsent { path }, FileState::FilePresent) => {
                Some(FileChange::RemoveFile { path: path.clone() })
            }

            (FileResource::FileAbsent { .. }, FileState::FileAbsent) => None,

            (FileResource::DirectoryPresent { path }, FileState::DirectoryAbsent) => {
                Some(FileChange::CreateDirectory { path: path.clone() })
            }

            (FileResource::DirectoryPresent { .. }, FileState::DirectoryPresent) => None,

            (FileResource::DirectoryAbsent { path }, FileState::DirectoryPresent) => {
                Some(FileChange::RemoveDirectory { path: path.clone() })
            }

            (FileResource::DirectoryAbsent { .. }, FileState::DirectoryAbsent) => None,

            (FileResource::Mode { path, mode }, FileState::ModeIncorrect) => {
                Some(FileChange::ChangeMode {
                    path: path.clone(),
                    mode: *mode,
                })
            }

            (FileResource::Mode { .. }, FileState::ModeCorrect) => None,

            (FileResource::User { path, user }, FileState::UserIncorrect) => {
                Some(FileChange::ChangeOwner {
                    path: path.clone(),
                    user: Some(user.clone()),
                    group: None,
                })
            }

            (FileResource::User { .. }, FileState::UserCorrect) => None,

            (FileResource::Group { path, group }, FileState::GroupIncorrect) => {
                Some(FileChange::ChangeOwner {
                    path: path.clone(),
                    user: None,
                    group: Some(group.clone()),
                })
            }

            (FileResource::Group { .. }, FileState::GroupCorrect) => None,

            _ => {
                // TODO (mw): Return an error. Which means changing the trait's change method.
                // Or, alternatively, we have separate resources for each case, so there's no
                // possible mismatch.
                panic!("Unexpected case in change method for File resource.")
            }
        }
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        let op = match change {
            FileChange::WriteFile { path, source } => {
                Operation::File(FileOperation::WriteFile { path, source })
            }
            FileChange::RemoveFile { path } => Operation::File(FileOperation::RemoveFile { path }),
            FileChange::CreateDirectory { path } => {
                Operation::File(FileOperation::CreateDirectory { path })
            }
            FileChange::RemoveDirectory { path } => {
                Operation::File(FileOperation::RemoveDirectory { path })
            }
            FileChange::ChangeMode { path, mode } => {
                Operation::File(FileOperation::ChangeMode { path, mode })
            }
            FileChange::ChangeOwner { path, user, group } => {
                Operation::File(FileOperation::ChangeOwner { path, user, group })
            }
        };

        vec![CausalityTree::leaf(CausalityMeta::default(), op)]
    }
}
