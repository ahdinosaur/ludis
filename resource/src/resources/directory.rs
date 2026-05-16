use std::fmt::{self, Display};

use async_trait::async_trait;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_ctx::Context;
use lusid_fs::{self as fs, FsError};
use lusid_operation::{
    Operation,
    operations::{
        directory::DirectoryOperation,
        file::{FileGroup, FileMode, FilePath, FileUser},
    },
};
use lusid_params::{ParseError, ParseParams, StructFields};
use lusid_view::impl_display_render;
use rimu::{Span, Spanned, Value};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::ResourceType;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DirectoryParams {
    /// Recursive copy of the directory tree at `source` into `path`. Edits
    /// to `source` only propagate on the next apply. The state probe is
    /// intentionally weak (existence-as-directory at `path` ⇒ `Sourced`);
    /// content drift in `source` after first apply is not detected - declare
    /// `state: "absent"` and re-apply to force a refresh.
    /// Note(cc): a content-aware recursive diff is a follow-up; see Salt's
    /// `file.recurse`.
    Sourced {
        source: FilePath,
        /// Span of the `source` value in the plan source. Carried so
        /// host-path validation errors can point at the offending line.
        /// Skipped on the wire: validation runs pre-emit, so the span
        /// is unused downstream.
        #[serde(skip, default)]
        source_span: Span,
        path: FilePath,
        mode: Option<FileMode>,
        user: Option<FileUser>,
        group: Option<FileGroup>,
    },

    /// Materialise `path` as a symlink to the directory at `source` (a
    /// host-path on the machine running apply). Mirror of
    /// [`FileParams::Linked`](super::file::FileParams::Linked); same rationale
    /// for refusing `mode`/`user`/`group` here at the parser level, and
    /// same `Note(cc)` about absolute symlink targets - see
    /// [`FileParams::Linked`](super::file::FileParams::Linked) for the
    /// relative-target follow-up.
    Linked {
        source: FilePath,
        /// Span of the `source` value in the plan source. See
        /// [`DirectoryParams::Sourced::source_span`] for rationale.
        #[serde(skip, default)]
        source_span: Span,
        path: FilePath,
    },

    Present {
        path: FilePath,
        mode: Option<FileMode>,
        user: Option<FileUser>,
        group: Option<FileGroup>,
    },
    Absent {
        path: FilePath,
    },
}

impl ParseParams for DirectoryParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let state =
            fields.take_discriminator("state", &["sourced", "linked", "present", "absent"])?;
        let out = match state {
            "sourced" => {
                let (source_path, source_span) =
                    fields.required_host_path_spanned("source")?.take();
                DirectoryParams::Sourced {
                    source: FilePath::new(source_path.to_string_lossy().into_owned()),
                    source_span,
                    path: FilePath::new(fields.required_target_path("path")?),
                    mode: fields.optional_u32("mode")?.map(FileMode::new),
                    user: fields.optional_string("user")?.map(FileUser::new),
                    group: fields.optional_string("group")?.map(FileGroup::new),
                }
            }
            "linked" => {
                let (source_path, source_span) =
                    fields.required_host_path_spanned("source")?.take();
                DirectoryParams::Linked {
                    source: FilePath::new(source_path.to_string_lossy().into_owned()),
                    source_span,
                    path: FilePath::new(fields.required_target_path("path")?),
                }
            }
            "present" => DirectoryParams::Present {
                path: FilePath::new(fields.required_target_path("path")?),
                mode: fields.optional_u32("mode")?.map(FileMode::new),
                user: fields.optional_string("user")?.map(FileUser::new),
                group: fields.optional_string("group")?.map(FileGroup::new),
            },
            "absent" => DirectoryParams::Absent {
                path: FilePath::new(fields.required_target_path("path")?),
            },
            _ => unreachable!(),
        };
        fields.finish()?;
        Ok(out)
    }
}

impl Display for DirectoryParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DirectoryParams::Sourced { source, path, .. } => {
                write!(f, "Directory::Sourced(source = {source}, path = {path})")
            }
            DirectoryParams::Linked { source, path, .. } => {
                write!(f, "Directory::Linked(source = {source}, path = {path})")
            }
            DirectoryParams::Present { path, .. } => write!(f, "Directory::Present(path = {path})"),
            DirectoryParams::Absent { path } => write!(f, "Directory::Absent(path = {path})"),
        }
    }
}

impl_display_render!(DirectoryParams);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DirectoryResource {
    Sourced { source: FilePath, path: FilePath },
    Linked { source: FilePath, path: FilePath },
    Present { path: FilePath },
    Absent { path: FilePath },
    Mode { path: FilePath, mode: FileMode },
    User { path: FilePath, user: FileUser },
    Group { path: FilePath, group: FileGroup },
}

impl Display for DirectoryResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DirectoryResource::Sourced { source, path } => {
                write!(f, "DirectorySourced({source} -> {path})")
            }
            DirectoryResource::Linked { source, path } => {
                write!(f, "DirectoryLinked({source} -> {path})")
            }
            DirectoryResource::Present { path } => write!(f, "DirectoryPresent({path})"),
            DirectoryResource::Absent { path } => write!(f, "DirectoryAbsent({path})"),
            DirectoryResource::Mode { path, mode } => {
                write!(f, "DirectoryMode({path}, mode = {mode})")
            }
            DirectoryResource::User { path, user } => {
                write!(f, "DirectoryUser({path}, user = {user})")
            }
            DirectoryResource::Group { path, group } => {
                write!(f, "DirectoryGroup({path}, group = {group})")
            }
        }
    }
}

impl_display_render!(DirectoryResource);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DirectoryState {
    Sourced,
    NotSourced,
    Linked,
    NotLinked,
    Present,
    Absent,
    ModeCorrect,
    ModeIncorrect,
    UserCorrect,
    UserIncorrect,
    GroupCorrect,
    GroupIncorrect,
}

impl Display for DirectoryState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use DirectoryState::*;
        let text = match self {
            Sourced => "Sourced",
            NotSourced => "NotSourced",
            Linked => "Linked",
            NotLinked => "NotLinked",
            Present => "Present",
            Absent => "Absent",
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

impl_display_render!(DirectoryState);

#[derive(Error, Debug)]
pub enum DirectoryStateError {
    #[error(transparent)]
    Fs(#[from] FsError),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DirectoryChange {
    Create {
        path: FilePath,
    },
    /// Materialise `path` as a symlink to `source` - emitted for
    /// `state: "linked"`.
    CreateSymlink {
        source: FilePath,
        path: FilePath,
    },
    /// Recursively copy `source` to `path` - emitted for `state: "sourced"`.
    CopyTree {
        source: FilePath,
        path: FilePath,
    },
    Remove {
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

impl Display for DirectoryChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DirectoryChange::Create { path } => {
                write!(f, "Directory::Create(path = {path})")
            }
            DirectoryChange::CreateSymlink { source, path } => {
                write!(
                    f,
                    "Directory::CreateSymlink(source = {source}, path = {path})"
                )
            }
            DirectoryChange::CopyTree { source, path } => {
                write!(f, "Directory::CopyTree(source = {source}, path = {path})")
            }
            DirectoryChange::Remove { path } => {
                write!(f, "Directory::Remove(path = {path})")
            }
            DirectoryChange::ChangeMode { path, mode } => {
                write!(f, "Directory::ChangeMode(path = {path}, mode = {mode})")
            }
            DirectoryChange::ChangeOwner { path, user, group } => {
                write!(
                    f,
                    "Directory::ChangeOwner(path = {path}, user = {user:?}, group = {group:?})"
                )
            }
        }
    }
}

impl_display_render!(DirectoryChange);

#[derive(Debug, Clone)]
pub struct Directory;

#[async_trait]
impl ResourceType for Directory {
    const ID: &'static str = "directory";

    type Params = DirectoryParams;
    type Resource = DirectoryResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        // Mode/User/Group sub-atoms are common to `Sourced` and `Present`
        // (Linked rejects them at parse time, so it never reaches here).
        fn permission_atoms(
            path: &FilePath,
            mode: Option<FileMode>,
            user: Option<FileUser>,
            group: Option<FileGroup>,
        ) -> Vec<CausalityTree<DirectoryResource>> {
            let mut nodes = Vec::new();
            if let Some(mode) = mode {
                nodes.push(CausalityTree::leaf(
                    CausalityMeta::requires(vec!["directory".into()]),
                    DirectoryResource::Mode {
                        path: path.clone(),
                        mode,
                    },
                ));
            }
            if let Some(user) = user {
                nodes.push(CausalityTree::leaf(
                    CausalityMeta::requires(vec!["directory".into()]),
                    DirectoryResource::User {
                        path: path.clone(),
                        user,
                    },
                ));
            }
            if let Some(group) = group {
                nodes.push(CausalityTree::leaf(
                    CausalityMeta::requires(vec!["directory".into()]),
                    DirectoryResource::Group {
                        path: path.clone(),
                        group,
                    },
                ));
            }
            nodes
        }

        match params {
            DirectoryParams::Sourced {
                source,
                source_span: _,
                path,
                mode,
                user,
                group,
            } => {
                let mut nodes = vec![CausalityTree::leaf(
                    CausalityMeta::id("directory".into()),
                    DirectoryResource::Sourced {
                        source,
                        path: path.clone(),
                    },
                )];
                nodes.extend(permission_atoms(&path, mode, user, group));
                nodes
            }

            DirectoryParams::Linked {
                source,
                source_span: _,
                path,
            } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                DirectoryResource::Linked { source, path },
            )],

            DirectoryParams::Present {
                path,
                mode,
                user,
                group,
            } => {
                let mut nodes = vec![CausalityTree::leaf(
                    CausalityMeta::id("directory".into()),
                    DirectoryResource::Present { path: path.clone() },
                )];
                nodes.extend(permission_atoms(&path, mode, user, group));
                nodes
            }

            DirectoryParams::Absent { path } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                DirectoryResource::Absent { path },
            )],
        }
    }

    type State = DirectoryState;
    type StateError = DirectoryStateError;

    async fn state(
        _ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        let state = match resource {
            DirectoryResource::Sourced { path, .. } => {
                // Weak: a directory at `path` is taken to mean Sourced. See
                // the variant docstring in `DirectoryParams::Sourced` for
                // the content-drift caveat.
                if fs::path_exists(path.as_path()).await? {
                    DirectoryState::Sourced
                } else {
                    DirectoryState::NotSourced
                }
            }

            DirectoryResource::Linked { source, path } => probe_linked_state(source, path).await?,

            DirectoryResource::Present { path } | DirectoryResource::Absent { path } => {
                if fs::path_exists(path.as_path()).await? {
                    DirectoryState::Present
                } else {
                    DirectoryState::Absent
                }
            }

            DirectoryResource::Mode { path, mode } => {
                if !fs::path_exists(path.as_path()).await? {
                    DirectoryState::ModeIncorrect
                } else {
                    let actual_mode = fs::get_mode(path.as_path()).await?;
                    let actual_mode = actual_mode & 0o7777;
                    if actual_mode == mode.as_u32() {
                        DirectoryState::ModeCorrect
                    } else {
                        DirectoryState::ModeIncorrect
                    }
                }
            }

            DirectoryResource::User { path, user } => {
                if !fs::path_exists(path.as_path()).await? {
                    DirectoryState::UserIncorrect
                } else {
                    let actual_user = fs::get_owner_user(path.as_path()).await?;
                    let actual_user = actual_user.map(|u| u.name.to_string());
                    if actual_user.as_deref() == Some(user.as_str()) {
                        DirectoryState::UserCorrect
                    } else {
                        DirectoryState::UserIncorrect
                    }
                }
            }

            DirectoryResource::Group { path, group } => {
                if !fs::path_exists(path.as_path()).await? {
                    DirectoryState::GroupIncorrect
                } else {
                    let actual_group = fs::get_owner_group(path.as_path()).await?;
                    let actual_group = actual_group.map(|g| g.name.to_string());
                    if actual_group.as_deref() == Some(group.as_str()) {
                        DirectoryState::GroupCorrect
                    } else {
                        DirectoryState::GroupIncorrect
                    }
                }
            }
        };

        Ok(state)
    }

    type Change = DirectoryChange;

    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        match (resource, state) {
            (DirectoryResource::Sourced { source, path }, DirectoryState::NotSourced) => {
                Some(DirectoryChange::CopyTree {
                    source: source.clone(),
                    path: path.clone(),
                })
            }

            (DirectoryResource::Sourced { .. }, DirectoryState::Sourced) => None,

            (DirectoryResource::Linked { source, path }, DirectoryState::NotLinked) => {
                Some(DirectoryChange::CreateSymlink {
                    source: source.clone(),
                    path: path.clone(),
                })
            }

            (DirectoryResource::Linked { .. }, DirectoryState::Linked) => None,

            (DirectoryResource::Present { path }, DirectoryState::Absent) => {
                Some(DirectoryChange::Create { path: path.clone() })
            }

            (DirectoryResource::Present { .. }, DirectoryState::Present) => None,

            (DirectoryResource::Absent { path }, DirectoryState::Present) => {
                Some(DirectoryChange::Remove { path: path.clone() })
            }

            (DirectoryResource::Absent { .. }, DirectoryState::Absent) => None,

            (DirectoryResource::Mode { path, mode }, DirectoryState::ModeIncorrect) => {
                Some(DirectoryChange::ChangeMode {
                    path: path.clone(),
                    mode: *mode,
                })
            }

            (DirectoryResource::Mode { .. }, DirectoryState::ModeCorrect) => None,

            (DirectoryResource::User { path, user }, DirectoryState::UserIncorrect) => {
                Some(DirectoryChange::ChangeOwner {
                    path: path.clone(),
                    user: Some(user.clone()),
                    group: None,
                })
            }

            (DirectoryResource::User { .. }, DirectoryState::UserCorrect) => None,

            (DirectoryResource::Group { path, group }, DirectoryState::GroupIncorrect) => {
                Some(DirectoryChange::ChangeOwner {
                    path: path.clone(),
                    user: None,
                    group: Some(group.clone()),
                })
            }

            (DirectoryResource::Group { .. }, DirectoryState::GroupCorrect) => None,

            _ => {
                // TODO (mw): Return an error. Which means changing the trait's change method.
                // Or, alternatively, we have separate resources for each case, so there's no
                // possible mismatch.
                panic!("Unexpected case in change method for Directory resource.")
            }
        }
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        let op = match change {
            DirectoryChange::Create { path } => {
                Operation::Directory(DirectoryOperation::Create { path })
            }
            DirectoryChange::CreateSymlink { source, path } => {
                Operation::Directory(DirectoryOperation::CreateSymlink { source, path })
            }
            DirectoryChange::CopyTree { source, path } => {
                Operation::Directory(DirectoryOperation::CopyTree { source, path })
            }
            DirectoryChange::Remove { path } => {
                Operation::Directory(DirectoryOperation::Remove { path })
            }
            DirectoryChange::ChangeMode { path, mode } => {
                Operation::Directory(DirectoryOperation::ChangeMode { path, mode })
            }
            DirectoryChange::ChangeOwner { path, user, group } => {
                Operation::Directory(DirectoryOperation::ChangeOwner { path, user, group })
            }
        };

        vec![CausalityTree::leaf(CausalityMeta::default(), op)]
    }
}

/// Probe `path` for whether it's a symlink with `source` as its lexical
/// target. Mirror of [`super::file::probe_linked_state`] - see the
/// non-canonicalisation rationale there.
async fn probe_linked_state(
    source: &FilePath,
    path: &FilePath,
) -> Result<DirectoryState, DirectoryStateError> {
    match fs::probe_symlink(path.as_path()).await? {
        fs::SymlinkTarget::Symlink(target) if target == source.as_path() => {
            Ok(DirectoryState::Linked)
        }
        _ => Ok(DirectoryState::NotLinked),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    fn file_path(p: &std::path::Path) -> FilePath {
        FilePath::new(p.to_string_lossy().into_owned())
    }

    // --- Sourced state probe (existence-as-directory) -------------------

    #[tokio::test]
    async fn sourced_existing_dir_reports_sourced_weakly() {
        // Pinning the deliberate weakness: a directory at `path` is taken
        // to mean "already sourced" regardless of content drift in `source`.
        let dir = tempdir().unwrap();
        let source = dir.path().join("src");
        tokio::fs::create_dir(&source).await.unwrap();
        let target = dir.path().join("dest");
        tokio::fs::create_dir(&target).await.unwrap();
        tokio::fs::write(source.join("only-in-source.txt"), b"x")
            .await
            .unwrap();

        let resource = DirectoryResource::Sourced {
            source: file_path(&source),
            path: file_path(&target),
        };
        let mut ctx = lusid_ctx::Context::create(dir.path()).unwrap();
        let state = Directory::state(&mut ctx, &resource).await.unwrap();
        assert!(matches!(state, DirectoryState::Sourced));
    }

    #[tokio::test]
    async fn sourced_missing_path_reports_not_sourced() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src");
        tokio::fs::create_dir(&source).await.unwrap();
        let target = dir.path().join("dest");

        let resource = DirectoryResource::Sourced {
            source: file_path(&source),
            path: file_path(&target),
        };
        let mut ctx = lusid_ctx::Context::create(dir.path()).unwrap();
        let state = Directory::state(&mut ctx, &resource).await.unwrap();
        assert!(matches!(state, DirectoryState::NotSourced));
    }

    // --- Linked state probe (lexical symlink target) --------------------

    #[tokio::test]
    async fn linked_correct_dir_symlink_reports_linked() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src");
        tokio::fs::create_dir(&source).await.unwrap();
        let target = dir.path().join("link");
        tokio::fs::symlink(&source, &target).await.unwrap();

        let state = probe_linked_state(&file_path(&source), &file_path(&target))
            .await
            .unwrap();
        assert!(matches!(state, DirectoryState::Linked));
    }

    #[tokio::test]
    async fn linked_real_directory_reports_not_linked() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src");
        tokio::fs::create_dir(&source).await.unwrap();
        let target = dir.path().join("dest");
        tokio::fs::create_dir(&target).await.unwrap();

        let state = probe_linked_state(&file_path(&source), &file_path(&target))
            .await
            .unwrap();
        assert!(matches!(state, DirectoryState::NotLinked));
    }

    #[tokio::test]
    async fn linked_wrong_symlink_target_reports_not_linked() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src");
        let other = dir.path().join("other");
        tokio::fs::create_dir(&source).await.unwrap();
        tokio::fs::create_dir(&other).await.unwrap();
        let target = dir.path().join("link");
        tokio::fs::symlink(&other, &target).await.unwrap();

        let state = probe_linked_state(&file_path(&source), &file_path(&target))
            .await
            .unwrap();
        assert!(matches!(state, DirectoryState::NotLinked));
    }

    #[tokio::test]
    async fn linked_missing_path_reports_not_linked() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src");
        tokio::fs::create_dir(&source).await.unwrap();
        let target = dir.path().join("missing");

        let state = probe_linked_state(&file_path(&source), &file_path(&target))
            .await
            .unwrap();
        assert!(matches!(state, DirectoryState::NotLinked));
    }

    // --- Change-emission table -----------------------------------------

    #[test]
    fn change_for_sourced_not_sourced_emits_copy_tree() {
        let resource = DirectoryResource::Sourced {
            source: FilePath::new("/host/src"),
            path: FilePath::new("/target/dest"),
        };
        let change =
            Directory::change(&resource, &DirectoryState::NotSourced).expect("some change");
        match change {
            DirectoryChange::CopyTree { source, path } => {
                assert_eq!(source.as_path(), std::path::Path::new("/host/src"));
                assert_eq!(path.as_path(), std::path::Path::new("/target/dest"));
            }
            other => panic!("expected CopyTree, got {other:?}"),
        }
    }

    #[test]
    fn change_for_linked_not_linked_emits_create_symlink() {
        let resource = DirectoryResource::Linked {
            source: FilePath::new("/host/src"),
            path: FilePath::new("/target/dest"),
        };
        let change = Directory::change(&resource, &DirectoryState::NotLinked).expect("some change");
        match change {
            DirectoryChange::CreateSymlink { source, path } => {
                assert_eq!(source.as_path(), std::path::Path::new("/host/src"));
                assert_eq!(path.as_path(), std::path::Path::new("/target/dest"));
            }
            other => panic!("expected CreateSymlink, got {other:?}"),
        }
    }
}

#[cfg(test)]
mod serde_tests {
    use super::*;

    fn round_trip<T: serde::Serialize + serde::de::DeserializeOwned>(value: &T) {
        let json = serde_json::to_string(value).unwrap();
        let back: T = serde_json::from_str(&json).unwrap();
        assert_eq!(json, serde_json::to_string(&back).unwrap());
    }

    #[test]
    fn params_round_trip() {
        // `source_span` is `#[serde(skip, default)]`; it does not survive the
        // wire and the round-tripped value contains a default Span.
        round_trip(&DirectoryParams::Sourced {
            source: FilePath::new("/host/src"),
            source_span: Span::default(),
            path: FilePath::new("/target/dest"),
            mode: Some(FileMode::new(0o755)),
            user: None,
            group: None,
        });
        round_trip(&DirectoryParams::Linked {
            source: FilePath::new("/host/src"),
            source_span: Span::default(),
            path: FilePath::new("/target/dest"),
        });
        round_trip(&DirectoryParams::Present {
            path: FilePath::new("/target/dest"),
            mode: None,
            user: None,
            group: None,
        });
        round_trip(&DirectoryParams::Absent {
            path: FilePath::new("/target/dest"),
        });
    }

    #[test]
    fn resource_round_trip_covers_every_variant() {
        let path = FilePath::new("/target/dest");
        round_trip(&DirectoryResource::Sourced {
            source: FilePath::new("/host/src"),
            path: path.clone(),
        });
        round_trip(&DirectoryResource::Linked {
            source: FilePath::new("/host/src"),
            path: path.clone(),
        });
        round_trip(&DirectoryResource::Present { path: path.clone() });
        round_trip(&DirectoryResource::Absent { path: path.clone() });
        round_trip(&DirectoryResource::Mode {
            path: path.clone(),
            mode: FileMode::new(0o755),
        });
        round_trip(&DirectoryResource::User {
            path: path.clone(),
            user: FileUser::new("root"),
        });
        round_trip(&DirectoryResource::Group {
            path,
            group: FileGroup::new("wheel"),
        });
    }

    #[test]
    fn state_round_trip_covers_every_variant() {
        for state in [
            DirectoryState::Sourced,
            DirectoryState::NotSourced,
            DirectoryState::Linked,
            DirectoryState::NotLinked,
            DirectoryState::Present,
            DirectoryState::Absent,
            DirectoryState::ModeCorrect,
            DirectoryState::ModeIncorrect,
            DirectoryState::UserCorrect,
            DirectoryState::UserIncorrect,
            DirectoryState::GroupCorrect,
            DirectoryState::GroupIncorrect,
        ] {
            round_trip(&state);
        }
    }

    #[test]
    fn change_round_trip_covers_every_variant() {
        let path = FilePath::new("/target/dest");
        round_trip(&DirectoryChange::Create { path: path.clone() });
        round_trip(&DirectoryChange::CreateSymlink {
            source: FilePath::new("/host/src"),
            path: path.clone(),
        });
        round_trip(&DirectoryChange::CopyTree {
            source: FilePath::new("/host/src"),
            path: path.clone(),
        });
        round_trip(&DirectoryChange::Remove { path: path.clone() });
        round_trip(&DirectoryChange::ChangeMode {
            path: path.clone(),
            mode: FileMode::new(0o755),
        });
        round_trip(&DirectoryChange::ChangeOwner {
            path,
            user: Some(FileUser::new("root")),
            group: None,
        });
    }
}
