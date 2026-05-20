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
use rimu::{Span, Spanned, Value};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::{ChangeKind, ResourceChangeTrait, ResourceType};

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
        /// See [`super::file::FileParams::Sourced::sudo`]. Wraps the
        /// recursive copy in `sudo -n` (and the follow-up chmod/chown).
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
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
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },

    Present {
        path: FilePath,
        mode: Option<FileMode>,
        user: Option<FileUser>,
        group: Option<FileGroup>,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    Absent {
        path: FilePath,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
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
                    sudo: fields.optional_bool("sudo")?.unwrap_or(false),
                }
            }
            "linked" => {
                let (source_path, source_span) =
                    fields.required_host_path_spanned("source")?.take();
                DirectoryParams::Linked {
                    source: FilePath::new(source_path.to_string_lossy().into_owned()),
                    source_span,
                    path: FilePath::new(fields.required_target_path("path")?),
                    sudo: fields.optional_bool("sudo")?.unwrap_or(false),
                }
            }
            "present" => DirectoryParams::Present {
                path: FilePath::new(fields.required_target_path("path")?),
                mode: fields.optional_u32("mode")?.map(FileMode::new),
                user: fields.optional_string("user")?.map(FileUser::new),
                group: fields.optional_string("group")?.map(FileGroup::new),
                sudo: fields.optional_bool("sudo")?.unwrap_or(false),
            },
            "absent" => DirectoryParams::Absent {
                path: FilePath::new(fields.required_target_path("path")?),
                sudo: fields.optional_bool("sudo")?.unwrap_or(false),
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
            DirectoryParams::Absent { path, .. } => write!(f, "Directory::Absent(path = {path})"),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DirectoryResource {
    Sourced {
        source: FilePath,
        path: FilePath,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    Linked {
        source: FilePath,
        path: FilePath,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    Present {
        path: FilePath,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    Absent {
        path: FilePath,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    Mode {
        path: FilePath,
        mode: FileMode,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    User {
        path: FilePath,
        user: FileUser,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    Group {
        path: FilePath,
        group: FileGroup,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
}

impl Display for DirectoryResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DirectoryResource::Sourced { source, path, .. } => {
                write!(f, "DirectorySourced({source} -> {path})")
            }
            DirectoryResource::Linked { source, path, .. } => {
                write!(f, "DirectoryLinked({source} -> {path})")
            }
            DirectoryResource::Present { path, .. } => write!(f, "DirectoryPresent({path})"),
            DirectoryResource::Absent { path, .. } => write!(f, "DirectoryAbsent({path})"),
            DirectoryResource::Mode { path, mode, .. } => {
                write!(f, "DirectoryMode({path}, mode = {mode})")
            }
            DirectoryResource::User { path, user, .. } => {
                write!(f, "DirectoryUser({path}, user = {user})")
            }
            DirectoryResource::Group { path, group, .. } => {
                write!(f, "DirectoryGroup({path}, group = {group})")
            }
        }
    }
}

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

#[derive(Error, Debug)]
pub enum DirectoryStateError {
    #[error(transparent)]
    Fs(#[from] FsError),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DirectoryChange {
    Create {
        path: FilePath,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    /// Materialise `path` as a symlink to `source` - emitted for
    /// `state: "linked"`.
    CreateSymlink {
        source: FilePath,
        path: FilePath,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    /// Recursively copy `source` to `path` - emitted for `state: "sourced"`.
    CopyTree {
        source: FilePath,
        path: FilePath,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    Remove {
        path: FilePath,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    ChangeMode {
        path: FilePath,
        mode: FileMode,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    ChangeOwner {
        path: FilePath,
        user: Option<FileUser>,
        group: Option<FileGroup>,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
}

impl Display for DirectoryChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DirectoryChange::Create { path, .. } => {
                write!(f, "Directory::Create(path = {path})")
            }
            DirectoryChange::CreateSymlink { source, path, .. } => {
                write!(
                    f,
                    "Directory::CreateSymlink(source = {source}, path = {path})"
                )
            }
            DirectoryChange::CopyTree { source, path, .. } => {
                write!(f, "Directory::CopyTree(source = {source}, path = {path})")
            }
            DirectoryChange::Remove { path, .. } => {
                write!(f, "Directory::Remove(path = {path})")
            }
            DirectoryChange::ChangeMode { path, mode, .. } => {
                write!(f, "Directory::ChangeMode(path = {path}, mode = {mode})")
            }
            DirectoryChange::ChangeOwner {
                path, user, group, ..
            } => {
                write!(
                    f,
                    "Directory::ChangeOwner(path = {path}, user = {user:?}, group = {group:?})"
                )
            }
        }
    }
}

impl ResourceChangeTrait for DirectoryChange {
    fn kind(&self) -> ChangeKind {
        match self {
            DirectoryChange::Create { .. }
            | DirectoryChange::CreateSymlink { .. }
            | DirectoryChange::CopyTree { .. } => ChangeKind::Added,
            DirectoryChange::Remove { .. } => ChangeKind::Removed,
            DirectoryChange::ChangeMode { .. } | DirectoryChange::ChangeOwner { .. } => {
                ChangeKind::Modified
            }
        }
    }
}

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
        // `sudo` propagates into every sub-atom so chmod/chown on a
        // root-owned path doesn't EACCES after the parent's successful
        // sudo'd write.
        fn permission_atoms(
            path: &FilePath,
            mode: Option<FileMode>,
            user: Option<FileUser>,
            group: Option<FileGroup>,
            sudo: bool,
        ) -> Vec<CausalityTree<DirectoryResource>> {
            let mut nodes = Vec::new();
            if let Some(mode) = mode {
                nodes.push(CausalityTree::leaf(
                    CausalityMeta::requires(vec!["directory".into()]),
                    DirectoryResource::Mode {
                        path: path.clone(),
                        mode,
                        sudo,
                    },
                ));
            }
            if let Some(user) = user {
                nodes.push(CausalityTree::leaf(
                    CausalityMeta::requires(vec!["directory".into()]),
                    DirectoryResource::User {
                        path: path.clone(),
                        user,
                        sudo,
                    },
                ));
            }
            if let Some(group) = group {
                nodes.push(CausalityTree::leaf(
                    CausalityMeta::requires(vec!["directory".into()]),
                    DirectoryResource::Group {
                        path: path.clone(),
                        group,
                        sudo,
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
                sudo,
            } => {
                let mut nodes = vec![CausalityTree::leaf(
                    CausalityMeta::id("directory".into()),
                    DirectoryResource::Sourced {
                        source,
                        path: path.clone(),
                        sudo,
                    },
                )];
                nodes.extend(permission_atoms(&path, mode, user, group, sudo));
                nodes
            }

            DirectoryParams::Linked {
                source,
                source_span: _,
                path,
                sudo,
            } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                DirectoryResource::Linked { source, path, sudo },
            )],

            DirectoryParams::Present {
                path,
                mode,
                user,
                group,
                sudo,
            } => {
                let mut nodes = vec![CausalityTree::leaf(
                    CausalityMeta::id("directory".into()),
                    DirectoryResource::Present {
                        path: path.clone(),
                        sudo,
                    },
                )];
                nodes.extend(permission_atoms(&path, mode, user, group, sudo));
                nodes
            }

            DirectoryParams::Absent { path, sudo } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                DirectoryResource::Absent { path, sudo },
            )],
        }
    }

    type State = DirectoryState;
    type StateError = DirectoryStateError;

    async fn state(
        _ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        // `sudo` is ignored by every probe in v1 - probes run as the calling
        // user. The common case (root-owned 0755 dirs that the user can stat)
        // works; restricted parents (e.g. `/root/`) fail with a standard
        // `FsError`. Sudo-wrapped probes are a follow-up.
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

            DirectoryResource::Linked { source, path, .. } => {
                probe_linked_state(source, path).await?
            }

            DirectoryResource::Present { path, .. } | DirectoryResource::Absent { path, .. } => {
                if fs::path_exists(path.as_path()).await? {
                    DirectoryState::Present
                } else {
                    DirectoryState::Absent
                }
            }

            DirectoryResource::Mode { path, mode, .. } => {
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

            DirectoryResource::User { path, user, .. } => {
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

            DirectoryResource::Group { path, group, .. } => {
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
            (DirectoryResource::Sourced { source, path, sudo }, DirectoryState::NotSourced) => {
                Some(DirectoryChange::CopyTree {
                    source: source.clone(),
                    path: path.clone(),
                    sudo: *sudo,
                })
            }

            (DirectoryResource::Sourced { .. }, DirectoryState::Sourced) => None,

            (DirectoryResource::Linked { source, path, sudo }, DirectoryState::NotLinked) => {
                Some(DirectoryChange::CreateSymlink {
                    source: source.clone(),
                    path: path.clone(),
                    sudo: *sudo,
                })
            }

            (DirectoryResource::Linked { .. }, DirectoryState::Linked) => None,

            (DirectoryResource::Present { path, sudo }, DirectoryState::Absent) => {
                Some(DirectoryChange::Create {
                    path: path.clone(),
                    sudo: *sudo,
                })
            }

            (DirectoryResource::Present { .. }, DirectoryState::Present) => None,

            (DirectoryResource::Absent { path, sudo }, DirectoryState::Present) => {
                Some(DirectoryChange::Remove {
                    path: path.clone(),
                    sudo: *sudo,
                })
            }

            (DirectoryResource::Absent { .. }, DirectoryState::Absent) => None,

            (DirectoryResource::Mode { path, mode, sudo }, DirectoryState::ModeIncorrect) => {
                Some(DirectoryChange::ChangeMode {
                    path: path.clone(),
                    mode: *mode,
                    sudo: *sudo,
                })
            }

            (DirectoryResource::Mode { .. }, DirectoryState::ModeCorrect) => None,

            (DirectoryResource::User { path, user, sudo }, DirectoryState::UserIncorrect) => {
                Some(DirectoryChange::ChangeOwner {
                    path: path.clone(),
                    user: Some(user.clone()),
                    group: None,
                    sudo: *sudo,
                })
            }

            (DirectoryResource::User { .. }, DirectoryState::UserCorrect) => None,

            (DirectoryResource::Group { path, group, sudo }, DirectoryState::GroupIncorrect) => {
                Some(DirectoryChange::ChangeOwner {
                    path: path.clone(),
                    user: None,
                    group: Some(group.clone()),
                    sudo: *sudo,
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
            DirectoryChange::Create { path, sudo } => {
                Operation::Directory(DirectoryOperation::Create { path, sudo })
            }
            DirectoryChange::CreateSymlink { source, path, sudo } => {
                Operation::Directory(DirectoryOperation::CreateSymlink { source, path, sudo })
            }
            DirectoryChange::CopyTree { source, path, sudo } => {
                Operation::Directory(DirectoryOperation::CopyTree { source, path, sudo })
            }
            DirectoryChange::Remove { path, sudo } => {
                Operation::Directory(DirectoryOperation::Remove { path, sudo })
            }
            DirectoryChange::ChangeMode { path, mode, sudo } => {
                Operation::Directory(DirectoryOperation::ChangeMode { path, mode, sudo })
            }
            DirectoryChange::ChangeOwner {
                path,
                user,
                group,
                sudo,
            } => Operation::Directory(DirectoryOperation::ChangeOwner {
                path,
                user,
                group,
                sudo,
            }),
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
            sudo: false,
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
            sudo: false,
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
            sudo: false,
        };
        let change =
            Directory::change(&resource, &DirectoryState::NotSourced).expect("some change");
        match change {
            DirectoryChange::CopyTree {
                source,
                path,
                sudo: _,
            } => {
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
            sudo: false,
        };
        let change = Directory::change(&resource, &DirectoryState::NotLinked).expect("some change");
        match change {
            DirectoryChange::CreateSymlink {
                source,
                path,
                sudo: _,
            } => {
                assert_eq!(source.as_path(), std::path::Path::new("/host/src"));
                assert_eq!(path.as_path(), std::path::Path::new("/target/dest"));
            }
            other => panic!("expected CreateSymlink, got {other:?}"),
        }
    }
}

#[cfg(test)]
mod sudo_tests {
    use super::*;
    use rimu::SourceId;

    fn host_path(s: &str) -> FilePath {
        FilePath::new(s.to_string())
    }

    /// Mirror of the file-side test: `sudo` propagates from `DirectoryParams`
    /// into every emitted atom, including the permission sub-atoms. Same
    /// rationale - chmod/chown on a root-owned dir must also be sudo'd.
    #[test]
    fn sourced_with_sudo_propagates_to_all_sub_atoms() {
        let params = DirectoryParams::Sourced {
            source: host_path("/host/src"),
            source_span: Span::new(SourceId::empty(), 0, 0),
            path: host_path("/etc/myapp"),
            mode: Some(FileMode::new(0o755)),
            user: Some(FileUser::new("root")),
            group: Some(FileGroup::new("root")),
            sudo: true,
        };
        let trees = Directory::resources(params);
        // Sourced + Mode + User + Group = 4 atoms.
        assert_eq!(trees.len(), 4);
        for tree in &trees {
            let resource = match tree {
                CausalityTree::Leaf { node, .. } => node,
                _ => panic!("expected leaf"),
            };
            let sudo = match resource {
                DirectoryResource::Sourced { sudo, .. } => *sudo,
                DirectoryResource::Mode { sudo, .. } => *sudo,
                DirectoryResource::User { sudo, .. } => *sudo,
                DirectoryResource::Group { sudo, .. } => *sudo,
                other => panic!("unexpected variant: {other:?}"),
            };
            assert!(sudo, "every atom under sudo:true should carry sudo:true");
        }
    }

    #[test]
    fn change_propagates_sudo_into_directory_operation() {
        let change = DirectoryChange::CopyTree {
            source: host_path("/host/src"),
            path: host_path("/etc/myapp"),
            sudo: true,
        };
        let ops = Directory::operations(change);
        assert_eq!(ops.len(), 1);
        let op = match &ops[0] {
            CausalityTree::Leaf { node, .. } => node,
            _ => panic!("expected leaf"),
        };
        match op {
            Operation::Directory(DirectoryOperation::CopyTree { sudo, .. }) => {
                assert!(*sudo)
            }
            other => panic!("expected Directory(CopyTree), got {other:?}"),
        }
    }
}
