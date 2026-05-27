//! User-facing resource types. See the crate README for the pipeline shape
//! and the conventions for adding a new resource.

use std::fmt::Display;
use std::path::{Path, PathBuf};

pub use crate::resources::*;

use async_trait::async_trait;
use lusid_causality::CausalityTree;
use lusid_ctx::Context;
use lusid_fs::FsError;
use lusid_operation::{Operation, operations::file::FilePath};
use lusid_params::ParseParams;
use rimu::Span;
use serde::{Deserialize, Serialize};
use thiserror::Error;

pub mod probe;
mod resources;

use crate::resources::apt::{Apt, AptChange, AptParams, AptResource, AptState};
use crate::resources::apt_repo::{
    AptRepo, AptRepoChange, AptRepoParams, AptRepoResource, AptRepoState,
};
use crate::resources::aur::{Aur, AurChange, AurParams, AurResource, AurState};
use crate::resources::command::{
    Command, CommandChange, CommandParams, CommandResource, CommandState,
};
use crate::resources::directory::{
    Directory, DirectoryChange, DirectoryParams, DirectoryResource, DirectoryState,
};
use crate::resources::file::{File, FileChange, FileParams, FileResource, FileState};
use crate::resources::flatpak::{
    Flatpak, FlatpakChange, FlatpakParams, FlatpakResource, FlatpakState,
};
use crate::resources::flatpak_remote::{
    FlatpakRemote, FlatpakRemoteChange, FlatpakRemoteParams, FlatpakRemoteResource,
    FlatpakRemoteState,
};
use crate::resources::git::{Git, GitChange, GitParams, GitResource, GitState};
use crate::resources::group::{Group, GroupChange, GroupParams, GroupResource, GroupState};
use crate::resources::pacman::{Pacman, PacmanChange, PacmanParams, PacmanResource, PacmanState};
use crate::resources::podman::{Podman, PodmanChange, PodmanParams, PodmanResource, PodmanState};
use crate::resources::secret::{Secret, SecretParams};
use crate::resources::systemd::{
    Systemd, SystemdChange, SystemdParams, SystemdResource, SystemdState,
};
use crate::resources::user::{User, UserChange, UserParams, UserResource, UserState};

/// Coarse classification of a [`ResourceChange`], used by the apply pipeline
/// to size the per-epoch confirm prompt and label each pending change with one
/// of three buckets. Finer-grained intent (e.g. the specific change variant)
/// stays on the structured change itself; this is only the headline.
///
/// - [`Added`](Self::Added) - the change introduces new state on the target
///   (install a package, create a file/symlink/dir/user/group).
/// - [`Removed`](Self::Removed) - the change deletes existing state.
/// - [`Modified`](Self::Modified) - everything else (writes-over-existing,
///   mode/owner adjustments, vcs updates, service config tweaks).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ChangeKind {
    Added,
    Removed,
    Modified,
}

/// Classify a resource's change value into the coarse [`ChangeKind`] bucket
/// shown in the confirm prompt summary.
///
/// Each resource module implements this on its own `*Change` type so the
/// add/remove/modify mapping lives next to the variants themselves - adding a
/// new variant cannot silently drift from the apply-time UI because the
/// match is local. The [`ResourceChange`] dispatcher implements this too, by
/// delegating to whichever per-resource variant it carries.
pub trait ResourceChangeTrait {
    fn kind(&self) -> ChangeKind;
}

/// The full pipeline for a single resource type.
///
/// Implementors are zero-sized marker types (e.g. `Apt`, `File`); all the real data lives
/// in the associated types. The flow for one plan item is:
///
/// `Params -> resources() -> State (via state()) -> change() -> operations()`
#[async_trait]
pub trait ResourceType {
    /// Stable identifier used as the `@resource/<ID>` module name in plans.
    const ID: &'static str;

    /// User-facing params struct, parsed directly from the plan's Rimu value
    /// via [`ParseParams`]. Each variant of the struct/enum corresponds to an
    /// allowed shape - the parser does shape validation and typed extraction
    /// in one pass.
    type Params: ParseParams;

    /// Indivisible unit of managed state. One `Params` may produce many atoms (e.g. one
    /// per package in a packages list).
    type Resource;

    /// Expand params into one or more resource atoms, organised as a causality tree so
    /// intra-resource ordering (e.g. "chmod after write") can be declared via meta ids.
    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>>;

    /// Observed state of a single atom on the target machine.
    type State;

    /// Failures that can occur while observing state (command exec, parse errors, etc.).
    type StateError;

    /// Observe the current state of `resource` on the target machine.
    async fn state(
        ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError>;

    /// The delta from `State` to the desired `Resource`.
    type Change: ResourceChangeTrait;

    /// Compute the change needed to reach `resource` from `state`. `None` means no-op.
    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change>;

    /// Lower a change into concrete operations (apt install, write file, …) to execute.
    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>>;
}

/// Dispatcher over every resource's `Params` variant. Produced by the planner from the
/// `@resource/<id>` module a plan item refers to.
///
/// Note(cc): `Secret` is a thin specialisation of `File` (stricter default
/// permissions, single-case schema) that reuses File's `Resource`/`State`/
/// `Change`/`Operation` machinery. It therefore does not get its own
/// variant in `Resource`/`ResourceState`/`ResourceChange` - the atoms it
/// produces are ordinary `Resource::File` atoms. The provenance ("this
/// file was written for a @resource/secret plan item") is preserved only at
/// this `ResourceParams` layer.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ResourceParams {
    Apt(AptParams),
    AptRepo(AptRepoParams),
    Aur(AurParams),
    File(FileParams),
    Directory(DirectoryParams),
    Flatpak(FlatpakParams),
    FlatpakRemote(FlatpakRemoteParams),
    Pacman(PacmanParams),
    Podman(PodmanParams),
    Command(CommandParams),
    Git(GitParams),
    Secret(SecretParams),
    Systemd(SystemdParams),
    User(UserParams),
    Group(GroupParams),
}

impl Display for ResourceParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ResourceParams::*;
        match self {
            Apt(params) => params.fmt(f),
            AptRepo(params) => params.fmt(f),
            Aur(params) => params.fmt(f),
            File(params) => params.fmt(f),
            Directory(params) => params.fmt(f),
            Flatpak(params) => params.fmt(f),
            FlatpakRemote(params) => params.fmt(f),
            Pacman(params) => params.fmt(f),
            Podman(params) => params.fmt(f),
            Command(params) => params.fmt(f),
            Git(params) => params.fmt(f),
            Secret(params) => params.fmt(f),
            Systemd(params) => params.fmt(f),
            User(params) => params.fmt(f),
            Group(params) => params.fmt(f),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Resource {
    Apt(AptResource),
    AptRepo(AptRepoResource),
    Aur(AurResource),
    File(FileResource),
    Directory(DirectoryResource),
    Flatpak(FlatpakResource),
    FlatpakRemote(FlatpakRemoteResource),
    Pacman(PacmanResource),
    Podman(PodmanResource),
    Command(CommandResource),
    Git(GitResource),
    Systemd(SystemdResource),
    User(UserResource),
    Group(GroupResource),
}

impl Display for Resource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Resource::*;
        match self {
            Apt(apt) => apt.fmt(f),
            AptRepo(apt_repo) => apt_repo.fmt(f),
            Aur(aur) => aur.fmt(f),
            File(file) => file.fmt(f),
            Directory(directory) => directory.fmt(f),
            Flatpak(flatpak) => flatpak.fmt(f),
            FlatpakRemote(flatpak_remote) => flatpak_remote.fmt(f),
            Pacman(pacman) => pacman.fmt(f),
            Podman(podman) => podman.fmt(f),
            Command(command) => command.fmt(f),
            Git(git) => git.fmt(f),
            Systemd(systemd) => systemd.fmt(f),
            User(user) => user.fmt(f),
            Group(group) => group.fmt(f),
        }
    }
}

/// Dispatcher over every resource's observed `State`.
///
/// Invariant: the variant always matches the originating `Resource` variant - see
/// [`Resource::change`] for the enforcement point.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ResourceState {
    Apt(AptState),
    AptRepo(AptRepoState),
    Aur(AurState),
    File(FileState),
    Directory(DirectoryState),
    Flatpak(FlatpakState),
    FlatpakRemote(FlatpakRemoteState),
    Pacman(PacmanState),
    Podman(PodmanState),
    Command(CommandState),
    Git(GitState),
    Systemd(SystemdState),
    User(UserState),
    Group(GroupState),
}

impl Display for ResourceState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ResourceState::*;
        match self {
            Apt(apt) => apt.fmt(f),
            AptRepo(apt_repo) => apt_repo.fmt(f),
            Aur(aur) => aur.fmt(f),
            File(file) => file.fmt(f),
            Directory(directory) => directory.fmt(f),
            Flatpak(flatpak) => flatpak.fmt(f),
            FlatpakRemote(flatpak_remote) => flatpak_remote.fmt(f),
            Pacman(pacman) => pacman.fmt(f),
            Podman(podman) => podman.fmt(f),
            Command(command) => command.fmt(f),
            Git(git) => git.fmt(f),
            Systemd(systemd) => systemd.fmt(f),
            User(user) => user.fmt(f),
            Group(group) => group.fmt(f),
        }
    }
}

/// Dispatcher over any per-resource `StateError`. The wrapped error carries the original
/// span/context; the variant just tells you which resource family failed.
#[derive(Error, Debug)]
pub enum ResourceStateError {
    #[error("apt state error: {0}")]
    Apt(#[from] <Apt as ResourceType>::StateError),

    #[error("apt-repo state error: {0}")]
    AptRepo(#[from] <AptRepo as ResourceType>::StateError),

    #[error("aur state error: {0}")]
    Aur(#[from] <Aur as ResourceType>::StateError),

    #[error("file state error: {0}")]
    File(#[from] <File as ResourceType>::StateError),

    #[error("directory state error: {0}")]
    Directory(#[from] <Directory as ResourceType>::StateError),

    #[error("flatpak state error: {0}")]
    Flatpak(#[from] <Flatpak as ResourceType>::StateError),

    #[error("flatpak-remote state error: {0}")]
    FlatpakRemote(#[from] <FlatpakRemote as ResourceType>::StateError),

    #[error("pacman state error: {0}")]
    Pacman(#[from] <Pacman as ResourceType>::StateError),

    #[error("podman state error: {0}")]
    Podman(#[from] <Podman as ResourceType>::StateError),

    #[error("command state error: {0}")]
    Command(#[from] <Command as ResourceType>::StateError),

    #[error("git state error: {0}")]
    Git(#[from] <Git as ResourceType>::StateError),

    #[error("systemd state error: {0}")]
    Systemd(#[from] <Systemd as ResourceType>::StateError),

    #[error("user state error: {0}")]
    User(#[from] <User as ResourceType>::StateError),

    #[error("group state error: {0}")]
    Group(#[from] <Group as ResourceType>::StateError),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ResourceChange {
    Apt(AptChange),
    AptRepo(AptRepoChange),
    Aur(AurChange),
    File(FileChange),
    Directory(DirectoryChange),
    Flatpak(FlatpakChange),
    FlatpakRemote(FlatpakRemoteChange),
    Pacman(PacmanChange),
    Podman(PodmanChange),
    Command(CommandChange),
    Git(GitChange),
    Systemd(SystemdChange),
    User(UserChange),
    Group(GroupChange),
}

impl Display for ResourceChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ResourceChange::*;
        match self {
            Apt(apt) => apt.fmt(f),
            AptRepo(apt_repo) => apt_repo.fmt(f),
            Aur(aur) => aur.fmt(f),
            File(file) => file.fmt(f),
            Directory(directory) => directory.fmt(f),
            Flatpak(flatpak) => flatpak.fmt(f),
            FlatpakRemote(flatpak_remote) => flatpak_remote.fmt(f),
            Pacman(pacman) => pacman.fmt(f),
            Podman(podman) => podman.fmt(f),
            Command(command) => command.fmt(f),
            Git(git) => git.fmt(f),
            Systemd(systemd) => systemd.fmt(f),
            User(user) => user.fmt(f),
            Group(group) => group.fmt(f),
        }
    }
}

impl ResourceChangeTrait for ResourceChange {
    fn kind(&self) -> ChangeKind {
        use ResourceChange::*;
        match self {
            Apt(c) => c.kind(),
            AptRepo(c) => c.kind(),
            Aur(c) => c.kind(),
            File(c) => c.kind(),
            Directory(c) => c.kind(),
            Flatpak(c) => c.kind(),
            FlatpakRemote(c) => c.kind(),
            Pacman(c) => c.kind(),
            Podman(c) => c.kind(),
            Command(c) => c.kind(),
            Git(c) => c.kind(),
            Systemd(c) => c.kind(),
            User(c) => c.kind(),
            Group(c) => c.kind(),
        }
    }
}

impl ResourceParams {
    /// Expand params into resource atoms and lift each per-type tree into the
    /// top-level [`Resource`] dispatcher.
    ///
    /// `secrets_dir` is consulted when expanding `@resource/file` so any
    /// `FileResource::Sourced` atom whose source lies under that directory
    /// is tagged `is_secret`. Downstream `state`/`change` honour the tag by
    /// shipping [`file::Content::Redacted`] rather than raw bytes.
    pub fn resources(self, secrets_dir: &Path) -> Vec<CausalityTree<Resource>> {
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
            ResourceParams::AptRepo(params) => typed::<AptRepo>(params, Resource::AptRepo),
            ResourceParams::Aur(params) => typed::<Aur>(params, Resource::Aur),
            ResourceParams::File(params) => typed::<File>(params, Resource::File)
                .into_iter()
                .map(|tree| tree.map(|r| mark_file_secret_source(r, secrets_dir)))
                .collect(),
            ResourceParams::Directory(params) => typed::<Directory>(params, Resource::Directory),
            ResourceParams::Flatpak(params) => typed::<Flatpak>(params, Resource::Flatpak),
            ResourceParams::FlatpakRemote(params) => {
                typed::<FlatpakRemote>(params, Resource::FlatpakRemote)
            }
            ResourceParams::Pacman(params) => typed::<Pacman>(params, Resource::Pacman),
            ResourceParams::Podman(params) => typed::<Podman>(params, Resource::Podman),
            ResourceParams::Command(params) => typed::<Command>(params, Resource::Command),
            ResourceParams::Git(params) => typed::<Git>(params, Resource::Git),
            // `@resource/secret` lowers to `FileResource::Secret`, which is
            // always redacted in `state`; the path-based `is_secret` flag is
            // not consulted for that variant.
            ResourceParams::Secret(params) => typed::<Secret>(params, Resource::File),
            ResourceParams::Systemd(params) => typed::<Systemd>(params, Resource::Systemd),
            ResourceParams::User(params) => typed::<User>(params, Resource::User),
            ResourceParams::Group(params) => typed::<Group>(params, Resource::Group),
        }
    }
}

/// Stamp `is_secret` on `FileResource::Sourced` atoms whose `source` lives
/// under `secrets_dir`. Other resource variants pass through untouched.
fn mark_file_secret_source(resource: Resource, secrets_dir: &Path) -> Resource {
    match resource {
        Resource::File(file::FileResource::Sourced {
            source,
            path,
            is_secret: _,
            sudo,
        }) => Resource::File(file::FileResource::Sourced {
            is_secret: file::is_secret_source(&source, secrets_dir),
            source,
            path,
            sudo,
        }),
        other => other,
    }
}

impl Resource {
    /// Observe this atom on the target machine and return a [`ResourceState`] in the
    /// matching variant.
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
            Resource::AptRepo(resource) => {
                typed::<AptRepo>(
                    ctx,
                    resource,
                    ResourceState::AptRepo,
                    ResourceStateError::AptRepo,
                )
                .await
            }
            Resource::Aur(resource) => {
                typed::<Aur>(ctx, resource, ResourceState::Aur, ResourceStateError::Aur).await
            }
            Resource::File(resource) => {
                typed::<File>(ctx, resource, ResourceState::File, ResourceStateError::File).await
            }
            Resource::Directory(resource) => {
                typed::<Directory>(
                    ctx,
                    resource,
                    ResourceState::Directory,
                    ResourceStateError::Directory,
                )
                .await
            }
            Resource::Flatpak(resource) => {
                typed::<Flatpak>(
                    ctx,
                    resource,
                    ResourceState::Flatpak,
                    ResourceStateError::Flatpak,
                )
                .await
            }
            Resource::FlatpakRemote(resource) => {
                typed::<FlatpakRemote>(
                    ctx,
                    resource,
                    ResourceState::FlatpakRemote,
                    ResourceStateError::FlatpakRemote,
                )
                .await
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
            Resource::Podman(resource) => {
                typed::<Podman>(
                    ctx,
                    resource,
                    ResourceState::Podman,
                    ResourceStateError::Podman,
                )
                .await
            }
            Resource::Command(resource) => {
                typed::<Command>(
                    ctx,
                    resource,
                    ResourceState::Command,
                    ResourceStateError::Command,
                )
                .await
            }
            Resource::Git(resource) => {
                typed::<Git>(ctx, resource, ResourceState::Git, ResourceStateError::Git).await
            }
            Resource::Systemd(resource) => {
                typed::<Systemd>(
                    ctx,
                    resource,
                    ResourceState::Systemd,
                    ResourceStateError::Systemd,
                )
                .await
            }
            Resource::User(resource) => {
                typed::<User>(ctx, resource, ResourceState::User, ResourceStateError::User).await
            }
            Resource::Group(resource) => {
                typed::<Group>(
                    ctx,
                    resource,
                    ResourceState::Group,
                    ResourceStateError::Group,
                )
                .await
            }
        }
    }

    /// Diff this atom against its observed state. `None` means "already correct".
    ///
    /// Panics if the state variant does not match the resource variant - this is a
    /// programmer error since [`Self::state`] always returns the matching variant.
    pub fn change(&self, state: &ResourceState) -> Option<ResourceChange> {
        fn typed<R: ResourceType>(
            resource: &R::Resource,
            state: &R::State,
            map: impl Fn(R::Change) -> ResourceChange,
        ) -> Option<ResourceChange> {
            R::change(resource, state).map(map)
        }

        match (self, state) {
            (Resource::Apt(resource), ResourceState::Apt(state)) => {
                typed::<Apt>(resource, state, ResourceChange::Apt)
            }
            (Resource::AptRepo(resource), ResourceState::AptRepo(state)) => {
                typed::<AptRepo>(resource, state, ResourceChange::AptRepo)
            }
            (Resource::Aur(resource), ResourceState::Aur(state)) => {
                typed::<Aur>(resource, state, ResourceChange::Aur)
            }
            (Resource::File(resource), ResourceState::File(state)) => {
                typed::<File>(resource, state, ResourceChange::File)
            }
            (Resource::Directory(resource), ResourceState::Directory(state)) => {
                typed::<Directory>(resource, state, ResourceChange::Directory)
            }
            (Resource::Flatpak(resource), ResourceState::Flatpak(state)) => {
                typed::<Flatpak>(resource, state, ResourceChange::Flatpak)
            }
            (Resource::FlatpakRemote(resource), ResourceState::FlatpakRemote(state)) => {
                typed::<FlatpakRemote>(resource, state, ResourceChange::FlatpakRemote)
            }
            (Resource::Pacman(resource), ResourceState::Pacman(state)) => {
                typed::<Pacman>(resource, state, ResourceChange::Pacman)
            }
            (Resource::Podman(resource), ResourceState::Podman(state)) => {
                typed::<Podman>(resource, state, ResourceChange::Podman)
            }
            (Resource::Command(resource), ResourceState::Command(state)) => {
                typed::<Command>(resource, state, ResourceChange::Command)
            }
            (Resource::Git(resource), ResourceState::Git(state)) => {
                typed::<Git>(resource, state, ResourceChange::Git)
            }
            (Resource::Systemd(resource), ResourceState::Systemd(state)) => {
                typed::<Systemd>(resource, state, ResourceChange::Systemd)
            }
            (Resource::User(resource), ResourceState::User(state)) => {
                typed::<User>(resource, state, ResourceChange::User)
            }
            (Resource::Group(resource), ResourceState::Group(state)) => {
                typed::<Group>(resource, state, ResourceChange::Group)
            }
            _ => panic!("Unmatched resource and state"),
        }
    }

    /// Stable lowercase identifier for the resource family. Used by the TUI's
    /// branch detail pane to label resources without leaking the operator's
    /// view to renderer-side formatting choices.
    pub fn family_name(&self) -> &'static str {
        match self {
            Resource::Apt(_) => "apt",
            Resource::AptRepo(_) => "apt_repo",
            Resource::Aur(_) => "aur",
            Resource::File(_) => "file",
            Resource::Directory(_) => "directory",
            Resource::Flatpak(_) => "flatpak",
            Resource::FlatpakRemote(_) => "flatpak_remote",
            Resource::Pacman(_) => "pacman",
            Resource::Podman(_) => "podman",
            Resource::Command(_) => "command",
            Resource::Git(_) => "git",
            Resource::Systemd(_) => "systemd",
            Resource::User(_) => "user",
            Resource::Group(_) => "group",
        }
    }
}

/// Errors from [`ResourceParams::validate_host_paths`] - pre-apply checks that a
/// `host-path` source actually exists on the operator's machine and has the
/// expected type.
///
/// We catch typos and stale paths here rather than letting them surface as
/// confusing apply-time symlink/copy failures.
///
/// Variants attributable to a specific plan value carry the source's
/// [`Span`] so diagnostics can point back at the offending `.lusid` line -
/// see AGENTS.md "spans are load-bearing". The [`Self::Fs`] variant is a
/// low-level filesystem failure with no plan attribution, so it has no span.
/// Errors surfaced by [`ResourceParams::prepare`] - post-validation file I/O
/// (e.g. reading compose files for hash computation). Validation already
/// confirmed each path exists and has the expected type, so failures here
/// are the rare "file deleted between validate and prepare" race, a
/// permission problem, or a transient I/O issue. Span-attributable variants
/// carry the source span for diagnostics.
#[derive(Debug, Error)]
pub enum ResourcePrepareError {
    #[error(transparent)]
    Podman(#[from] crate::resources::podman::PodmanPrepareError),
}

#[derive(Debug, Error)]
pub enum HostPathValidationError {
    #[error("source host-path {path:?} for @resource/file resource was not found")]
    FileSourceMissing { path: PathBuf, span: Span },

    #[error("source host-path {path:?} for @resource/file resource is not a regular file")]
    FileSourceNotFile { path: PathBuf, span: Span },

    #[error("source host-path {path:?} for @resource/directory resource was not found")]
    DirectorySourceMissing { path: PathBuf, span: Span },

    #[error("source host-path {path:?} for @resource/directory resource is not a directory")]
    DirectorySourceNotDirectory { path: PathBuf, span: Span },

    #[error(
        "compose file {path:?} (entry {index} of `files:`) for @resource/podman compose was not found"
    )]
    ComposeFileMissing {
        path: PathBuf,
        span: Span,
        index: usize,
    },

    #[error(
        "compose file {path:?} (entry {index} of `files:`) for @resource/podman compose is not a regular file"
    )]
    ComposeFileNotFile {
        path: PathBuf,
        span: Span,
        index: usize,
    },

    #[error("compose env_file {path:?} for @resource/podman compose was not found")]
    ComposeEnvFileMissing { path: PathBuf, span: Span },

    #[error("compose env_file {path:?} for @resource/podman compose is not a regular file")]
    ComposeEnvFileNotFile { path: PathBuf, span: Span },

    #[error("compose working_dir {path:?} for @resource/podman compose was not found")]
    ComposeWorkingDirMissing { path: PathBuf, span: Span },

    #[error("compose working_dir {path:?} for @resource/podman compose is not a directory")]
    ComposeWorkingDirNotDirectory { path: PathBuf, span: Span },

    #[error(transparent)]
    Fs(#[from] FsError),
}

impl ResourceParams {
    /// Validate that any `host-path` source referenced by this params variant
    /// exists on the operator's filesystem with the expected type.
    ///
    /// `@resource/file` `state: "sourced"` and `state: "linked"` both require
    /// `source` to be a regular file (or a symlink that resolves to one).
    /// `@resource/directory` `state: "sourced"` and `state: "linked"` both
    /// require `source` to be a directory. All other variants are no-ops.
    ///
    /// Source paths arrive here already resolved to absolute `PathBuf`s (see
    /// `params::ParamType::HostPath` coercion). The probe follows a single
    /// layer of symlink so the `Symlink → File` and `Symlink → Dir` cases
    /// classify correctly; deeper symlink chains are accepted whatever
    /// `tokio::fs::metadata` resolves them to.
    ///
    /// Plan-attributable variants of [`HostPathValidationError`] carry the
    /// source field's span so callers can surface a diagnostic that points
    /// at the offending `.lusid` line - see AGENTS.md "spans are
    /// load-bearing".
    pub async fn validate_host_paths(&self) -> Result<(), HostPathValidationError> {
        match self {
            ResourceParams::File(FileParams::Sourced {
                source,
                source_span,
                ..
            })
            | ResourceParams::File(FileParams::Linked {
                source,
                source_span,
                ..
            }) => check_source_is_file(source, source_span).await,
            ResourceParams::Directory(DirectoryParams::Sourced {
                source,
                source_span,
                ..
            })
            | ResourceParams::Directory(DirectoryParams::Linked {
                source,
                source_span,
                ..
            }) => check_source_is_directory(source, source_span).await,
            ResourceParams::Podman(PodmanParams::ComposePresent {
                files,
                files_spans,
                working_dir,
                working_dir_span,
                env_file,
                env_file_span,
                ..
            }) => {
                // Per-element index-aware diagnostics for `files: [...]` so an
                // "entry 2 of `files:` is missing" error points at the right
                // list element rather than the list as a whole.
                for (index, file) in files.iter().enumerate() {
                    check_compose_file(file, &files_spans[index], index).await?;
                }
                check_compose_working_dir(working_dir, working_dir_span).await?;
                if let (Some(ef), Some(span)) = (env_file.as_ref(), env_file_span.as_ref()) {
                    check_compose_env_file(ef, span).await?;
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    /// Post-validation preparation: read host-side files referenced by params
    /// (today only compose YAML) and bake the resulting hash into the params
    /// for downstream change-time comparison.
    ///
    /// Called by the apply pipeline between [`Self::validate_host_paths`] and
    /// [`Self::resources`]. For non-compose variants this is a pass-through.
    /// Validation runs first so a `ReadFailed` here is the rare
    /// "file deleted between validate and prepare" race or a permission
    /// error - both worth their own span-attributable diagnostic.
    pub async fn prepare(self) -> Result<Self, ResourcePrepareError> {
        match self {
            ResourceParams::Podman(podman) => Ok(ResourceParams::Podman(podman.prepare().await?)),
            other => Ok(other),
        }
    }
}

/// Resolve `path`'s metadata, classifying symlink chains by what they
/// ultimately resolve to. Returns `Ok(None)` if `path` (or anywhere along its
/// symlink chain) does not exist, so callers can map both into a "source
/// missing" diagnostic without caring whether the dangling part is the path
/// itself or somewhere down the link chain.
async fn resolved_metadata(path: &std::path::Path) -> Result<Option<std::fs::Metadata>, FsError> {
    let metadata = match tokio::fs::symlink_metadata(path).await {
        Ok(m) => m,
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(source) => {
            return Err(FsError::Metadata {
                path: path.to_path_buf(),
                source,
            });
        }
    };
    if !metadata.file_type().is_symlink() {
        return Ok(Some(metadata));
    }
    // Symlink - `tokio::fs::metadata` is `stat(2)`, which walks the full
    // chain. Dangling anywhere along the chain reads as NotFound; surface
    // as None so the caller's `Missing` diagnostic fires (the link is
    // useless either way).
    match tokio::fs::metadata(path).await {
        Ok(m) => Ok(Some(m)),
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(source) => Err(FsError::Metadata {
            path: path.to_path_buf(),
            source,
        }),
    }
}

async fn check_source_is_file(
    source: &FilePath,
    span: &Span,
) -> Result<(), HostPathValidationError> {
    let path = source.as_path();
    let Some(metadata) = resolved_metadata(path).await? else {
        return Err(HostPathValidationError::FileSourceMissing {
            path: path.to_path_buf(),
            span: span.clone(),
        });
    };
    if !metadata.is_file() {
        return Err(HostPathValidationError::FileSourceNotFile {
            path: path.to_path_buf(),
            span: span.clone(),
        });
    }
    Ok(())
}

async fn check_compose_file(
    source: &FilePath,
    span: &Span,
    index: usize,
) -> Result<(), HostPathValidationError> {
    let path = source.as_path();
    let Some(metadata) = resolved_metadata(path).await? else {
        return Err(HostPathValidationError::ComposeFileMissing {
            path: path.to_path_buf(),
            span: span.clone(),
            index,
        });
    };
    if !metadata.is_file() {
        return Err(HostPathValidationError::ComposeFileNotFile {
            path: path.to_path_buf(),
            span: span.clone(),
            index,
        });
    }
    Ok(())
}

async fn check_compose_env_file(
    source: &FilePath,
    span: &Span,
) -> Result<(), HostPathValidationError> {
    let path = source.as_path();
    let Some(metadata) = resolved_metadata(path).await? else {
        return Err(HostPathValidationError::ComposeEnvFileMissing {
            path: path.to_path_buf(),
            span: span.clone(),
        });
    };
    if !metadata.is_file() {
        return Err(HostPathValidationError::ComposeEnvFileNotFile {
            path: path.to_path_buf(),
            span: span.clone(),
        });
    }
    Ok(())
}

async fn check_compose_working_dir(
    source: &FilePath,
    span: &Span,
) -> Result<(), HostPathValidationError> {
    let path = source.as_path();
    let Some(metadata) = resolved_metadata(path).await? else {
        return Err(HostPathValidationError::ComposeWorkingDirMissing {
            path: path.to_path_buf(),
            span: span.clone(),
        });
    };
    if !metadata.is_dir() {
        return Err(HostPathValidationError::ComposeWorkingDirNotDirectory {
            path: path.to_path_buf(),
            span: span.clone(),
        });
    }
    Ok(())
}

async fn check_source_is_directory(
    source: &FilePath,
    span: &Span,
) -> Result<(), HostPathValidationError> {
    let path = source.as_path();
    let Some(metadata) = resolved_metadata(path).await? else {
        return Err(HostPathValidationError::DirectorySourceMissing {
            path: path.to_path_buf(),
            span: span.clone(),
        });
    };
    if !metadata.is_dir() {
        return Err(HostPathValidationError::DirectorySourceNotDirectory {
            path: path.to_path_buf(),
            span: span.clone(),
        });
    }
    Ok(())
}

impl ResourceChange {
    /// Lower a change into the concrete operations that execute it, preserving any
    /// intra-change ordering (e.g. `apt update` before `apt install`).
    pub fn operations(self) -> Vec<CausalityTree<Operation>> {
        match self {
            ResourceChange::Apt(change) => Apt::operations(change),
            ResourceChange::AptRepo(change) => AptRepo::operations(change),
            ResourceChange::Aur(change) => Aur::operations(change),
            ResourceChange::File(change) => File::operations(change),
            ResourceChange::Directory(change) => Directory::operations(change),
            ResourceChange::Flatpak(change) => Flatpak::operations(change),
            ResourceChange::FlatpakRemote(change) => FlatpakRemote::operations(change),
            ResourceChange::Pacman(change) => Pacman::operations(change),
            ResourceChange::Podman(change) => Podman::operations(change),
            ResourceChange::Command(change) => Command::operations(change),
            ResourceChange::Git(change) => Git::operations(change),
            ResourceChange::Systemd(change) => Systemd::operations(change),
            ResourceChange::User(change) => User::operations(change),
            ResourceChange::Group(change) => Group::operations(change),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lusid_operation::operations::file::FilePath;
    use rimu::SourceId;
    use tempfile::tempdir;

    fn file_path(p: &std::path::Path) -> FilePath {
        FilePath::new(p.to_string_lossy().into_owned())
    }

    fn empty_span() -> Span {
        Span::new(SourceId::empty(), 0, 0)
    }

    fn file_sourced(source: FilePath) -> ResourceParams {
        ResourceParams::File(FileParams::Sourced {
            source,
            source_span: empty_span(),
            path: FilePath::new("/tmp/lusid-validate-test-target"),
            mode: None,
            user: None,
            group: None,
            sudo: false,
        })
    }

    fn directory_sourced(source: FilePath) -> ResourceParams {
        ResourceParams::Directory(DirectoryParams::Sourced {
            source,
            source_span: empty_span(),
            path: FilePath::new("/tmp/lusid-validate-test-target"),
            mode: None,
            user: None,
            group: None,
            sudo: false,
        })
    }

    fn file_linked(source: FilePath) -> ResourceParams {
        ResourceParams::File(FileParams::Linked {
            source,
            source_span: empty_span(),
            path: FilePath::new("/tmp/lusid-validate-test-target"),
            sudo: false,
        })
    }

    fn directory_linked(source: FilePath) -> ResourceParams {
        ResourceParams::Directory(DirectoryParams::Linked {
            source,
            source_span: empty_span(),
            path: FilePath::new("/tmp/lusid-validate-test-target"),
            sudo: false,
        })
    }

    #[tokio::test]
    async fn file_sourced_validates_when_source_is_a_file() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src.txt");
        tokio::fs::write(&source, b"x").await.unwrap();
        file_sourced(file_path(&source))
            .validate_host_paths()
            .await
            .expect("file source should validate");
    }

    #[tokio::test]
    async fn file_sourced_errors_when_source_is_missing() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("missing.txt");
        let err = file_sourced(file_path(&source))
            .validate_host_paths()
            .await
            .unwrap_err();
        assert!(matches!(
            err,
            HostPathValidationError::FileSourceMissing { .. }
        ));
    }

    #[tokio::test]
    async fn file_sourced_errors_when_source_is_a_directory() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("a-dir");
        tokio::fs::create_dir(&source).await.unwrap();
        let err = file_sourced(file_path(&source))
            .validate_host_paths()
            .await
            .unwrap_err();
        assert!(matches!(
            err,
            HostPathValidationError::FileSourceNotFile { .. }
        ));
    }

    #[tokio::test]
    async fn file_sourced_follows_symlinks_to_files() {
        // A symlink-to-file is fine: the bytes still resolve to a regular
        // file, which is what `state: "sourced"` ultimately needs.
        let dir = tempdir().unwrap();
        let real = dir.path().join("real.txt");
        tokio::fs::write(&real, b"x").await.unwrap();
        let link = dir.path().join("link.txt");
        tokio::fs::symlink(&real, &link).await.unwrap();
        file_sourced(file_path(&link))
            .validate_host_paths()
            .await
            .expect("symlink to file should validate");
    }

    #[tokio::test]
    async fn directory_sourced_validates_when_source_is_a_directory() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src");
        tokio::fs::create_dir(&source).await.unwrap();
        directory_sourced(file_path(&source))
            .validate_host_paths()
            .await
            .expect("directory source should validate");
    }

    #[tokio::test]
    async fn directory_sourced_errors_when_source_is_missing() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("missing");
        let err = directory_sourced(file_path(&source))
            .validate_host_paths()
            .await
            .unwrap_err();
        assert!(matches!(
            err,
            HostPathValidationError::DirectorySourceMissing { .. }
        ));
    }

    #[tokio::test]
    async fn directory_sourced_errors_when_source_is_a_file() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src.txt");
        tokio::fs::write(&source, b"x").await.unwrap();
        let err = directory_sourced(file_path(&source))
            .validate_host_paths()
            .await
            .unwrap_err();
        assert!(matches!(
            err,
            HostPathValidationError::DirectorySourceNotDirectory { .. }
        ));
    }

    // --- `state: "linked"` reuses the same file/directory checks ---------

    #[tokio::test]
    async fn file_linked_validates_when_source_is_a_file() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src.txt");
        tokio::fs::write(&source, b"x").await.unwrap();
        file_linked(file_path(&source))
            .validate_host_paths()
            .await
            .expect("file source should validate");
    }

    #[tokio::test]
    async fn file_linked_errors_when_source_is_missing() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("missing.txt");
        let err = file_linked(file_path(&source))
            .validate_host_paths()
            .await
            .unwrap_err();
        assert!(matches!(
            err,
            HostPathValidationError::FileSourceMissing { .. }
        ));
    }

    #[tokio::test]
    async fn file_linked_errors_when_source_is_a_directory() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("a-dir");
        tokio::fs::create_dir(&source).await.unwrap();
        let err = file_linked(file_path(&source))
            .validate_host_paths()
            .await
            .unwrap_err();
        assert!(matches!(
            err,
            HostPathValidationError::FileSourceNotFile { .. }
        ));
    }

    #[tokio::test]
    async fn directory_linked_validates_when_source_is_a_directory() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src");
        tokio::fs::create_dir(&source).await.unwrap();
        directory_linked(file_path(&source))
            .validate_host_paths()
            .await
            .expect("directory source should validate");
    }

    #[tokio::test]
    async fn directory_linked_errors_when_source_is_missing() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("missing");
        let err = directory_linked(file_path(&source))
            .validate_host_paths()
            .await
            .unwrap_err();
        assert!(matches!(
            err,
            HostPathValidationError::DirectorySourceMissing { .. }
        ));
    }

    #[tokio::test]
    async fn directory_linked_errors_when_source_is_a_file() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src.txt");
        tokio::fs::write(&source, b"x").await.unwrap();
        let err = directory_linked(file_path(&source))
            .validate_host_paths()
            .await
            .unwrap_err();
        assert!(matches!(
            err,
            HostPathValidationError::DirectorySourceNotDirectory { .. }
        ));
    }

    #[tokio::test]
    async fn unrelated_resource_params_are_a_no_op() {
        // Non-sourced resources don't reach the filesystem at all.
        let absent = ResourceParams::File(FileParams::Absent {
            path: FilePath::new("/tmp/never-touched"),
            sudo: false,
        });
        absent.validate_host_paths().await.expect("no-op");
    }

    #[tokio::test]
    async fn file_sourced_errors_when_source_is_a_symlink_to_a_directory() {
        let dir = tempdir().unwrap();
        let real_dir = dir.path().join("real-dir");
        tokio::fs::create_dir(&real_dir).await.unwrap();
        let link = dir.path().join("link-to-dir");
        tokio::fs::symlink(&real_dir, &link).await.unwrap();

        let err = file_sourced(file_path(&link))
            .validate_host_paths()
            .await
            .unwrap_err();
        assert!(matches!(
            err,
            HostPathValidationError::FileSourceNotFile { .. }
        ));
    }

    /// A dangling symlink as source surfaces as `*Missing`, not the lower-
    /// level `FsError::Metadata` - the operator's mental model is "the
    /// source isn't there", and where exactly the chain breaks isn't useful
    /// at the diagnostic layer.
    #[tokio::test]
    async fn file_sourced_dangling_symlink_reports_missing() {
        let dir = tempdir().unwrap();
        let dangling_target = dir.path().join("never-existed.txt");
        let link = dir.path().join("dangle.txt");
        tokio::fs::symlink(&dangling_target, &link).await.unwrap();

        let err = file_sourced(file_path(&link))
            .validate_host_paths()
            .await
            .unwrap_err();
        assert!(matches!(
            err,
            HostPathValidationError::FileSourceMissing { .. }
        ));
    }

    #[tokio::test]
    async fn directory_sourced_dangling_symlink_reports_missing() {
        let dir = tempdir().unwrap();
        let dangling_target = dir.path().join("never-existed");
        let link = dir.path().join("dangle");
        tokio::fs::symlink(&dangling_target, &link).await.unwrap();

        let err = directory_sourced(file_path(&link))
            .validate_host_paths()
            .await
            .unwrap_err();
        assert!(matches!(
            err,
            HostPathValidationError::DirectorySourceMissing { .. }
        ));
    }

    // -- compose --------------------------------------------------------

    fn compose_params(
        files: Vec<FilePath>,
        files_spans: Vec<Span>,
        working_dir: FilePath,
        env_file: Option<FilePath>,
        env_file_span: Option<Span>,
    ) -> ResourceParams {
        use crate::resources::podman::PodmanParams;
        ResourceParams::Podman(PodmanParams::ComposePresent {
            project: "app".into(),
            project_span: empty_span(),
            files,
            files_spans,
            working_dir,
            working_dir_span: empty_span(),
            env_file,
            env_file_span,
            config_hash: None,
            sudo: false,
        })
    }

    #[tokio::test]
    async fn compose_validates_existing_files() {
        let dir = tempdir().unwrap();
        let file = dir.path().join("compose.yaml");
        tokio::fs::write(&file, b"services: {}\n").await.unwrap();
        compose_params(
            vec![file_path(&file)],
            vec![empty_span()],
            file_path(dir.path()),
            None,
            None,
        )
        .validate_host_paths()
        .await
        .expect("should validate");
    }

    #[tokio::test]
    async fn compose_reports_missing_file_with_index() {
        let dir = tempdir().unwrap();
        let real = dir.path().join("real.yaml");
        tokio::fs::write(&real, b"x").await.unwrap();
        let missing = dir.path().join("missing.yaml");
        let err = compose_params(
            vec![file_path(&real), file_path(&missing)],
            vec![empty_span(), empty_span()],
            file_path(dir.path()),
            None,
            None,
        )
        .validate_host_paths()
        .await
        .unwrap_err();
        match err {
            HostPathValidationError::ComposeFileMissing { index, .. } => {
                assert_eq!(index, 1, "should report the second (missing) entry");
            }
            other => panic!("expected ComposeFileMissing, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn compose_reports_directory_as_not_file() {
        let dir = tempdir().unwrap();
        let subdir = dir.path().join("subdir");
        tokio::fs::create_dir(&subdir).await.unwrap();
        let err = compose_params(
            vec![file_path(&subdir)],
            vec![empty_span()],
            file_path(dir.path()),
            None,
            None,
        )
        .validate_host_paths()
        .await
        .unwrap_err();
        assert!(matches!(
            err,
            HostPathValidationError::ComposeFileNotFile { .. }
        ));
    }

    #[tokio::test]
    async fn compose_reports_missing_env_file() {
        let dir = tempdir().unwrap();
        let file = dir.path().join("compose.yaml");
        tokio::fs::write(&file, b"x").await.unwrap();
        let missing_env = dir.path().join("missing.env");
        let err = compose_params(
            vec![file_path(&file)],
            vec![empty_span()],
            file_path(dir.path()),
            Some(file_path(&missing_env)),
            Some(empty_span()),
        )
        .validate_host_paths()
        .await
        .unwrap_err();
        assert!(matches!(
            err,
            HostPathValidationError::ComposeEnvFileMissing { .. }
        ));
    }

    #[tokio::test]
    async fn compose_reports_working_dir_not_directory() {
        let dir = tempdir().unwrap();
        let file = dir.path().join("compose.yaml");
        tokio::fs::write(&file, b"x").await.unwrap();
        // working_dir points at a regular file, not a directory.
        let err = compose_params(
            vec![file_path(&file)],
            vec![empty_span()],
            file_path(&file),
            None,
            None,
        )
        .validate_host_paths()
        .await
        .unwrap_err();
        assert!(matches!(
            err,
            HostPathValidationError::ComposeWorkingDirNotDirectory { .. }
        ));
    }
}

#[cfg(test)]
mod dispatch_tests {
    use super::*;
    use crate::resources::file::FileResource;
    use lusid_operation::operations::file::FilePath;
    use rimu::SourceId;

    fn empty_span() -> Span {
        Span::new(SourceId::empty(), 0, 0)
    }

    #[test]
    fn resources_tag_is_secret_when_source_under_secrets_dir() {
        let params = ResourceParams::File(FileParams::Sourced {
            source: FilePath::new("/proj/secrets/api.txt"),
            source_span: empty_span(),
            path: FilePath::new("/target/dest.txt"),
            mode: None,
            user: None,
            group: None,
            sudo: false,
        });
        let trees = params.resources(Path::new("/proj/secrets"));
        // Walk the first tree's leaves; the leading atom is `Sourced`.
        let leaf = collect_first_leaf(&trees[0]).expect("at least one leaf");
        match leaf {
            Resource::File(FileResource::Sourced { is_secret, .. }) => {
                assert!(*is_secret, "source under secrets_dir should mark is_secret");
            }
            other => panic!("expected File::Sourced, got {other:?}"),
        }
    }

    #[test]
    fn resources_leaves_is_secret_false_when_source_outside_secrets_dir() {
        let params = ResourceParams::File(FileParams::Sourced {
            source: FilePath::new("/proj/files/app.conf"),
            source_span: empty_span(),
            path: FilePath::new("/target/app.conf"),
            mode: None,
            user: None,
            group: None,
            sudo: false,
        });
        let trees = params.resources(Path::new("/proj/secrets"));
        let leaf = collect_first_leaf(&trees[0]).expect("at least one leaf");
        match leaf {
            Resource::File(FileResource::Sourced { is_secret, .. }) => {
                assert!(
                    !is_secret,
                    "source outside secrets_dir should leave is_secret false"
                );
            }
            other => panic!("expected File::Sourced, got {other:?}"),
        }
    }

    fn collect_first_leaf(tree: &CausalityTree<Resource>) -> Option<&Resource> {
        use lusid_tree::Tree;
        match tree {
            Tree::Leaf { node, .. } => Some(node),
            Tree::Branch { children, .. } => children.iter().find_map(collect_first_leaf),
        }
    }

    /// Walk every `Resource` variant and confirm [`Resource::family_name`]
    /// returns the documented lowercase identifier. The match in
    /// `family_name` is exhaustive on the enum, so when a new variant is
    /// added this test forces the author to extend the mapping too.
    #[test]
    fn family_name_covers_every_resource_variant() {
        use crate::resources::{
            apt::AptResource,
            apt_repo::AptRepoResource,
            aur::AurResource,
            command::{CommandResource, CommandStatus},
            directory::DirectoryResource,
            file::FileResource,
            flatpak::FlatpakResource,
            flatpak_remote::FlatpakRemoteResource,
            git::GitResource,
            group::GroupResource,
            pacman::PacmanResource,
            podman::PodmanResource,
            systemd::SystemdResource,
            user::UserResource,
        };

        let cases: Vec<(Resource, &'static str)> = vec![
            (
                Resource::Apt(AptResource {
                    package: "nginx".into(),
                }),
                "apt",
            ),
            (
                Resource::AptRepo(AptRepoResource {
                    name: "nginx".into(),
                    sources_path: FilePath::new("/etc/apt/sources.list.d/nginx.list"),
                    sources_content: "deb https://example".into(),
                    key_url: "https://example/key.gpg".into(),
                    key_path: FilePath::new("/etc/apt/keyrings/nginx.gpg"),
                }),
                "apt_repo",
            ),
            (
                Resource::Aur(AurResource {
                    package: "yay".into(),
                }),
                "aur",
            ),
            (
                Resource::File(FileResource::Linked {
                    source: FilePath::new("/src"),
                    path: FilePath::new("/dst"),
                    sudo: false,
                }),
                "file",
            ),
            (
                Resource::Directory(DirectoryResource::Present {
                    path: FilePath::new("/dir"),
                    sudo: false,
                }),
                "directory",
            ),
            (
                Resource::Flatpak(FlatpakResource::Absent {
                    name: "app".into(),
                    user: false,
                    delete_data: false,
                }),
                "flatpak",
            ),
            (
                Resource::FlatpakRemote(FlatpakRemoteResource::Absent {
                    name: "flathub".into(),
                    user: false,
                }),
                "flatpak_remote",
            ),
            (
                Resource::Pacman(PacmanResource {
                    package: "zsh".into(),
                }),
                "pacman",
            ),
            (
                Resource::Podman(PodmanResource::Absent {
                    name: "ctr".into(),
                    sudo: false,
                }),
                "podman",
            ),
            (
                Resource::Command(CommandResource {
                    status: CommandStatus::Install,
                    is_installed: None,
                    install: None,
                    uninstall: None,
                    sudo: false,
                }),
                "command",
            ),
            (
                Resource::Git(GitResource {
                    repo: "https://example/r.git".into(),
                    path: FilePath::new("/repo"),
                    version: None,
                    update: false,
                    force: false,
                }),
                "git",
            ),
            (
                Resource::Systemd(SystemdResource {
                    name: "nginx".into(),
                    enabled: true,
                    active: true,
                    user: false,
                }),
                "systemd",
            ),
            (
                Resource::User(UserResource::Absent {
                    name: "alice".into(),
                    remove_home: false,
                }),
                "user",
            ),
            (
                Resource::Group(GroupResource::Absent {
                    name: "wheel".into(),
                }),
                "group",
            ),
        ];

        for (resource, expected) in cases {
            assert_eq!(resource.family_name(), expected, "for {resource:?}");
        }
    }
}
