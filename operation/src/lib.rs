//! Concrete mutations executed against a target machine. See the crate
//! README for the [`OperationType`] trait shape and the `merge` / `apply`
//! contract.

use async_trait::async_trait;
use core::task;
use lusid_ctx::Context;
use lusid_view::Render;
use pin_project::pin_project;
use serde::{Deserialize, Serialize};
use std::{
    fmt::{Debug, Display},
    future::Future,
    pin::Pin,
    task::Poll,
};
use thiserror::Error;
use tokio::io::AsyncRead;

pub mod operations;

use crate::operations::{
    apt::{Apt, AptOperation},
    apt_repo::{AptRepo, AptRepoOperation},
    aur::{Aur, AurOperation},
    command::{Command, CommandOperation},
    directory::{Directory, DirectoryOperation},
    file::{File, FileOperation},
    flatpak::{Flatpak, FlatpakOperation},
    git::{Git, GitOperation},
    group::{Group, GroupOperation},
    pacman::{Pacman, PacmanOperation},
    podman::{Podman, PodmanOperation},
    systemd::{Systemd, SystemdOperation},
    user::{User, UserOperation},
};

/// One family of operations (apt, pacman, file, …). Implementors are zero-sized
/// markers; the real data lives in `Operation`.
#[async_trait]
pub trait OperationType {
    /// The concrete operation value (e.g. `AptOperation::Install { packages }`).
    type Operation: Render;

    /// Coalesce a batch of same-type operations scheduled in one epoch.
    ///
    /// For package managers this unions install sets. For side-effecting operations
    /// (file, command, git) the order matters, so `merge` is a no-op.
    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation>;

    /// Failure returned when `apply`'s future resolves.
    type ApplyError;

    /// Stdout stream of the running operation - polled by the TUI.
    type ApplyStdout: AsyncRead;

    /// Stderr stream of the running operation - polled by the TUI.
    type ApplyStderr: AsyncRead;

    /// Future that resolves when the operation finishes.
    type ApplyOutput: Future<Output = Result<(), Self::ApplyError>>;

    /// Kick off the operation and return its completion future plus live
    /// stdout/stderr streams. The caller drives all three concurrently so output
    /// is streamed in real time.
    async fn apply(
        ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError>;
}

/// Dispatcher over every operation family. Every leaf of the per-epoch causality
/// tree is an `Operation`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Operation {
    Apt(AptOperation),
    AptRepo(AptRepoOperation),
    Aur(AurOperation),
    Pacman(PacmanOperation),
    Podman(PodmanOperation),
    Flatpak(FlatpakOperation),
    File(FileOperation),
    Directory(DirectoryOperation),
    Command(CommandOperation),
    Git(GitOperation),
    Systemd(SystemdOperation),
    User(UserOperation),
    Group(GroupOperation),
}

impl Operation {
    /// Partition `operations` by family, merge each family via its [`OperationType::merge`]
    /// impl, and re-wrap in family order (apt, pacman, file, command, git).
    ///
    /// Called once per epoch before `apply` - the whole point is to collapse e.g. 20
    /// separate `apt install` operations into one multi-package install.
    pub fn merge(operations: impl IntoIterator<Item = Operation>) -> Vec<Operation> {
        let OperationsByType {
            apt,
            apt_repo,
            aur,
            pacman,
            podman,
            flatpak,
            file,
            directory,
            command,
            git,
            systemd,
            user,
            group,
        } = partition_by_type(operations);

        std::iter::empty()
            .chain(Apt::merge(apt).into_iter().map(Operation::Apt))
            .chain(AptRepo::merge(apt_repo).into_iter().map(Operation::AptRepo))
            .chain(Aur::merge(aur).into_iter().map(Operation::Aur))
            .chain(Pacman::merge(pacman).into_iter().map(Operation::Pacman))
            .chain(Podman::merge(podman).into_iter().map(Operation::Podman))
            .chain(Flatpak::merge(flatpak).into_iter().map(Operation::Flatpak))
            .chain(File::merge(file).into_iter().map(Operation::File))
            .chain(
                Directory::merge(directory)
                    .into_iter()
                    .map(Operation::Directory),
            )
            .chain(Command::merge(command).into_iter().map(Operation::Command))
            .chain(Git::merge(git).into_iter().map(Operation::Git))
            .chain(Systemd::merge(systemd).into_iter().map(Operation::Systemd))
            .chain(User::merge(user).into_iter().map(Operation::User))
            .chain(Group::merge(group).into_iter().map(Operation::Group))
            .collect()
    }
}

/// Dispatcher over any per-family `ApplyError`.
#[derive(Error, Debug)]
pub enum OperationApplyError {
    #[error("apt operation failed: {0:?}")]
    Apt(<Apt as OperationType>::ApplyError),

    #[error("apt-repo operation failed: {0:?}")]
    AptRepo(<AptRepo as OperationType>::ApplyError),

    #[error("aur operation failed: {0:?}")]
    Aur(<Aur as OperationType>::ApplyError),

    #[error("pacman operation failed: {0:?}")]
    Pacman(<Pacman as OperationType>::ApplyError),

    #[error("podman operation failed: {0:?}")]
    Podman(<Podman as OperationType>::ApplyError),

    #[error("flatpak operation failed: {0:?}")]
    Flatpak(<Flatpak as OperationType>::ApplyError),

    #[error("file operation failed: {0:?}")]
    File(<File as OperationType>::ApplyError),

    #[error("directory operation failed: {0:?}")]
    Directory(<Directory as OperationType>::ApplyError),

    #[error("command operation failed: {0:?}")]
    Command(<Command as OperationType>::ApplyError),

    #[error("git operation failed: {0:?}")]
    Git(<Git as OperationType>::ApplyError),

    #[error("systemd operation failed: {0:?}")]
    Systemd(<Systemd as OperationType>::ApplyError),

    #[error("user operation failed: {0:?}")]
    User(<User as OperationType>::ApplyError),

    #[error("group operation failed: {0:?}")]
    Group(<Group as OperationType>::ApplyError),
}

/// Unified completion future for any operation. `Future::poll` forwards to the active
/// variant via `pin_project`, avoiding a per-operation boxing allocation.
#[pin_project(project = OperationApplyOutputProject)]
pub enum OperationApplyOutput {
    Apt(#[pin] <Apt as OperationType>::ApplyOutput),
    AptRepo(#[pin] <AptRepo as OperationType>::ApplyOutput),
    Aur(#[pin] <Aur as OperationType>::ApplyOutput),
    Pacman(#[pin] <Pacman as OperationType>::ApplyOutput),
    Podman(#[pin] <Podman as OperationType>::ApplyOutput),
    Flatpak(#[pin] <Flatpak as OperationType>::ApplyOutput),
    File(#[pin] <File as OperationType>::ApplyOutput),
    Directory(#[pin] <Directory as OperationType>::ApplyOutput),
    Command(#[pin] <Command as OperationType>::ApplyOutput),
    Git(#[pin] <Git as OperationType>::ApplyOutput),
    Systemd(#[pin] <Systemd as OperationType>::ApplyOutput),
    User(#[pin] <User as OperationType>::ApplyOutput),
    Group(#[pin] <Group as OperationType>::ApplyOutput),
}

impl Future for OperationApplyOutput {
    type Output = Result<(), OperationApplyError>;

    fn poll(self: Pin<&mut Self>, cx: &mut task::Context<'_>) -> Poll<Self::Output> {
        use OperationApplyOutputProject::*;
        match self.project() {
            Apt(fut) => fut.poll(cx).map_err(OperationApplyError::Apt),
            AptRepo(fut) => fut.poll(cx).map_err(OperationApplyError::AptRepo),
            Aur(fut) => fut.poll(cx).map_err(OperationApplyError::Aur),
            Pacman(fut) => fut.poll(cx).map_err(OperationApplyError::Pacman),
            Podman(fut) => fut.poll(cx).map_err(OperationApplyError::Podman),
            Flatpak(fut) => fut.poll(cx).map_err(OperationApplyError::Flatpak),
            File(fut) => fut.poll(cx).map_err(OperationApplyError::File),
            Directory(fut) => fut.poll(cx).map_err(OperationApplyError::Directory),
            Command(fut) => fut.poll(cx).map_err(OperationApplyError::Command),
            Git(fut) => fut.poll(cx).map_err(OperationApplyError::Git),
            Systemd(fut) => fut.poll(cx).map_err(OperationApplyError::Systemd),
            User(fut) => fut.poll(cx).map_err(OperationApplyError::User),
            Group(fut) => fut.poll(cx).map_err(OperationApplyError::Group),
        }
    }
}

/// Unified stdout stream for any running operation. Implements [`AsyncRead`] by
/// forwarding to the active variant.
#[pin_project(project = OperationApplyStdoutProject)]
pub enum OperationApplyStdout {
    Apt(#[pin] <Apt as OperationType>::ApplyStdout),
    AptRepo(#[pin] <AptRepo as OperationType>::ApplyStdout),
    Aur(#[pin] <Aur as OperationType>::ApplyStdout),
    Pacman(#[pin] <Pacman as OperationType>::ApplyStdout),
    Podman(#[pin] <Podman as OperationType>::ApplyStdout),
    Flatpak(#[pin] <Flatpak as OperationType>::ApplyStdout),
    File(#[pin] <File as OperationType>::ApplyStdout),
    Directory(#[pin] <Directory as OperationType>::ApplyStdout),
    Command(#[pin] <Command as OperationType>::ApplyStdout),
    Git(#[pin] <Git as OperationType>::ApplyStdout),
    Systemd(#[pin] <Systemd as OperationType>::ApplyStdout),
    User(#[pin] <User as OperationType>::ApplyStdout),
    Group(#[pin] <Group as OperationType>::ApplyStdout),
}

impl AsyncRead for OperationApplyStdout {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        use OperationApplyStdoutProject::*;
        match self.project() {
            Apt(stream) => stream.poll_read(cx, buf),
            AptRepo(stream) => stream.poll_read(cx, buf),
            Aur(stream) => stream.poll_read(cx, buf),
            Pacman(stream) => stream.poll_read(cx, buf),
            Podman(stream) => stream.poll_read(cx, buf),
            Flatpak(stream) => stream.poll_read(cx, buf),
            File(stream) => stream.poll_read(cx, buf),
            Directory(stream) => stream.poll_read(cx, buf),
            Command(stream) => stream.poll_read(cx, buf),
            Git(stream) => stream.poll_read(cx, buf),
            Systemd(stream) => stream.poll_read(cx, buf),
            User(stream) => stream.poll_read(cx, buf),
            Group(stream) => stream.poll_read(cx, buf),
        }
    }
}

/// Unified stderr stream for any running operation. Implements [`AsyncRead`] by
/// forwarding to the active variant.
#[pin_project(project = OperationApplyStderrProject)]
pub enum OperationApplyStderr {
    Apt(#[pin] <Apt as OperationType>::ApplyStderr),
    AptRepo(#[pin] <AptRepo as OperationType>::ApplyStderr),
    Aur(#[pin] <Aur as OperationType>::ApplyStderr),
    Pacman(#[pin] <Pacman as OperationType>::ApplyStderr),
    Podman(#[pin] <Podman as OperationType>::ApplyStderr),
    Flatpak(#[pin] <Flatpak as OperationType>::ApplyStderr),
    File(#[pin] <File as OperationType>::ApplyStderr),
    Directory(#[pin] <Directory as OperationType>::ApplyStderr),
    Command(#[pin] <Command as OperationType>::ApplyStderr),
    Git(#[pin] <Git as OperationType>::ApplyStderr),
    Systemd(#[pin] <Systemd as OperationType>::ApplyStderr),
    User(#[pin] <User as OperationType>::ApplyStderr),
    Group(#[pin] <Group as OperationType>::ApplyStderr),
}

impl AsyncRead for OperationApplyStderr {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        use OperationApplyStderrProject::*;
        match self.project() {
            Apt(stream) => stream.poll_read(cx, buf),
            AptRepo(stream) => stream.poll_read(cx, buf),
            Aur(stream) => stream.poll_read(cx, buf),
            Pacman(stream) => stream.poll_read(cx, buf),
            Podman(stream) => stream.poll_read(cx, buf),
            Flatpak(stream) => stream.poll_read(cx, buf),
            File(stream) => stream.poll_read(cx, buf),
            Directory(stream) => stream.poll_read(cx, buf),
            Command(stream) => stream.poll_read(cx, buf),
            Git(stream) => stream.poll_read(cx, buf),
            Systemd(stream) => stream.poll_read(cx, buf),
            User(stream) => stream.poll_read(cx, buf),
            Group(stream) => stream.poll_read(cx, buf),
        }
    }
}

impl Operation {
    /// Start the operation on the target machine. Returns a completion future plus
    /// streaming stdout/stderr. The caller (typically `lusid-apply`) should drive the
    /// future and both streams concurrently so output is surfaced in real time.
    pub async fn apply(
        &self,
        ctx: &mut Context,
    ) -> Result<
        (
            OperationApplyOutput,
            OperationApplyStdout,
            OperationApplyStderr,
        ),
        OperationApplyError,
    > {
        match self {
            Operation::Apt(op) => {
                let (output, stdout, stderr) = Apt::apply(ctx, op)
                    .await
                    .map_err(OperationApplyError::Apt)?;
                Ok((
                    OperationApplyOutput::Apt(output),
                    OperationApplyStdout::Apt(stdout),
                    OperationApplyStderr::Apt(stderr),
                ))
            }
            Operation::AptRepo(op) => {
                let (output, stdout, stderr) = AptRepo::apply(ctx, op)
                    .await
                    .map_err(OperationApplyError::AptRepo)?;
                Ok((
                    OperationApplyOutput::AptRepo(output),
                    OperationApplyStdout::AptRepo(stdout),
                    OperationApplyStderr::AptRepo(stderr),
                ))
            }
            Operation::Aur(op) => {
                let (output, stdout, stderr) = Aur::apply(ctx, op)
                    .await
                    .map_err(OperationApplyError::Aur)?;
                Ok((
                    OperationApplyOutput::Aur(output),
                    OperationApplyStdout::Aur(stdout),
                    OperationApplyStderr::Aur(stderr),
                ))
            }
            Operation::Pacman(op) => {
                let (output, stdout, stderr) = Pacman::apply(ctx, op)
                    .await
                    .map_err(OperationApplyError::Pacman)?;
                Ok((
                    OperationApplyOutput::Pacman(output),
                    OperationApplyStdout::Pacman(stdout),
                    OperationApplyStderr::Pacman(stderr),
                ))
            }
            Operation::Podman(op) => {
                let (output, stdout, stderr) = Podman::apply(ctx, op)
                    .await
                    .map_err(OperationApplyError::Podman)?;
                Ok((
                    OperationApplyOutput::Podman(output),
                    OperationApplyStdout::Podman(stdout),
                    OperationApplyStderr::Podman(stderr),
                ))
            }
            Operation::Flatpak(op) => {
                let (output, stdout, stderr) = Flatpak::apply(ctx, op)
                    .await
                    .map_err(OperationApplyError::Flatpak)?;
                Ok((
                    OperationApplyOutput::Flatpak(output),
                    OperationApplyStdout::Flatpak(stdout),
                    OperationApplyStderr::Flatpak(stderr),
                ))
            }
            Operation::File(op) => {
                let (output, stdout, stderr) = File::apply(ctx, op)
                    .await
                    .map_err(OperationApplyError::File)?;
                Ok((
                    OperationApplyOutput::File(output),
                    OperationApplyStdout::File(stdout),
                    OperationApplyStderr::File(stderr),
                ))
            }
            Operation::Directory(op) => {
                let (output, stdout, stderr) = Directory::apply(ctx, op)
                    .await
                    .map_err(OperationApplyError::Directory)?;
                Ok((
                    OperationApplyOutput::Directory(output),
                    OperationApplyStdout::Directory(stdout),
                    OperationApplyStderr::Directory(stderr),
                ))
            }
            Operation::Command(op) => {
                let (output, stdout, stderr) = Command::apply(ctx, op)
                    .await
                    .map_err(OperationApplyError::Command)?;
                Ok((
                    OperationApplyOutput::Command(output),
                    OperationApplyStdout::Command(stdout),
                    OperationApplyStderr::Command(stderr),
                ))
            }
            Operation::Git(op) => {
                let (output, stdout, stderr) = Git::apply(ctx, op)
                    .await
                    .map_err(OperationApplyError::Git)?;
                Ok((
                    OperationApplyOutput::Git(output),
                    OperationApplyStdout::Git(stdout),
                    OperationApplyStderr::Git(stderr),
                ))
            }
            Operation::Systemd(op) => {
                let (output, stdout, stderr) = Systemd::apply(ctx, op)
                    .await
                    .map_err(OperationApplyError::Systemd)?;
                Ok((
                    OperationApplyOutput::Systemd(output),
                    OperationApplyStdout::Systemd(stdout),
                    OperationApplyStderr::Systemd(stderr),
                ))
            }
            Operation::User(op) => {
                let (output, stdout, stderr) = User::apply(ctx, op)
                    .await
                    .map_err(OperationApplyError::User)?;
                Ok((
                    OperationApplyOutput::User(output),
                    OperationApplyStdout::User(stdout),
                    OperationApplyStderr::User(stderr),
                ))
            }
            Operation::Group(op) => {
                let (output, stdout, stderr) = Group::apply(ctx, op)
                    .await
                    .map_err(OperationApplyError::Group)?;
                Ok((
                    OperationApplyOutput::Group(output),
                    OperationApplyStdout::Group(stdout),
                    OperationApplyStderr::Group(stderr),
                ))
            }
        }
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Operation::*;
        match self {
            Apt(op) => Display::fmt(op, f),
            AptRepo(op) => Display::fmt(op, f),
            Aur(op) => Display::fmt(op, f),
            Pacman(op) => Display::fmt(op, f),
            Podman(op) => Display::fmt(op, f),
            Flatpak(op) => Display::fmt(op, f),
            File(op) => Display::fmt(op, f),
            Directory(op) => Display::fmt(op, f),
            Command(op) => Display::fmt(op, f),
            Git(op) => Display::fmt(op, f),
            Systemd(op) => Display::fmt(op, f),
            User(op) => Display::fmt(op, f),
            Group(op) => Display::fmt(op, f),
        }
    }
}

impl Render for Operation {
    fn render(&self) -> lusid_view::View {
        use Operation::*;
        match self {
            Apt(params) => params.render(),
            AptRepo(params) => params.render(),
            Aur(params) => params.render(),
            File(params) => params.render(),
            Directory(params) => params.render(),
            Pacman(params) => params.render(),
            Podman(params) => params.render(),
            Flatpak(params) => params.render(),
            Command(params) => params.render(),
            Git(params) => params.render(),
            Systemd(params) => params.render(),
            User(params) => params.render(),
            Group(params) => params.render(),
        }
    }
}

/// Operations grouped by family, ready to be fed to each family's `merge`.
#[derive(Debug, Clone)]
pub struct OperationsByType {
    apt: Vec<AptOperation>,
    apt_repo: Vec<AptRepoOperation>,
    aur: Vec<AurOperation>,
    pacman: Vec<PacmanOperation>,
    podman: Vec<PodmanOperation>,
    flatpak: Vec<FlatpakOperation>,
    file: Vec<FileOperation>,
    directory: Vec<DirectoryOperation>,
    command: Vec<CommandOperation>,
    git: Vec<GitOperation>,
    systemd: Vec<SystemdOperation>,
    user: Vec<UserOperation>,
    group: Vec<GroupOperation>,
}

#[cfg(test)]
mod serde_tests {
    use super::*;
    use crate::operations::{
        apt::AptOperation, command::CommandExecutor, command::CommandOperation,
        file::FileOperation, file::FilePath, file::FileSource, systemd::SystemdOperation,
    };

    fn round_trip(op: Operation) {
        let json = serde_json::to_string(&op).unwrap();
        let back: Operation = serde_json::from_str(&json).unwrap();
        assert_eq!(json, serde_json::to_string(&back).unwrap());
    }

    #[test]
    fn round_trip_dispatches_to_each_family() {
        round_trip(Operation::Apt(AptOperation::Install {
            packages: vec!["nginx".into()],
        }));
        round_trip(Operation::File(FileOperation::Write {
            path: FilePath::new("/etc/motd"),
            source: FileSource::Contents(b"hi".to_vec()),
        }));
        round_trip(Operation::Command(CommandOperation {
            command: "echo hi".into(),
            executor: CommandExecutor::Shell,
        }));
        round_trip(Operation::Systemd(SystemdOperation::Reload {
            name: "nginx".into(),
            user: false,
        }));
    }
}

/// Bucket a mixed iterator of operations into per-family vectors.
fn partition_by_type(operations: impl IntoIterator<Item = Operation>) -> OperationsByType {
    let mut apt: Vec<AptOperation> = Vec::new();
    let mut apt_repo: Vec<AptRepoOperation> = Vec::new();
    let mut aur: Vec<AurOperation> = Vec::new();
    let mut pacman: Vec<PacmanOperation> = Vec::new();
    let mut podman: Vec<PodmanOperation> = Vec::new();
    let mut flatpak: Vec<FlatpakOperation> = Vec::new();
    let mut file: Vec<FileOperation> = Vec::new();
    let mut directory: Vec<DirectoryOperation> = Vec::new();
    let mut command: Vec<CommandOperation> = Vec::new();
    let mut git: Vec<GitOperation> = Vec::new();
    let mut systemd: Vec<SystemdOperation> = Vec::new();
    let mut user: Vec<UserOperation> = Vec::new();
    let mut group: Vec<GroupOperation> = Vec::new();
    for operation in operations.into_iter() {
        match operation {
            Operation::Apt(op) => apt.push(op),
            Operation::AptRepo(op) => apt_repo.push(op),
            Operation::Aur(op) => aur.push(op),
            Operation::Pacman(op) => pacman.push(op),
            Operation::Podman(op) => podman.push(op),
            Operation::Flatpak(op) => flatpak.push(op),
            Operation::File(op) => file.push(op),
            Operation::Directory(op) => directory.push(op),
            Operation::Command(op) => command.push(op),
            Operation::Git(op) => git.push(op),
            Operation::Systemd(op) => systemd.push(op),
            Operation::User(op) => user.push(op),
            Operation::Group(op) => group.push(op),
        }
    }
    OperationsByType {
        apt,
        apt_repo,
        aur,
        pacman,
        podman,
        flatpak,
        file,
        directory,
        command,
        git,
        systemd,
        user,
        group,
    }
}
