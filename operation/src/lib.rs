use async_trait::async_trait;
use core::task;
use lusid_ctx::Context;
use lusid_view::Render;
use pin_project::pin_project;
use std::{
    fmt::{Debug, Display},
    future::Future,
    pin::{pin, Pin},
    task::Poll,
};
use thiserror::Error;
use tokio::io::AsyncRead;

pub mod operations;

use crate::operations::{
    apt::{Apt, AptOperation},
    file::{File, FileOperation},
    pacman::{Pacman, PacmanOperation},
};

/// OperationType specifies how to merge and apply a concrete Operation type.
///
/// Operations are the results of ResourceChanges and are executed per epoch.
/// Each type decides how to merge same-type operations and how to apply them.
#[async_trait]
pub trait OperationType {
    type Operation: Render;

    /// Merge a set of operations of this type within the same epoch.
    /// Implementations should coalesce operations to a minimal set.
    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation>;

    type ApplyError;
    type ApplyStdout: AsyncRead;
    type ApplyStderr: AsyncRead;
    type ApplyOutput: Future<Output = Result<(), Self::ApplyError>>;

    /// Apply an operation of this type.
    async fn apply(
        ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError>;
}

#[derive(Debug, Clone)]
pub enum Operation {
    Apt(AptOperation),
    Pacman(PacmanOperation),
    File(FileOperation),
}

impl Operation {
    /// Merge a set of operations by type.
    pub fn merge(operations: Vec<Operation>) -> Vec<Operation> {
        let OperationsByType { apt, pacman, file } = partition_by_type(operations);

        std::iter::empty()
            .chain(Apt::merge(apt).into_iter().map(Operation::Apt))
            .chain(Pacman::merge(pacman).into_iter().map(Operation::Pacman))
            .chain(File::merge(file).into_iter().map(Operation::File))
            .collect()
    }
}

#[derive(Error, Debug)]
pub enum OperationApplyError {
    #[error("apt operation failed: {0:?}")]
    Apt(<Apt as OperationType>::ApplyError),
    #[error("pacman operation failed: {0:?}")]
    Pacman(<Pacman as OperationType>::ApplyError),
    #[error("file operation failed: {0:?}")]
    File(<File as OperationType>::ApplyError),
}

#[pin_project(project = OperationApplyOutputProject)]
pub enum OperationApplyOutput {
    Apt(#[pin] <Apt as OperationType>::ApplyOutput),
    Pacman(#[pin] <Pacman as OperationType>::ApplyOutput),
    File(#[pin] <File as OperationType>::ApplyOutput),
}

impl Future for OperationApplyOutput {
    type Output = Result<(), OperationApplyError>;

    fn poll(self: Pin<&mut Self>, cx: &mut task::Context<'_>) -> Poll<Self::Output> {
        use OperationApplyOutputProject::*;
        match self.project() {
            Apt(fut) => fut.poll(cx).map_err(OperationApplyError::Apt),
            Pacman(fut) => fut.poll(cx).map_err(OperationApplyError::Pacman),
            File(fut) => fut.poll(cx).map_err(OperationApplyError::File),
        }
    }
}

#[pin_project(project = OperationApplyStdoutProject)]
pub enum OperationApplyStdout {
    Apt(#[pin] <Apt as OperationType>::ApplyStdout),
    Pacman(#[pin] <Pacman as OperationType>::ApplyStdout),
    File(#[pin] <File as OperationType>::ApplyStdout),
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
            Pacman(stream) => stream.poll_read(cx, buf),
            File(stream) => stream.poll_read(cx, buf),
        }
    }
}

#[pin_project(project = OperationApplyStderrProject)]
pub enum OperationApplyStderr {
    Apt(#[pin] <Apt as OperationType>::ApplyStderr),
    Pacman(#[pin] <Pacman as OperationType>::ApplyStderr),
    File(#[pin] <File as OperationType>::ApplyStderr),
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
            Pacman(stream) => stream.poll_read(cx, buf),
            File(stream) => stream.poll_read(cx, buf),
        }
    }
}

impl Operation {
    /// Apply a set of operations by type
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
        }
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Operation::*;
        match self {
            Apt(op) => Display::fmt(op, f),
            Pacman(op) => Display::fmt(op, f),
            File(op) => Display::fmt(op, f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct OperationsByType {
    apt: Vec<AptOperation>,
    pacman: Vec<PacmanOperation>,
    file: Vec<FileOperation>,
}

/// Merge a set of operations by type.
fn partition_by_type(operations: Vec<Operation>) -> OperationsByType {
    let mut apt: Vec<AptOperation> = Vec::new();
    let mut pacman: Vec<PacmanOperation> = Vec::new();
    let mut file: Vec<FileOperation> = Vec::new();
    for operation in operations {
        match operation {
            Operation::Apt(op) => apt.push(op),
            Operation::Pacman(op) => pacman.push(op),
            Operation::File(op) => file.push(op),
        }
    }
    OperationsByType { apt, pacman, file }
}
