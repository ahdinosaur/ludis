use async_trait::async_trait;
use core::task;
use lusid_ctx::Context;
use lusid_view::Render;
use pin_project::pin_project;
use std::{
    fmt::{Debug, Display},
    pin::{pin, Pin},
    task::Poll,
};
use thiserror::Error;
use tokio::io::AsyncRead;

pub mod operations;

use crate::operations::apt::{Apt, AptOperation};

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
}

impl Operation {
    /// Merge a set of operations by type.
    pub fn merge(operations: Vec<Operation>) -> Vec<Operation> {
        let OperationsByType { apt } = partition_by_type(operations);

        let mut result = Vec::new();

        result.extend(Apt::merge(apt).into_iter().map(Operation::Apt));

        result
    }
}

#[derive(Error, Debug)]
pub enum OperationApplyError {
    #[error("apt operation failed: {0:?}")]
    Apt(<Apt as OperationType>::ApplyError),
}

#[pin_project(project = OperationApplyOutputProject)]
pub enum OperationApplyOutput {
    Apt(#[pin] <Apt as OperationType>::ApplyOutput),
}

impl Future for OperationApplyOutput {
    type Output = Result<(), OperationApplyError>;

    fn poll(self: Pin<&mut Self>, cx: &mut task::Context<'_>) -> Poll<Self::Output> {
        use OperationApplyOutputProject::*;
        match self.project() {
            Apt(fut) => fut.poll(cx).map_err(OperationApplyError::Apt),
        }
    }
}

#[derive(Debug)]
#[pin_project(project = OperationApplyStdoutProject)]
pub enum OperationApplyStdout {
    Apt(#[pin] <Apt as OperationType>::ApplyStdout),
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
        }
    }
}

#[derive(Debug)]
#[pin_project(project = OperationApplyStderrProject)]
pub enum OperationApplyStderr {
    Apt(#[pin] <Apt as OperationType>::ApplyStderr),
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
        }
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Operation::*;
        match self {
            Apt(apt) => Display::fmt(apt, f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct OperationsByType {
    apt: Vec<AptOperation>,
}

/// Merge a set of operations by type.
fn partition_by_type(operations: Vec<Operation>) -> OperationsByType {
    let mut apt: Vec<AptOperation> = Vec::new();
    for operation in operations {
        match operation {
            Operation::Apt(op) => apt.push(op),
        }
    }
    OperationsByType { apt }
}
