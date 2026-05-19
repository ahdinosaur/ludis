use lusid_ctx::{Context as BaseContext, ContextError as BaseContextError};
use thiserror::Error;

use crate::paths::{ExecutablePaths, ExecutablePathsError, Paths};
use lusid_http::{HttpClient, HttpError};

#[derive(Error, Debug)]
pub enum ContextError {
    #[error(transparent)]
    Http(#[from] HttpError),

    #[error(transparent)]
    Context(#[from] BaseContextError),

    #[error(transparent)]
    ExecutablePaths(#[from] ExecutablePathsError),
}

/// VM-crate-internal context: the pieces of the base [`BaseContext`] that the
/// VM pipeline touches (HTTP for image downloads, filesystem paths for
/// images/instances, resolved executable paths for qemu/qemu-img/mkisofs).
#[derive(Debug, Clone)]
pub struct Context {
    http_client: HttpClient,
    paths: Paths,
    executables: ExecutablePaths,
}

impl Context {
    pub fn create(base: &mut BaseContext) -> Result<Self, ContextError> {
        let http_client = base.http_client().clone();
        let paths = Paths::new(base.paths().clone());
        let executables = ExecutablePaths::new()?;
        Ok(Self {
            http_client,
            paths,
            executables,
        })
    }

    pub fn http_client(&mut self) -> &mut HttpClient {
        &mut self.http_client
    }

    pub fn paths(&self) -> &Paths {
        &self.paths
    }

    pub fn executables(&self) -> &ExecutablePaths {
        &self.executables
    }
}
