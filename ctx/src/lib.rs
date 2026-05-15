//! Shared runtime context passed through the planning and apply pipeline.
//!
//! A [`Context`] bundles things every stage needs: the plan root directory (used to
//! resolve `HostPath` params relative to the source file), platform-specific data/cache
//! paths ([`Paths`]), and a reusable HTTP client. Construct once per run and hand it
//! down; prefer adding fields here over threading new arguments everywhere.

mod paths;

use std::path::{Path, PathBuf};

use lusid_http::{HttpClient, HttpError};
use lusid_secrets::Secrets;
use thiserror::Error;

pub use crate::paths::{Paths, PathsError};

#[derive(Error, Debug)]
pub enum ContextError {
    #[error(transparent)]
    Paths(#[from] PathsError),

    #[error(transparent)]
    Http(#[from] HttpError),
}

/// Runtime context for a lusid invocation - plan root, XDG paths, HTTP client,
/// decrypted secrets bundle.
#[derive(Debug, Clone)]
pub struct Context {
    root: PathBuf,
    paths: Paths,
    http: HttpClient,
    secrets: Secrets,
}

impl Context {
    pub fn create(root: &Path) -> Result<Self, ContextError> {
        let paths = Paths::create()?;
        let http = HttpClient::new()?;
        Ok(Self {
            root: root.to_path_buf(),
            paths,
            http,
            secrets: Secrets::empty(),
        })
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn paths(&self) -> &Paths {
        &self.paths
    }

    pub fn http_client(&mut self) -> &mut HttpClient {
        &mut self.http
    }

    pub fn secrets(&self) -> &Secrets {
        &self.secrets
    }

    pub fn set_secrets(&mut self, secrets: Secrets) {
        self.secrets = secrets;
    }
}
