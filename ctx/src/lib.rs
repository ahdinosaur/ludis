mod paths;

use std::path::{Path, PathBuf};

use lusid_http::{HttpClient, HttpError};
use thiserror::Error;

pub use crate::paths::{Paths, PathsError};

#[derive(Error, Debug)]
pub enum ContextError {
    #[error(transparent)]
    Paths(#[from] PathsError),

    #[error(transparent)]
    Http(#[from] HttpError),
}

#[derive(Debug, Clone)]
pub struct Context {
    root: PathBuf,
    paths: Paths,
    http: HttpClient,
}

impl Context {
    pub fn create(root: &Path) -> Result<Self, ContextError> {
        let paths = Paths::create()?;
        let http = HttpClient::new()?;
        Ok(Self {
            root: root.to_path_buf(),
            paths,
            http,
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
}
