//! Platform-specific directories (data/cache/runtime) for lusid.
//!
//! On Linux follows the XDG Base Directory spec; on macOS uses `~/Library` conventions;
//! on Windows uses `%LOCALAPPDATA%` / `%TEMP%`. Each directory is suffixed with the
//! project name.
//
// Inspiration: https://github.com/cubic-vm/cubic/blob/68566f79d72e2037bce1b75246d92e6da7b999e5/src/env/environment_factory.rs

use std::{
    env::{self, VarError},
    path::{Path, PathBuf},
};
use thiserror::Error;

const PROJECT_NAME: &str = "lusid";

/// Platform-specific directory bundle for lusid: persistent data, transient cache,
/// and runtime (sockets / pid files) roots.
#[derive(Debug, Clone)]
pub struct Paths {
    data_dir: PathBuf,
    cache_dir: PathBuf,
    runtime_dir: PathBuf,
}

#[derive(Error, Debug, Clone)]
pub enum PathsError {
    #[error(transparent)]
    Var(#[from] VarError),
}

impl Paths {
    pub fn new(data_dir: PathBuf, cache_dir: PathBuf, runtime_dir: PathBuf) -> Self {
        Self {
            data_dir,
            cache_dir,
            runtime_dir,
        }
    }

    #[cfg(target_os = "linux")]
    pub fn create() -> Result<Paths, PathsError> {
        let data_dirs: PathBuf = Self::var("XDG_DATA_HOME")
            .or_else(|_| Self::var("HOME").map(|home| format!("{home}/.local/share")))
            .map(From::from)?;
        let cache_dirs: PathBuf = Self::var("XDG_CACHE_HOME")
            .or_else(|_| Self::var("HOME").map(|home| format!("{home}/.cache")))
            .map(From::from)?;
        // TODO(cc): `UID` is a shell variable - it's set in bash/zsh but not usually
        // exported, so this fallback often fails. Prefer `nix::unistd::getuid()` (already
        // a workspace dep) to look up the real uid.
        let runtime_dirs: PathBuf = Self::var("XDG_RUNTIME_DIR")
            .or_else(|_| Self::var("UID").map(|uid| format!("/run/user/{uid}")))
            .map(From::from)?;

        Ok(Paths::new(
            data_dirs.join(PROJECT_NAME),
            cache_dirs.join(PROJECT_NAME),
            runtime_dirs.join(PROJECT_NAME),
        ))
    }

    #[cfg(target_os = "macos")]
    pub fn create() -> Result<Paths, PathsError> {
        let home_dir: PathBuf = Self::var("HOME").map(From::from)?;
        Ok(Paths::new(
            home_dir.join("Library").join(PROJECT_NAME),
            home_dir.join("Library").join("Caches").join(PROJECT_NAME),
            home_dir.join("Library").join("Caches").join(PROJECT_NAME),
        ))
    }

    #[cfg(target_os = "windows")]
    pub fn create() -> Result<Paths, PathsError> {
        let local_app_data_dir: PathBuf = Self::var("LOCALAPPDATA").map(From::from)?;
        let temp_dir: PathBuf = Self::var("TEMP").map(From::from)?;
        Ok(Paths::new(
            local_app_data_dir.join(PROJECT_NAME),
            temp_dir.join(PROJECT_NAME),
            temp_dir.join(PROJECT_NAME),
        ))
    }

    pub fn data_dir(&self) -> &Path {
        &self.data_dir
    }
    pub fn cache_dir(&self) -> &Path {
        &self.cache_dir
    }
    pub fn runtime_dir(&self) -> &Path {
        &self.runtime_dir
    }

    fn var(var: &str) -> Result<String, PathsError> {
        env::var(var).map_err(From::from)
    }
}
