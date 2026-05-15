//! Abstract content-addressed store for file-like bytes referenced by a plan.
//!
//! [`Store`] multiplexes over one or more [`SubStore`] backends. Today there's only
//! `LocalFile` (read straight off disk), but the shape is deliberately extensible:
//! future backends could cover HTTP URLs, git blobs, or content-hashed blobs living
//! in the XDG cache directory.
//
// TODO(cc): the only backend today is a thin wrapper around `tokio::fs::read`. When
// adding remote backends, wire up the `cache_dir` argument that `SubStore::new`
// already receives (currently ignored by `LocalFileStore`).

use async_trait::async_trait;
use displaydoc::Display;
use std::{
    fmt::Debug,
    io,
    path::{Path, PathBuf},
};
use thiserror::Error;

/// A single storage backend (e.g. local file, remote URL, git blob).
#[async_trait]
pub trait SubStore {
    type ItemId;
    type Error: Debug;

    fn new(cache_dir: PathBuf) -> Self;

    async fn read(&mut self, id: &Self::ItemId) -> Result<Vec<u8>, Self::Error>;
}

/// Multiplexed store: dispatches a [`StoreItemId`] to the right backend.
#[derive(Debug, Clone)]
pub struct Store {
    local_file_store: LocalFileStore,
}

/// Tagged identifier for a store item - picks which backend handles the read.
#[derive(Debug, Clone)]
pub enum StoreItemId {
    LocalFile(PathBuf),
}

#[derive(Debug, Error, Display)]
pub enum StoreError {
    /// Local file store failed
    LocalFile(#[from] io::Error),
}

impl Store {
    pub fn new(cache_dir: &Path) -> Self {
        Self {
            local_file_store: LocalFileStore::new(cache_dir.join("files")),
        }
    }

    pub async fn read(&mut self, id: &StoreItemId) -> Result<Vec<u8>, StoreError> {
        match id {
            StoreItemId::LocalFile(id) => self
                .local_file_store
                .read(id)
                .await
                .map_err(StoreError::from),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct LocalFileStore;

#[async_trait]
impl SubStore for LocalFileStore {
    type ItemId = PathBuf;
    type Error = io::Error;

    fn new(_cache_dir: PathBuf) -> Self {
        Self
    }

    async fn read(&mut self, id: &Self::ItemId) -> Result<Vec<u8>, Self::Error> {
        tokio::fs::read(id).await
    }
}
