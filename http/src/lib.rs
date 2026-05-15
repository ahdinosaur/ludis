//! Thin HTTP client for fetching remote artifacts during plan apply.
//!
//! Wraps `reqwest` with sensible defaults (gzip/brotli, read timeout) and exposes
//! a streaming `download_file` that writes through a `.tmp` sidecar and renames on
//! success - so partial downloads never appear as completed files.

use std::ffi::OsString;
use std::path::{Path, PathBuf};

use lusid_fs::{self as fs, FsError};
use reqwest::Client;
use thiserror::Error;
use tokio::io::AsyncWriteExt;
use tokio_stream::StreamExt;

const REQUEST_TIMEOUT_SEC: u64 = 10;

#[derive(Error, Debug)]
pub enum HttpError {
    #[error("Failed to build HTTP client: {0}")]
    BuildClient(#[source] reqwest::Error),

    #[error("HTTP request error: {0}")]
    Request(#[source] reqwest::Error),

    #[error("HTTP stream error: {0}")]
    Stream(#[source] reqwest::Error),

    #[error("File write error for '{path}': {source}")]
    Write {
        path: PathBuf,
        source: std::io::Error,
    },

    #[error(transparent)]
    Fs(#[from] FsError),
}

#[derive(Debug, Clone)]
pub struct HttpClient {
    client: Client,
}

impl HttpClient {
    pub fn new() -> Result<Self, HttpError> {
        let client = Client::builder()
            .read_timeout(std::time::Duration::from_secs(REQUEST_TIMEOUT_SEC))
            .gzip(true)
            .brotli(true)
            .build()
            .map_err(HttpError::BuildClient)?;
        Ok(HttpClient { client })
    }

    #[allow(dead_code)]
    pub async fn get_file_size(&self, url: &str) -> Result<Option<u64>, HttpError> {
        let resp = self
            .client
            .head(url)
            .send()
            .await
            .map_err(HttpError::Request)?;
        let size = resp
            .headers()
            .get("Content-Length")
            .and_then(|v| v.to_str().ok())
            .and_then(|s| s.parse::<u64>().ok());
        Ok(size)
    }

    /// Stream a URL to a file on disk.
    ///
    /// If `file_path` already exists, this is a no-op - the URL is trusted to be
    /// content-stable. The download is staged to a `.tmp` sidecar and atomically
    /// renamed on success, so interrupted runs don't leave a half-written file
    /// masquerading as complete.
    ///
    /// Note(cc): no retry, resume, or content verification (checksum, etag) yet.
    /// If the URL changes under us, we'll silently use the stale local copy.
    pub async fn download_file<P: AsRef<Path>>(
        &self,
        url: &str,
        file_path: P,
    ) -> Result<(), HttpError> {
        let file_path = file_path.as_ref();
        if fs::path_exists(file_path).await? {
            return Ok(());
        }

        let temp_file = with_added_extension(file_path, "tmp");
        if fs::path_exists(&temp_file).await? {
            fs::remove_file(&temp_file).await?;
        }

        let resp = self
            .client
            .get(url)
            .send()
            .await
            .map_err(HttpError::Request)?;

        let mut file = fs::create_file(&temp_file).await?;
        let mut stream = resp.bytes_stream();

        while let Some(chunk) = stream.next().await {
            let bytes = chunk.map_err(HttpError::Stream)?;
            file.write_all(&bytes)
                .await
                .map_err(|source| HttpError::Write {
                    path: temp_file.clone(),
                    source,
                })?;
        }

        file.flush().await.map_err(|source| HttpError::Write {
            path: temp_file.clone(),
            source,
        })?;

        fs::rename_file(&temp_file, file_path).await?;
        Ok(())
    }

    #[allow(dead_code)]
    pub async fn download_content(&self, url: &str) -> Result<String, HttpError> {
        self.client
            .get(url)
            .send()
            .await
            .map_err(HttpError::Request)?
            .text()
            .await
            .map_err(HttpError::Request)
    }
}

// Produce "<orig_ext>.<added>" if an extension exists, otherwise "added".
fn with_added_extension(path: &Path, added: &str) -> PathBuf {
    let mut new_ext = OsString::new();
    if let Some(ext) = path.extension() {
        new_ext.push(ext);
        new_ext.push(".");
    }
    new_ext.push(added);
    path.with_extension(new_ext)
}
