use russh::client::Handler;
use russh_sftp::{
    client::{SftpSession, error::Error as SftpError},
    protocol::{FileAttributes, OpenFlags},
};
use std::{
    borrow::Cow,
    fmt::Debug,
    path::{Path, PathBuf},
};
use thiserror::Error;
use tokio::{
    fs as tfs,
    io::{AsyncReadExt, AsyncWriteExt},
};
use tracing::{debug, info, instrument, trace, warn};

use lusid_fs::{self as fs, FsError};

use crate::session::{AsyncSession, HostKeyHandler};

#[derive(Clone, PartialEq, Eq)]
pub enum SshVolume {
    DirPath {
        local: PathBuf,
        remote: String,
    },
    FilePath {
        local: PathBuf,
        remote: String,
    },
    /// Upload an in-memory byte blob. `local` is a `Cow` so callers can hand
    /// over either runtime-built bytes (e.g. re-encrypted secret ciphertexts)
    /// via `Cow::Owned`, or compile-time-embedded blobs (e.g. the embedded
    /// `lusid-apply` binary) via `Cow::Borrowed(&'static [u8])` without an
    /// extra ~10–30 MB clone per apply.
    FileBytes {
        local: Cow<'static, [u8]>,
        permissions: Option<u32>,
        remote: String,
    },
}

impl Debug for SshVolume {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SshVolume::DirPath { local, remote } => {
                write!(f, "{}:{}", local.display(), remote)
            }
            SshVolume::FilePath { local, remote } => {
                write!(f, "{}:{}", local.display(), remote)
            }
            SshVolume::FileBytes { local, remote, .. } => {
                write!(f, "<{} bytes>:{}", local.len(), remote)
            }
        }
    }
}

#[derive(Error, Debug)]
pub enum SshSyncError {
    #[error("filesystem error: {0}")]
    Fs(#[from] FsError),

    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    #[error("SSH protocol error: {0}")]
    Russh(#[from] russh::Error),

    #[error("SFTP error: {0}")]
    RusshSftp(#[from] SftpError),

    // TODO(cc): these two variants are declared but never returned - symlinks are
    // currently skipped with a `warn!` log rather than erroring. Either wire them up
    // at the top-level entry point or delete them.
    #[error("refusing to upload: top-level source is a symlink")]
    TopLevelSymlink,

    #[error("unsupported source type (must be file or directory)")]
    UnsupportedSource,

    #[error("source path must be a directory")]
    SourceMustBeDirectory,
}

#[instrument(skip(session))]
pub(super) async fn ssh_sync(
    session: &AsyncSession<HostKeyHandler>,
    volume: SshVolume,
) -> Result<(), SshSyncError> {
    info!("Starting SSH volume sync");
    let mut sftp = open_sftp(session).await?;
    sftp_upload_volume(&mut sftp, &volume).await?;
    info!("Volume sync completed");
    Ok(())
}

#[instrument(skip_all)]
async fn open_sftp<H: Handler + 'static>(
    session: &AsyncSession<H>,
) -> Result<SftpSession, SshSyncError> {
    let channel = session.open_channel().await?;
    channel.request_subsystem(true, "sftp").await?;
    let sftp = SftpSession::new(tokio::io::join(channel.stdout(), channel.stdin())).await?;
    Ok(sftp)
}

async fn sftp_upload_volume(
    sftp: &mut SftpSession,
    volume: &SshVolume,
) -> Result<(), SshSyncError> {
    match volume {
        SshVolume::DirPath { local, remote } => sftp_upload_dir(sftp, local, remote).await,
        SshVolume::FilePath { local, remote } => sftp_upload_file(sftp, local, remote).await,
        SshVolume::FileBytes {
            local,
            permissions,
            remote,
        } => sftp_upload_file_bytes(sftp, local, *permissions, remote).await,
    }
}

#[instrument(skip(sftp))]
async fn sftp_upload_dir(
    sftp: &mut SftpSession,
    local_root: &Path,
    remote_root: &str,
) -> Result<(), SshSyncError> {
    if !local_root.is_dir() {
        return Err(SshSyncError::SourceMustBeDirectory);
    }

    trace!(remote = %remote_root, "Ensuring remote destination root exists");
    sftp_mkdirs(sftp, remote_root).await?;

    let mut stack: Vec<PathBuf> = vec![local_root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        let rel = dir.strip_prefix(local_root).unwrap_or(Path::new(""));
        let remote_dir = remote_join(remote_root, rel);

        trace!(
            local = %dir.display(),
            remote = %remote_dir,
            "Ensuring remote directory exists"
        );
        sftp_mkdirs(sftp, &remote_dir).await?;

        let entries = fs::read_dir(&dir).await?;
        for path in entries {
            let md = tfs::symlink_metadata(&path).await?;

            if md.file_type().is_symlink() {
                warn!(path = %path.display(), "Skipping symlink");
                continue;
            }

            if md.is_dir() {
                stack.push(path);
            } else if md.is_file() {
                let rel = path.strip_prefix(local_root).unwrap_or(Path::new(""));
                let remote_file = remote_join(remote_root, rel);
                sftp_upload_file(sftp, &path, &remote_file).await?;
            } else {
                warn!(
                    path = %path.display(),
                    "Skipping special/unsupported file type"
                );
                continue;
            }
        }
    }

    debug!("Directory upload completed");
    Ok(())
}

#[instrument(skip(sftp))]
async fn sftp_upload_file(
    sftp: &mut SftpSession,
    local: &Path,
    remote: &str,
) -> Result<(), SshSyncError> {
    #[allow(clippy::collapsible_if)]
    if let Some(parent) = remote_parent(remote) {
        if !parent.is_empty() {
            trace!(parent, "Ensuring remote parent directory exists");
            sftp_mkdirs(sftp, parent).await?;
        }
    }

    let mut local_file = fs::open_file(local).await?;
    let local_metadata = local_file.metadata().await?;
    let size = local_metadata.len();
    trace!(local = %local.display(), size_bytes = size, "Opened local file");

    let flags = OpenFlags::CREATE
        .union(OpenFlags::TRUNCATE)
        .union(OpenFlags::WRITE);
    let mut remote_file = sftp.open_with_flags(remote, flags).await?;
    trace!("Opened remote file for writing");

    let mut buf = vec![0u8; 128 * 1024];
    loop {
        let n = local_file.read(&mut buf).await?;
        if n == 0 {
            break;
        }
        remote_file.write_all(&buf[..n]).await?;
    }

    remote_file.flush().await?;
    remote_file.shutdown().await?;

    let remote_metadata: FileAttributes = (&local_metadata).into();
    sftp.set_metadata(remote, remote_metadata).await?;

    debug!("File upload completed");
    Ok(())
}

#[instrument(skip(sftp))]
async fn sftp_upload_file_bytes(
    sftp: &mut SftpSession,
    local: &[u8],
    permissions: Option<u32>,
    remote: &str,
) -> Result<(), SshSyncError> {
    #[allow(clippy::collapsible_if)]
    if let Some(parent) = remote_parent(remote) {
        if !parent.is_empty() {
            trace!(parent, "Ensuring remote parent directory exists");
            sftp_mkdirs(sftp, parent).await?;
        }
    }

    let flags = OpenFlags::CREATE
        .union(OpenFlags::TRUNCATE)
        .union(OpenFlags::WRITE);
    let mut remote_file = sftp.open_with_flags(remote, flags).await?;
    trace!("Opened remote file for writing");

    remote_file.write_all(local).await?;
    remote_file.flush().await?;
    remote_file
        .set_metadata(FileAttributes {
            permissions,
            ..FileAttributes::empty()
        })
        .await?;
    remote_file.shutdown().await?;

    debug!("File upload completed");
    Ok(())
}

#[instrument(skip(sftp))]
async fn sftp_mkdirs(sftp: &mut SftpSession, remote_dir: &str) -> Result<(), SshSyncError> {
    let remote_dir = remote_dir.trim();
    if remote_dir.is_empty() || remote_dir == "." {
        return Ok(());
    }

    let mut accum = String::new();
    if remote_dir.starts_with('/') {
        accum.push('/');
    }

    for seg in remote_dir.split('/').filter(|s| !s.is_empty()) {
        if accum.is_empty() || accum == "/" {
            accum.push_str(seg);
        } else {
            accum.push('/');
            accum.push_str(seg);
        }

        if sftp.try_exists(&accum).await? {
            let metadata = sftp.metadata(&accum).await?;
            if metadata.is_dir() {
                trace!(path = %accum, "Remote directory already exists");
            } else {
                warn!(
                    path = %accum,
                    "Remote path exists but is not a directory; continuing"
                );
            }
            continue;
        }

        match sftp.create_dir(&accum).await {
            Ok(_) => trace!(path = %accum, "Created remote directory"),
            Err(e) => {
                tracing::error!(
                    path = %accum,
                    error = %e,
                    "Failed to create remote directory"
                );
                return Err(SshSyncError::from(e));
            }
        }
    }

    Ok(())
}

fn remote_join(base: &str, rel: &Path) -> String {
    if rel.as_os_str().is_empty() {
        return base.to_string();
    }
    let mut out = base.trim_end_matches('/').to_string();
    for c in rel.components() {
        use std::path::Component;
        match c {
            Component::Normal(seg) => {
                out.push('/');
                out.push_str(&seg.to_string_lossy());
            }
            Component::CurDir => {}
            Component::ParentDir => {}
            _ => {}
        }
    }
    if out.is_empty() { "/".to_string() } else { out }
}

fn remote_parent(path: &str) -> Option<&str> {
    match path.rsplit_once('/') {
        Some((parent, _)) => Some(parent),
        None => None,
    }
}
