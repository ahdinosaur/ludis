//! Async filesystem helpers used by lusid operations and resources.
//!
//! Every function wraps a `tokio::fs` / `nix` / `filetime` call with a contextual
//! [`FsError`] variant — errors always include the offending path(s), so diagnostics
//! don't require parsing raw `io::Error` messages.
//!
//! Highlights:
//! - [`write_file_atomic`] / [`copy_file_atomic`]: write to a sibling temp file, copy
//!   destination metadata (or source metadata, respectively), then rename. This means
//!   readers never observe a half-written file.
//! - [`change_owner`] / [`change_owner_by_id`]: uid/gid changes, Unix-only.
//! - [`copy_dir`]: shells out to `cp --recursive` (see the note on the function for
//!   portability caveats).
//
// TODO(cc): like `lusid-cmd`, this crate relies on tokio features (`fs`, `io-util`,
// `process`) that are enabled transitively via the workspace rather than declared in
// its own `Cargo.toml`. `cargo check -p lusid-fs` in isolation fails. Declare the
// needed tokio features locally.

use nix::unistd::{Group, User};
use std::os::unix::fs::{MetadataExt, PermissionsExt};
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::time::SystemTime;

use filetime::FileTime;
use thiserror::Error;
use tokio::fs::{self};
use tokio::io::AsyncWriteExt;
use tokio::process::Command;

#[derive(Error, Debug)]
pub enum FsError {
    #[error("Cannot create directory '{path}': {source}")]
    CreateDir {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Failed to spawn copy from '{from}' to '{to}': {source}")]
    CopyDirSpawn {
        from: PathBuf,
        to: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Failed waiting for copy from '{from}' to '{to}': {source}")]
    CopyDirWait {
        from: PathBuf,
        to: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Copy command returned non-zero status from '{from}' to '{to}'")]
    CopyDirStatus { from: PathBuf, to: PathBuf },

    #[error("Cannot read directory '{path}': {source}")]
    ReadDir {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Cannot iterate directory '{path}': {source}")]
    ReadDirEntry {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Cannot remove directory '{path}': {source}")]
    RemoveDir {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Cannot read metadata '{path}': {source}")]
    Metadata {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Cannot change mode '{path}' to {mode}: {source}")]
    ChangeMode {
        path: PathBuf,
        mode: u32,
        #[source]
        source: std::io::Error,
    },

    #[error("Cannot set permissions '{path}': {source}")]
    SetPermissions {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Cannot change owner '{path}' to user {uid:?} + group {gid:?}: {source}")]
    ChangeOwner {
        path: PathBuf,
        uid: Option<u32>,
        gid: Option<u32>,
        #[source]
        source: std::io::Error,
    },

    #[error("Failed to get user from name: {user}")]
    UserFromName {
        user: String,
        #[source]
        source: nix::Error,
    },

    #[error("Failed to get user from uid: {uid}")]
    UserFromUid {
        uid: u32,
        #[source]
        source: nix::Error,
    },

    #[error("User not found: {user}")]
    UserNotFound { user: String },

    #[error("Failed to get group from name: {group}")]
    GroupFromName {
        group: String,
        #[source]
        source: nix::Error,
    },

    #[error("Failed to get group from gid: {gid}")]
    GroupFromGid {
        gid: u32,
        #[source]
        source: nix::Error,
    },

    #[error("Group not found: {group}")]
    GroupNotFound { group: String },

    #[error("Cannot write directory '{path}' (read-only)")]
    ReadOnlyDir { path: PathBuf },

    #[error("Cannot create file '{path}': {source}")]
    CreateFile {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Cannot open file '{path}': {source}")]
    OpenFile {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Cannot determine if path exists '{path}': {source}")]
    PathExists {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Cannot write file '{path}': {source}")]
    WriteFile {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Cannot read file '{path}': {source}")]
    ReadFile {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Cannot copy file from '{from}' to '{to}': {source}")]
    CopyFile {
        from: PathBuf,
        to: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Cannot rename file from '{from}' to '{to}': {source}")]
    RenameFile {
        from: PathBuf,
        to: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Cannot delete file '{path}': {source}")]
    RemoveFile {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Cannot create symlink from '{from}' to '{to}': {source}")]
    CreateSymlink {
        from: PathBuf,
        to: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Cannot read symlink '{path}': {source}")]
    ReadSymlink {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Failed to set file times: {source}")]
    SetFileTimes {
        #[source]
        source: std::io::Error,
    },
}

pub async fn create_dir<P: AsRef<Path>>(path: P) -> Result<(), FsError> {
    let p = path.as_ref();
    fs::create_dir_all(p)
        .await
        .map_err(|source| FsError::CreateDir {
            path: p.to_path_buf(),
            source,
        })
}

/// Recursively copy a directory tree.
///
/// Note(cc): shells out to `cp --recursive`, which is GNU coreutils — BSD `cp` on
/// macOS uses `-R` instead, and Windows has no `cp` at all. If we ever care about
/// non-Linux targets, swap to a Rust-native walker (e.g. the `walkdir` + manual copy
/// pattern, or the `fs_extra` crate).
///
/// Note(cc): callers must ensure `to` does not exist. GNU `cp -r src dst` when
/// `dst` already exists creates `dst/<basename of src>` (a nested copy) instead
/// of replacing `dst`'s contents. The current sole caller —
/// `DirectoryOperation::CopyTree` from `@core/directory state: "sourced"` in
/// guest mode — short-circuits via the directory-resource state probe (which
/// reports `Sourced` weakly when `path` exists), so this footgun is masked in
/// the happy path. A future caller that bypasses that probe will need to
/// rmdir `to` first or this helper should grow a "remove destination first"
/// option.
pub async fn copy_dir<F: AsRef<Path>, T: AsRef<Path>>(from: F, to: T) -> Result<(), FsError> {
    let from_path = from.as_ref();
    let to_path = to.as_ref();
    let from_buf = from_path.to_path_buf();
    let to_buf = to_path.to_path_buf();

    let mut child = Command::new("cp")
        .arg("--recursive")
        .arg(from_path)
        .arg(to_path)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|source| FsError::CopyDirSpawn {
            from: from_buf.clone(),
            to: to_buf.clone(),
            source,
        })?;

    let status = child.wait().await.map_err(|source| FsError::CopyDirWait {
        from: from_buf.clone(),
        to: to_buf.clone(),
        source,
    })?;

    if status.success() {
        Ok(())
    } else {
        Err(FsError::CopyDirStatus {
            from: from_buf,
            to: to_buf,
        })
    }
}

pub async fn read_dir<P: AsRef<Path>>(path: P) -> Result<Vec<PathBuf>, FsError> {
    let p = path.as_ref();
    let mut dir = fs::read_dir(p).await.map_err(|source| FsError::ReadDir {
        path: p.to_path_buf(),
        source,
    })?;
    let mut entries = Vec::new();
    while let Some(entry) = dir
        .next_entry()
        .await
        .map_err(|source| FsError::ReadDirEntry {
            path: p.to_path_buf(),
            source,
        })?
    {
        entries.push(entry.path());
    }
    Ok(entries)
}

pub async fn remove_dir<P: AsRef<Path>>(path: P) -> Result<(), FsError> {
    let p = path.as_ref();
    fs::remove_dir_all(p)
        .await
        .map_err(|source| FsError::RemoveDir {
            path: p.to_path_buf(),
            source,
        })
}

pub async fn setup_directory_access<P: AsRef<Path>>(path: P) -> Result<(), FsError> {
    let p = path.as_ref();
    create_dir(p).await?;
    let permission = fs::metadata(p)
        .await
        .map_err(|source| FsError::Metadata {
            path: p.to_path_buf(),
            source,
        })?
        .permissions();
    if permission.readonly() {
        return Err(FsError::ReadOnlyDir {
            path: p.to_path_buf(),
        });
    }
    Ok(())
}

pub async fn get_mode<P: AsRef<Path>>(path: P) -> Result<u32, FsError> {
    let p = path.as_ref();
    let metadata = fs::metadata(p).await.map_err(|source| FsError::Metadata {
        path: p.to_path_buf(),
        source,
    })?;
    Ok(metadata.permissions().mode())
}

pub async fn change_mode<P: AsRef<Path>>(path: P, mode: u32) -> Result<(), FsError> {
    let p = path.as_ref();
    let mut permissions = fs::metadata(p)
        .await
        .map_err(|source| FsError::Metadata {
            path: p.to_path_buf(),
            source,
        })?
        .permissions();
    permissions.set_mode(mode);
    fs::set_permissions(p, permissions)
        .await
        .map_err(|source| FsError::ChangeMode {
            path: p.to_path_buf(),
            mode,
            source,
        })?;
    Ok(())
}

#[cfg(unix)]
pub async fn change_owner_by_id<P: AsRef<Path>>(
    path: P,
    uid: Option<u32>,
    gid: Option<u32>,
) -> Result<(), FsError> {
    let p = path.as_ref();

    let std_file = &std::fs::File::open(p).map_err(|source| FsError::OpenFile {
        path: p.to_path_buf(),
        source,
    })?;

    std::os::unix::fs::fchown(std_file, uid, gid).map_err(|source| FsError::ChangeOwner {
        path: p.to_path_buf(),
        uid,
        gid,
        source,
    })
}

#[cfg(unix)]
pub async fn change_owner<P: AsRef<Path>>(
    path: P,
    user: Option<&str>,
    group: Option<&str>,
) -> Result<(), FsError> {
    let uid = match user {
        Some(user) => Some(
            User::from_name(user)
                .map_err(|source| FsError::UserFromName {
                    user: user.into(),
                    source,
                })?
                .ok_or_else(|| FsError::UserNotFound { user: user.into() })?
                .uid
                .as_raw(),
        ),
        None => None,
    };

    let gid = match group {
        Some(group) => Some(
            Group::from_name(group)
                .map_err(|source| FsError::GroupFromName {
                    group: group.into(),
                    source,
                })?
                .ok_or_else(|| FsError::GroupNotFound {
                    group: group.into(),
                })?
                .gid
                .as_raw(),
        ),
        None => None,
    };

    change_owner_by_id(path, uid, gid).await
}

pub async fn get_owner_user<P: AsRef<Path>>(path: P) -> Result<Option<User>, FsError> {
    let p = path.as_ref();
    let metadata = fs::metadata(p).await.map_err(|source| FsError::Metadata {
        path: p.to_path_buf(),
        source,
    })?;
    let uid = metadata.uid();
    User::from_uid(uid.into()).map_err(|source| FsError::UserFromUid { uid, source })
}

pub async fn get_owner_group<P: AsRef<Path>>(path: P) -> Result<Option<Group>, FsError> {
    let p = path.as_ref();
    let metadata = fs::metadata(p).await.map_err(|source| FsError::Metadata {
        path: p.to_path_buf(),
        source,
    })?;
    let gid = metadata.gid();
    Group::from_gid(gid.into()).map_err(|source| FsError::GroupFromGid { gid, source })
}

pub async fn create_file<P: AsRef<Path>>(path: P) -> Result<tokio::fs::File, FsError> {
    let p = path.as_ref();
    fs::OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(p)
        .await
        .map_err(|source| FsError::CreateFile {
            path: p.to_path_buf(),
            source,
        })
}

pub async fn open_file<P: AsRef<Path>>(path: P) -> Result<tokio::fs::File, FsError> {
    let p = path.as_ref();
    fs::File::open(p).await.map_err(|source| FsError::OpenFile {
        path: p.to_path_buf(),
        source,
    })
}

pub async fn path_exists<P: AsRef<Path>>(path: P) -> Result<bool, FsError> {
    let p = path.as_ref();
    fs::try_exists(p)
        .await
        .map_err(|source| FsError::PathExists {
            path: p.to_path_buf(),
            source,
        })
}

pub async fn write_file<P: AsRef<Path>>(path: P, data: &[u8]) -> Result<(), FsError> {
    let p = path.as_ref();
    let mut file = create_file(p).await?;
    file.write_all(data)
        .await
        .map_err(|source| FsError::WriteFile {
            path: p.to_path_buf(),
            source,
        })?;
    file.flush().await.map_err(|source| FsError::WriteFile {
        path: p.to_path_buf(),
        source,
    })?;
    Ok(())
}

/// Atomically write `data` to `path`.
///
/// Strategy: write to a sibling temp file, copy existing destination metadata onto the
/// temp file (permissions, owner, times — so users don't see a file whose mode changed
/// when it shouldn't have), then rename. Readers never observe a partial write.
pub async fn write_file_atomic<P: AsRef<Path>>(path: P, data: &[u8]) -> Result<(), FsError> {
    write_file_atomic_with_initial_mode(path, data, None).await
}

/// Atomically write `data` to `path`, applying `initial_mode` to the
/// temp file before any bytes are written.
///
/// `initial_mode = Some(0o600)` is the secrets case: the file should
/// never be world- or group-readable, even briefly. Without this, the
/// temp file picks up the umask (typically `0o644`), and there's a
/// window between rename and the eventual `chmod 0600` op where any
/// local UID could `read(2)` the bytes.
///
/// `initial_mode = None` matches `write_file_atomic`'s historical
/// behaviour: temp file gets umask perms; if the destination already
/// exists, [`copy_metadata`] later copies its mode/owner onto the
/// temp file before the rename.
pub async fn write_file_atomic_with_initial_mode<P: AsRef<Path>>(
    path: P,
    data: &[u8],
    initial_mode: Option<u32>,
) -> Result<(), FsError> {
    let dest_path = path.as_ref();
    let temp_path = temporary_path_for(dest_path);

    // Open the temp file before writing bytes so we can pin the mode
    // for `initial_mode = Some(_)` callers (secrets) up-front. The
    // alternative — chmod after write — leaves a window where the bytes
    // are on disk under umask perms.
    let mut file = create_file_with_mode(&temp_path, initial_mode).await?;
    file.write_all(data)
        .await
        .map_err(|source| FsError::WriteFile {
            path: temp_path.clone(),
            source,
        })?;
    file.flush().await.map_err(|source| FsError::WriteFile {
        path: temp_path.clone(),
        source,
    })?;
    drop(file);

    if path_exists(dest_path).await? {
        // Copy metadata from destination path.
        copy_metadata(dest_path, &temp_path).await?;
    }

    // Rename temporary path to destination path.
    rename_file(&temp_path, dest_path).await?;

    Ok(())
}

/// Like [`create_file`] but optionally pins the mode at create time
/// (via `OpenOptions::mode`). When `mode` is `None`, identical to
/// [`create_file`].
async fn create_file_with_mode<P: AsRef<Path>>(
    path: P,
    mode: Option<u32>,
) -> Result<tokio::fs::File, FsError> {
    let p = path.as_ref();
    let mut opts = fs::OpenOptions::new();
    opts.write(true).create(true).truncate(true);
    if let Some(m) = mode {
        // Unix-only. The full lusid project assumes Unix targets;
        // `lusid_system::Os` enumerates Linux variants only today.
        opts.mode(m);
    }
    opts.open(p).await.map_err(|source| FsError::CreateFile {
        path: p.to_path_buf(),
        source,
    })
}

pub async fn copy_metadata<Src: AsRef<Path>, Dest: AsRef<Path>>(
    src: Src,
    dest: Dest,
) -> Result<(), FsError> {
    let src = src.as_ref();
    let dest = dest.as_ref();

    let src_metadata = fs::metadata(src)
        .await
        .map_err(|source| FsError::Metadata {
            path: src.to_path_buf(),
            source,
        })?;

    // Copy permissions.
    fs::set_permissions(dest, src_metadata.permissions())
        .await
        .map_err(|source| FsError::SetPermissions {
            path: dest.to_path_buf(),
            source,
        })?;

    // Copy ownership
    change_owner_by_id(dest, Some(src_metadata.uid()), Some(src_metadata.gid())).await?;

    // Copy file times
    let atime = FileTime::from_last_access_time(&src_metadata);
    let mtime = FileTime::from_last_modification_time(&src_metadata);
    let std_file = &std::fs::File::open(dest).map_err(|source| FsError::OpenFile {
        path: dest.to_path_buf(),
        source,
    })?;
    filetime::set_file_handle_times(std_file, Some(atime), Some(mtime))
        .map_err(|source| FsError::SetFileTimes { source })?;

    Ok(())
}

pub async fn read_file_to_string<P: AsRef<Path>>(path: P) -> Result<String, FsError> {
    let p = path.as_ref();
    fs::read_to_string(p)
        .await
        .map_err(|source| FsError::ReadFile {
            path: p.to_path_buf(),
            source,
        })
}

pub async fn read_file_to_bytes<P: AsRef<Path>>(path: P) -> Result<Vec<u8>, FsError> {
    let p = path.as_ref();
    fs::read(p).await.map_err(|source| FsError::ReadFile {
        path: p.to_path_buf(),
        source,
    })
}

/// Atomically copy `from` → `to`, preserving the source's permissions/owner/times.
///
/// Unlike [`write_file_atomic`], this copies metadata from the *source* (since the
/// whole point is replicating it). The sibling-temp-then-rename dance is identical.
pub async fn copy_file_atomic<F: AsRef<Path>, T: AsRef<Path>>(
    from: F,
    to: T,
) -> Result<(), FsError> {
    let from_path = from.as_ref();
    let to_path = to.as_ref();

    let temp_path = temporary_path_for(to_path);

    {
        let mut source_file =
            fs::File::open(from_path)
                .await
                .map_err(|source| FsError::OpenFile {
                    path: from_path.to_path_buf(),
                    source,
                })?;
        let mut destination_file = fs::OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(&temp_path)
            .await
            .map_err(|source| FsError::OpenFile {
                path: temp_path.to_path_buf(),
                source,
            })?;

        tokio::io::copy(&mut source_file, &mut destination_file)
            .await
            .map_err(|source| FsError::CopyFile {
                from: from_path.to_path_buf(),
                to: temp_path.to_path_buf(),
                source,
            })?;
        destination_file
            .flush()
            .await
            .map_err(|source| FsError::CopyFile {
                from: from_path.to_path_buf(),
                to: temp_path.to_path_buf(),
                source,
            })?;

        // Copy metadata from destination path.
        copy_metadata(from_path, &temp_path).await?;
    }

    rename_file(&temp_path, to_path).await?;

    Ok(())
}

pub async fn rename_file<F: AsRef<Path>, T: AsRef<Path>>(from: F, to: T) -> Result<(), FsError> {
    let from_p = from.as_ref();
    let to_p = to.as_ref();
    fs::rename(from_p, to_p)
        .await
        .map_err(|source| FsError::RenameFile {
            from: from_p.to_path_buf(),
            to: to_p.to_path_buf(),
            source,
        })
}

pub async fn remove_file<P: AsRef<Path>>(path: P) -> Result<(), FsError> {
    let p = path.as_ref();
    fs::remove_file(p)
        .await
        .map_err(|source| FsError::RemoveFile {
            path: p.to_path_buf(),
            source,
        })
}

/// Atomically create (or replace) a symlink at `to` pointing to `from`.
///
/// Strategy: create the symlink at a sibling temp path, then `rename(2)` it
/// over the destination. POSIX `rename` is path-level and atomic across
/// regular files and symlinks, so any concurrent reader of `to` sees either
/// the old entry or the new one — never an absent or half-created link.
///
/// Replaces an existing regular file or symlink at `to`. Fails (with
/// `ENOTEMPTY`/`EISDIR`) if `to` is a non-empty directory — convert via an
/// explicit `state: "absent"` first.
///
/// Note(cc): `from` is not stat-checked here. `lusid-resource`'s
/// `validate_host_paths` checks the source up front, but the source can
/// disappear in the gap between validation and apply, in which case this
/// silently produces a dangling symlink. If dangling-link confusion ever
/// surfaces in practice, add an `lstat(from)` here and surface a dedicated
/// `FsError::SymlinkSourceMissing`.
pub async fn create_symlink_atomic<F: AsRef<Path>, T: AsRef<Path>>(
    from: F,
    to: T,
) -> Result<(), FsError> {
    let from_path = from.as_ref();
    let to_path = to.as_ref();

    let temp_path = temporary_path_for(to_path);

    fs::symlink(from_path, &temp_path)
        .await
        .map_err(|source| FsError::CreateSymlink {
            from: from_path.to_path_buf(),
            to: temp_path.clone(),
            source,
        })?;

    rename_file(&temp_path, to_path).await?;

    Ok(())
}

/// Atomically probe `path`: returns the symlink target if `path` is a
/// symlink, [`SymlinkTarget::NotASymlink`] if it exists but is something else
/// (regular file, directory, …), or [`SymlinkTarget::Missing`] if it doesn't
/// exist at all.
///
/// Combines `lstat` + `readlink` so a TOCTOU swap between the two only
/// widens the window for a stale answer — never produces a confusing
/// "Cannot read symlink" error for something that lstat'd as a symlink and
/// then got replaced before the readlink. We translate `EINVAL` from
/// `readlink` (path is no longer a symlink) into `NotASymlink` for that
/// reason.
pub async fn probe_symlink<P: AsRef<Path>>(path: P) -> Result<SymlinkTarget, FsError> {
    let p = path.as_ref();
    let metadata = match fs::symlink_metadata(p).await {
        Ok(m) => m,
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
            return Ok(SymlinkTarget::Missing);
        }
        Err(source) => {
            return Err(FsError::Metadata {
                path: p.to_path_buf(),
                source,
            });
        }
    };
    if !metadata.file_type().is_symlink() {
        return Ok(SymlinkTarget::NotASymlink);
    }
    match fs::read_link(p).await {
        Ok(target) => Ok(SymlinkTarget::Symlink(target)),
        Err(err) if err.raw_os_error() == Some(nix::libc::EINVAL) => Ok(SymlinkTarget::NotASymlink),
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(SymlinkTarget::Missing),
        Err(source) => Err(FsError::ReadSymlink {
            path: p.to_path_buf(),
            source,
        }),
    }
}

/// Outcome of [`probe_symlink`]. Lexical — `Symlink(target)` is whatever
/// `readlink(2)` returned, not a canonicalised path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymlinkTarget {
    Symlink(PathBuf),
    NotASymlink,
    Missing,
}

// Sibling temp path: `<path>.<nanos>.tmp`. Nanosecond resolution is effectively
// collision-free for sequential writes to the same target; parallel writers to the
// same target would be racing anyway and the final `rename` is atomic.
fn temporary_path_for(path: &Path) -> PathBuf {
    let time = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();

    path.with_extension(format!("{time}.tmp"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[tokio::test]
    async fn create_symlink_atomic_creates_when_destination_is_missing() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("source.txt");
        write_file(&source, b"hello").await.unwrap();
        let link = dir.path().join("link.txt");

        create_symlink_atomic(&source, &link).await.unwrap();

        assert_eq!(
            probe_symlink(&link).await.unwrap(),
            SymlinkTarget::Symlink(source)
        );
    }

    #[tokio::test]
    async fn create_symlink_atomic_replaces_an_existing_regular_file() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("source.txt");
        write_file(&source, b"new bytes").await.unwrap();
        let link = dir.path().join("link.txt");
        write_file(&link, b"stale bytes").await.unwrap();

        create_symlink_atomic(&source, &link).await.unwrap();

        assert_eq!(
            probe_symlink(&link).await.unwrap(),
            SymlinkTarget::Symlink(source)
        );
    }

    #[tokio::test]
    async fn create_symlink_atomic_replaces_a_stale_symlink() {
        let dir = tempdir().unwrap();
        let old_target = dir.path().join("old.txt");
        let new_target = dir.path().join("new.txt");
        write_file(&old_target, b"old").await.unwrap();
        write_file(&new_target, b"new").await.unwrap();
        let link = dir.path().join("link.txt");

        create_symlink_atomic(&old_target, &link).await.unwrap();
        assert_eq!(
            probe_symlink(&link).await.unwrap(),
            SymlinkTarget::Symlink(old_target)
        );

        create_symlink_atomic(&new_target, &link).await.unwrap();
        assert_eq!(
            probe_symlink(&link).await.unwrap(),
            SymlinkTarget::Symlink(new_target)
        );
    }

    #[tokio::test]
    async fn create_symlink_atomic_fails_when_destination_is_a_non_empty_dir() {
        // Non-empty directory at `to` makes `rename(2)` fail with
        // ENOTEMPTY/EISDIR. We surface that as `RenameFile` so the user sees
        // the offending paths and can recover via `state: "absent"`.
        let dir = tempdir().unwrap();
        let source = dir.path().join("source.txt");
        write_file(&source, b"x").await.unwrap();
        let link = dir.path().join("link");
        create_dir(&link).await.unwrap();
        write_file(&link.join("inner.txt"), b"keep me")
            .await
            .unwrap();

        let err = create_symlink_atomic(&source, &link).await.unwrap_err();
        assert!(matches!(err, FsError::RenameFile { .. }));
    }

    #[tokio::test]
    async fn create_symlink_atomic_fails_when_destination_is_an_empty_dir() {
        // Linux's `rename(2)` rejects renaming a non-directory over a
        // directory (even an empty one) with EISDIR. Pinning this so a
        // future "auto-rmdir if empty" change is intentional.
        let dir = tempdir().unwrap();
        let source = dir.path().join("source.txt");
        write_file(&source, b"x").await.unwrap();
        let link = dir.path().join("empty-dir");
        create_dir(&link).await.unwrap();

        let err = create_symlink_atomic(&source, &link).await.unwrap_err();
        assert!(matches!(err, FsError::RenameFile { .. }));
    }

    #[tokio::test]
    async fn probe_symlink_classifies_files_links_and_missing() {
        let dir = tempdir().unwrap();
        let regular = dir.path().join("regular.txt");
        write_file(&regular, b"hi").await.unwrap();
        let link = dir.path().join("link.txt");
        create_symlink_atomic(&regular, &link).await.unwrap();
        let missing = dir.path().join("missing.txt");

        assert_eq!(
            probe_symlink(&regular).await.unwrap(),
            SymlinkTarget::NotASymlink
        );
        assert_eq!(
            probe_symlink(&link).await.unwrap(),
            SymlinkTarget::Symlink(regular)
        );
        assert_eq!(
            probe_symlink(&missing).await.unwrap(),
            SymlinkTarget::Missing
        );
    }

    #[tokio::test]
    async fn probe_symlink_handles_dangling_symlink() {
        // `lstat` returns the symlink itself, regardless of whether the
        // target exists. The probe should report `Symlink(target)`, not
        // `Missing`, so callers can decide what dangling means for them.
        let dir = tempdir().unwrap();
        let target = dir.path().join("does-not-exist.txt");
        let link = dir.path().join("dangling.txt");
        create_symlink_atomic(&target, &link).await.unwrap();

        assert_eq!(
            probe_symlink(&link).await.unwrap(),
            SymlinkTarget::Symlink(target)
        );
    }
}
