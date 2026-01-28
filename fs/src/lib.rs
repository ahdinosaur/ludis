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

pub async fn write_file_atomic<P: AsRef<Path>>(path: P, data: &[u8]) -> Result<(), FsError> {
    let dest_path = path.as_ref();
    let temp_path = temporary_path_for(dest_path);

    // Write file contents to temporary path in same directory as destination path.
    write_file(&temp_path, data).await?;

    if path_exists(dest_path).await? {
        // Copy metadata from destination path.
        copy_metadata(dest_path, &temp_path).await?;
    }

    // Rename temporary path to destination path.
    rename_file(&temp_path, dest_path).await?;

    Ok(())
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

pub async fn create_symlink<F: AsRef<Path>, T: AsRef<Path>>(from: F, to: T) -> Result<(), FsError> {
    let from_path = from.as_ref();
    let to_path = to.as_ref();

    fs::symlink(from_path, to_path)
        .await
        .map_err(|source| FsError::CreateSymlink {
            from: from_path.to_path_buf(),
            to: to_path.to_path_buf(),
            source,
        })
}

fn temporary_path_for(path: &Path) -> PathBuf {
    let time = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();

    path.with_extension(format!("{time}.tmp"))
}
