//! Embedded `lusid-apply` binaries, keyed by target arch.
//!
//! At build time, [`build.rs`](../../build.rs) copies prebuilt `lusid-apply`
//! binaries for each supported worker arch into `OUT_DIR` and emits
//! `cargo:rustc-cfg=embedded_apply_<arch>` for the arches it found. This
//! module guards the `include_bytes!` calls behind those cfgs so a dev build
//! with no env vars set compiles without needing placeholder files.
//!
//! Two entry points:
//!
//! - [`embedded_lusid_apply`] returns the static bytes for an arch. Used by
//!   `dev apply` and `remote apply`, which ship the bytes to SFTP via
//!   [`SshVolume::FileBytes`](lusid_ssh::SshVolume::FileBytes) wrapped in
//!   `Cow::Borrowed` - no copy of the ~10–30 MB blob per apply.
//! - [`resolve_or_extract_for_arch`] writes the bytes to an XDG-style cache
//!   path and returns the path. Used by `local apply`, which spawns
//!   `lusid-apply` as a subprocess.

use std::path::PathBuf;

use displaydoc::Display;
use thiserror::Error;
use tokio::fs;
use tokio::io::AsyncWriteExt;
use tracing::{debug, instrument};

use lusid_system::Arch;

#[cfg(embedded_apply_x86_64)]
static LUSID_APPLY_X86_64: &[u8] = include_bytes!(env!("LUSID_APPLY_OUT_X86_64"));

#[cfg(embedded_apply_aarch64)]
static LUSID_APPLY_AARCH64: &[u8] = include_bytes!(env!("LUSID_APPLY_OUT_AARCH64"));

#[derive(Error, Debug, Display)]
pub enum EmbeddedError {
    /// no lusid-apply was embedded for {arch}; run `just build-lusid-apply` then rebuild lusid (see lusid/build.rs)
    NotEmbedded { arch: Arch },

    /// failed to determine cache directory
    CacheDir(#[source] std::io::Error),

    /// failed to create cache directory at {path}
    CreateCacheDir {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// failed to write extracted lusid-apply to {path}
    WriteExtracted {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// failed to rename {tmp} to {target}
    Rename {
        tmp: PathBuf,
        target: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// failed to set executable permissions on {path}
    Chmod {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },
}

/// Return the embedded `lusid-apply` bytes for `arch`, or `NotEmbedded` if
/// the current build did not include a binary for it.
pub fn embedded_lusid_apply(arch: Arch) -> Result<&'static [u8], EmbeddedError> {
    match arch {
        #[cfg(embedded_apply_x86_64)]
        Arch::X86_64 => Ok(LUSID_APPLY_X86_64),

        #[cfg(embedded_apply_aarch64)]
        Arch::Aarch64 => Ok(LUSID_APPLY_AARCH64),

        #[allow(unreachable_patterns)]
        _ => Err(EmbeddedError::NotEmbedded { arch }),
    }
}

/// Materialize the embedded `lusid-apply` for `arch` on disk and return its
/// path. Always overwrites - a ~10 MB rewrite per `local apply` is
/// negligible, and avoids subtle freshness bugs (a length-only check would
/// false-match a stale file of the same size, and a content hash adds
/// complexity for no real win).
#[instrument(level = "debug")]
pub async fn resolve_or_extract_for_arch(arch: Arch) -> Result<PathBuf, EmbeddedError> {
    let bytes = embedded_lusid_apply(arch)?;
    let target = cache_path(arch)?;

    let dir = target.parent().expect("cache_path has a parent");
    fs::create_dir_all(dir)
        .await
        .map_err(|source| EmbeddedError::CreateCacheDir {
            path: dir.to_path_buf(),
            source,
        })?;

    let tmp = dir.join(format!(
        ".{}.tmp.{}",
        target
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_default(),
        std::process::id()
    ));

    write_executable(&tmp, bytes).await?;

    fs::rename(&tmp, &target)
        .await
        .map_err(|source| EmbeddedError::Rename {
            tmp: tmp.clone(),
            target: target.clone(),
            source,
        })?;

    debug!(arch = %arch, path = %target.display(), "extracted embedded lusid-apply");
    Ok(target)
}

async fn write_executable(path: &std::path::Path, bytes: &[u8]) -> Result<(), EmbeddedError> {
    let mut file =
        fs::File::create(path)
            .await
            .map_err(|source| EmbeddedError::WriteExtracted {
                path: path.to_path_buf(),
                source,
            })?;
    file.write_all(bytes)
        .await
        .map_err(|source| EmbeddedError::WriteExtracted {
            path: path.to_path_buf(),
            source,
        })?;
    file.flush()
        .await
        .map_err(|source| EmbeddedError::WriteExtracted {
            path: path.to_path_buf(),
            source,
        })?;
    drop(file);

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let permissions = std::fs::Permissions::from_mode(0o755);
        fs::set_permissions(path, permissions)
            .await
            .map_err(|source| EmbeddedError::Chmod {
                path: path.to_path_buf(),
                source,
            })?;
    }
    Ok(())
}

/// `${XDG_CACHE_HOME:-$HOME/.cache}/lusid/lusid-apply/<crate-version>/<arch>/lusid-apply`.
///
/// Version-keyed so a `lusid` upgrade triggers re-extraction without
/// fighting any leftover cache from an older version.
fn cache_path(arch: Arch) -> Result<PathBuf, EmbeddedError> {
    // `XDG_CACHE_HOME=""` is treated as unset (matches the XDG basedir spec:
    // "If $XDG_CACHE_HOME is either not set or empty, a default … should be
    // used."), so we fall through to `$HOME/.cache` rather than rooting the
    // cache at `/lusid/...`.
    let cache_root = match std::env::var_os("XDG_CACHE_HOME").filter(|v| !v.is_empty()) {
        Some(val) => PathBuf::from(val),
        None => {
            let home = std::env::var_os("HOME")
                .filter(|h| !h.is_empty())
                .ok_or_else(|| {
                    EmbeddedError::CacheDir(std::io::Error::new(
                        std::io::ErrorKind::NotFound,
                        "neither XDG_CACHE_HOME nor HOME is set",
                    ))
                })?;
            PathBuf::from(home).join(".cache")
        }
    };
    Ok(cache_root
        .join("lusid")
        .join("lusid-apply")
        .join(env!("CARGO_PKG_VERSION"))
        .join(arch.cfg_suffix())
        .join("lusid-apply"))
}
