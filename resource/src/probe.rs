//! Sudo-aware probe helpers for resource state observation.
//!
//! Every probe takes a `sudo: bool`. When `false`, delegates straight to
//! the equivalent [`lusid_fs`] helper (the same call the file/directory
//! resources used before sudo-aware probes existed). When `true`, shells
//! out under `sudo -n` so the probe succeeds against paths whose parent
//! directories the calling user cannot traverse - the canonical case is
//! `/etc/sudoers.d/`, which on Arch Linux is mode `0750 root:root` and so
//! cannot even be `stat(2)`d by a non-`wheel` operator running
//! `./snug apply`.
//!
//! These helpers cover exactly the primitives the file and directory
//! resources need: existence check, byte read, mode and ownership lookup,
//! and the symlink probe. Each probe is a thin wrapper that picks the
//! right shell-out (`test`, `cat`, `stat`, `readlink`) - the cost is one
//! `sudo` invocation per probe, which is acceptable given probes only
//! run once per epoch and never on a hot path.
//!
//! Sudo state probes assume the operator can run those tools
//! passwordlessly under `sudo -n`. The existing `sudo: true` apply path
//! makes the same assumption for `install`/`chmod`/`chown`, so this
//! does not raise the bar for operators who already use `sudo: true`.

use std::path::{Path, PathBuf};

use lusid_cmd::{Command, CommandError};
use lusid_fs::{self as fs, FsError, SymlinkTarget};
use nix::unistd::{Group, User};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ProbeError {
    #[error(transparent)]
    Fs(#[from] FsError),

    #[error(transparent)]
    Command(#[from] CommandError),

    /// Stdout from a sudo'd probe didn't parse as expected. The path and
    /// detail are kept so the operator can investigate; this should never
    /// fire against a sane `stat`/`readlink`/our `sh` probe and indicates
    /// a real environmental bug if it ever does.
    #[error("unexpected output from sudo probe for '{path}': {detail}")]
    UnexpectedOutput { path: PathBuf, detail: String },

    /// `stat` returned a uid that `getpwuid` couldn't translate to a
    /// `User` entry. Effectively never happens, pinned here so probe
    /// callers don't have to deal with `nix::Error` separately from
    /// the other variants.
    #[error("Failed to resolve uid {uid}: {source}")]
    ResolveUid {
        uid: u32,
        #[source]
        source: nix::Error,
    },

    /// Mirror of [`Self::ResolveUid`] for gid → `Group`.
    #[error("Failed to resolve gid {gid}: {source}")]
    ResolveGid {
        gid: u32,
        #[source]
        source: nix::Error,
    },
}

/// Does `path` exist?
///
/// Sudo path uses `sudo -n sh -c 'if test -e "$0"; then printf Y; else
/// printf N; fi' <path>`. The explicit `Y`/`N` verdict on stdout lets
/// us distinguish "file does not exist" from "sudo itself failed":
/// `test -e` exits 1 in both cases, but a sudo-policy failure aborts
/// the shell before either marker is emitted, surfacing as
/// [`CommandError::Failure`] with the `sudo:` stderr attached (or
/// [`CommandError::Spawn`] if `sudo`/`sh` themselves are missing).
pub async fn path_exists(path: &Path, sudo: bool) -> Result<bool, ProbeError> {
    if !sudo {
        return Ok(fs::path_exists(path).await?);
    }
    let mut cmd = Command::new("sh");
    cmd.arg("-c")
        .arg("if test -e \"$0\"; then printf Y; else printf N; fi")
        .arg(path);
    let out = cmd.sudo().run().await?;
    parse_yes_no(path, &out)
}

/// Read the file at `path` into a `Vec<u8>`.
///
/// Sudo path uses `sudo -n cat -- <path>`. Stdout is captured verbatim,
/// so the read is binary-safe and matches [`fs::read_file_to_bytes`].
pub async fn read_file_to_bytes(path: &Path, sudo: bool) -> Result<Vec<u8>, ProbeError> {
    if !sudo {
        return Ok(fs::read_file_to_bytes(path).await?);
    }
    let mut cmd = Command::new("cat");
    cmd.arg("--").arg(path);
    Ok(cmd.sudo().run().await?)
}

/// Stat `path` and return its raw `st_mode`.
///
/// Sudo path uses `sudo -n stat -L -c %f -- <path>`. `%f` is the raw
/// mode (file-type bits + perms) in hex; `-L` follows symlinks to match
/// [`fs::get_mode`] (which uses `stat(2)`, not `lstat(2)`). Callers
/// typically mask with `& 0o7777` to compare against a declared mode.
pub async fn get_mode(path: &Path, sudo: bool) -> Result<u32, ProbeError> {
    if !sudo {
        return Ok(fs::get_mode(path).await?);
    }
    let mut cmd = Command::new("stat");
    cmd.arg("-L").arg("-c").arg("%f").arg("--").arg(path);
    let out = cmd.sudo().run().await?;
    parse_hex_mode(path, &out)
}

/// Look up the owning user of `path`, following symlinks.
///
/// Sudo path uses `sudo -n stat -L -c %u -- <path>` to get the numeric
/// uid, then resolves it with [`User::from_uid`] - matching
/// [`fs::get_owner_user`]'s shape. Returns `Ok(None)` when the uid has
/// no matching entry in the passwd database (orphaned ownership).
pub async fn get_owner_user(path: &Path, sudo: bool) -> Result<Option<User>, ProbeError> {
    if !sudo {
        return Ok(fs::get_owner_user(path).await?);
    }
    let mut cmd = Command::new("stat");
    cmd.arg("-L").arg("-c").arg("%u").arg("--").arg(path);
    let out = cmd.sudo().run().await?;
    let uid = parse_id(path, &out, "uid")?;
    User::from_uid(uid.into()).map_err(|source| ProbeError::ResolveUid { uid, source })
}

/// Mirror of [`get_owner_user`] for gid → `Group`.
pub async fn get_owner_group(path: &Path, sudo: bool) -> Result<Option<Group>, ProbeError> {
    if !sudo {
        return Ok(fs::get_owner_group(path).await?);
    }
    let mut cmd = Command::new("stat");
    cmd.arg("-L").arg("-c").arg("%g").arg("--").arg(path);
    let out = cmd.sudo().run().await?;
    let gid = parse_id(path, &out, "gid")?;
    Group::from_gid(gid.into()).map_err(|source| ProbeError::ResolveGid { gid, source })
}

/// Classify `path` as a symlink (with target), a non-symlink that
/// exists, or missing - returning the same `SymlinkTarget` variant set
/// that [`fs::probe_symlink`] does.
///
/// Sudo path uses one shell-out: a `sh -c` script that classifies via
/// `[ -L ]`/`[ -e ]` (lstat semantics) and `readlink` (no follow) for
/// the target. We deliberately do not canonicalise the target string;
/// see the [`fs::probe_symlink`] docstring for why drift between a
/// declared and existing-but-different symlink target needs to surface.
///
/// One behavioural difference from the user-mode path: if the symlink
/// is swapped to a regular file between the `[ -L ]` check and
/// `readlink`, the sudo path surfaces a [`CommandError::Failure`]
/// rather than the user-mode path's `EINVAL → NotASymlink`
/// re-classification. The TOCTOU window is microscopic and surfacing
/// the race as an apply-time error is acceptable - the operator can
/// re-run and the second probe sees the regular file.
pub async fn probe_symlink(path: &Path, sudo: bool) -> Result<SymlinkTarget, ProbeError> {
    if !sudo {
        return Ok(fs::probe_symlink(path).await?);
    }
    let mut stat_cmd = Command::new("sh");
    // Distinguish "missing" from "exists, not a symlink" from "is a
    // symlink with target T" in one shot. Without the explicit M/L/F
    // marker the caller can't tell `stat` failing because the path
    // doesn't exist (exit non-zero, empty stdout) from a stat success
    // with empty type (impossible, but the parser would have to guess).
    stat_cmd
        .arg("-c")
        .arg(
            "if [ ! -e \"$0\" ] && [ ! -L \"$0\" ]; then \
                printf M; \
            elif [ -L \"$0\" ]; then \
                printf L; \
                readlink -- \"$0\"; \
            else \
                printf F; \
            fi",
        )
        .arg(path);
    let out = stat_cmd.sudo().run().await?;
    parse_symlink_probe(path, &out)
}

fn parse_yes_no(path: &Path, out: &[u8]) -> Result<bool, ProbeError> {
    match out {
        b"Y" => Ok(true),
        b"N" => Ok(false),
        _ => Err(ProbeError::UnexpectedOutput {
            path: path.to_path_buf(),
            detail: format!(
                "expected Y/N from existence probe, got {:?}",
                String::from_utf8_lossy(out)
            ),
        }),
    }
}

fn parse_hex_mode(path: &Path, out: &[u8]) -> Result<u32, ProbeError> {
    let s = std::str::from_utf8(out)
        .map_err(|e| ProbeError::UnexpectedOutput {
            path: path.to_path_buf(),
            detail: format!("non-utf8 mode: {e}"),
        })?
        .trim();
    u32::from_str_radix(s, 16).map_err(|e| ProbeError::UnexpectedOutput {
        path: path.to_path_buf(),
        detail: format!("not a hex st_mode {s:?}: {e}"),
    })
}

fn parse_id(path: &Path, out: &[u8], label: &str) -> Result<u32, ProbeError> {
    let s = std::str::from_utf8(out)
        .map_err(|e| ProbeError::UnexpectedOutput {
            path: path.to_path_buf(),
            detail: format!("non-utf8 {label}: {e}"),
        })?
        .trim();
    s.parse::<u32>().map_err(|e| ProbeError::UnexpectedOutput {
        path: path.to_path_buf(),
        detail: format!("not a numeric {label} {s:?}: {e}"),
    })
}

fn parse_symlink_probe(path: &Path, out: &[u8]) -> Result<SymlinkTarget, ProbeError> {
    match out.split_first() {
        Some((b'M', [])) => Ok(SymlinkTarget::Missing),
        Some((b'F', [])) => Ok(SymlinkTarget::NotASymlink),
        Some((b'L', rest)) => {
            // `readlink` always trails its output with a newline; strip it
            // so the target round-trips byte-equal to what the planner
            // stored.
            let target = strip_trailing_newline(rest);
            let target_str =
                std::str::from_utf8(target).map_err(|e| ProbeError::UnexpectedOutput {
                    path: path.to_path_buf(),
                    detail: format!("non-utf8 symlink target: {e}"),
                })?;
            Ok(SymlinkTarget::Symlink(PathBuf::from(target_str)))
        }
        _ => Err(ProbeError::UnexpectedOutput {
            path: path.to_path_buf(),
            detail: format!(
                "expected M/F/L marker from symlink probe, got {:?}",
                String::from_utf8_lossy(out)
            ),
        }),
    }
}

fn strip_trailing_newline(bytes: &[u8]) -> &[u8] {
    if bytes.last() == Some(&b'\n') {
        &bytes[..bytes.len() - 1]
    } else {
        bytes
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    fn p() -> &'static Path {
        Path::new("/probe/target")
    }

    #[test]
    fn parse_yes_no_accepts_y_n() {
        assert!(parse_yes_no(p(), b"Y").unwrap());
        assert!(!parse_yes_no(p(), b"N").unwrap());
    }

    #[test]
    fn parse_yes_no_rejects_anything_else() {
        // Defensive: trailing newline is rejected so we'd notice if the
        // shell ever started appending one to `printf`.
        assert!(matches!(
            parse_yes_no(p(), b"Y\n"),
            Err(ProbeError::UnexpectedOutput { .. })
        ));
        assert!(matches!(
            parse_yes_no(p(), b""),
            Err(ProbeError::UnexpectedOutput { .. })
        ));
    }

    #[test]
    fn parse_hex_mode_strips_trailing_newline() {
        // `stat` always trails its output with `\n`; the parser must
        // ignore that to match `fs::get_mode`.
        let mode = parse_hex_mode(p(), b"81a4\n").unwrap();
        assert_eq!(mode & 0o7777, 0o644);
    }

    #[test]
    fn parse_hex_mode_handles_full_st_mode() {
        // Symlink raw mode: file-type bits 0xa000 + perms 0o777.
        let mode = parse_hex_mode(p(), b"a1ff").unwrap();
        assert_eq!(mode, 0xa1ff);
    }

    #[test]
    fn parse_hex_mode_rejects_non_hex() {
        assert!(matches!(
            parse_hex_mode(p(), b"gxyz"),
            Err(ProbeError::UnexpectedOutput { .. })
        ));
    }

    #[test]
    fn parse_id_strips_trailing_newline() {
        assert_eq!(parse_id(p(), b"1000\n", "uid").unwrap(), 1000);
        assert_eq!(parse_id(p(), b"0", "gid").unwrap(), 0);
    }

    #[test]
    fn parse_id_rejects_non_numeric() {
        assert!(matches!(
            parse_id(p(), b"root", "uid"),
            Err(ProbeError::UnexpectedOutput { .. })
        ));
    }

    #[test]
    fn parse_symlink_probe_missing() {
        assert!(matches!(
            parse_symlink_probe(p(), b"M").unwrap(),
            SymlinkTarget::Missing
        ));
    }

    #[test]
    fn parse_symlink_probe_not_a_symlink() {
        assert!(matches!(
            parse_symlink_probe(p(), b"F").unwrap(),
            SymlinkTarget::NotASymlink
        ));
    }

    #[test]
    fn parse_symlink_probe_symlink_strips_trailing_newline() {
        // `readlink` always trails the target with `\n`; the parser
        // strips it so the recovered target round-trips byte-equal to
        // the path the planner stored.
        let target = parse_symlink_probe(p(), b"L/etc/foo\n").unwrap();
        assert!(matches!(target, SymlinkTarget::Symlink(t) if t == *"/etc/foo"));
    }

    #[test]
    fn parse_symlink_probe_symlink_without_trailing_newline_works_too() {
        // Defence in depth: even if a future busybox readlink stops
        // appending the newline, we still hand back the right target.
        let target = parse_symlink_probe(p(), b"L/etc/foo").unwrap();
        assert!(matches!(target, SymlinkTarget::Symlink(t) if t == *"/etc/foo"));
    }

    #[test]
    fn parse_symlink_probe_rejects_unknown_marker() {
        assert!(matches!(
            parse_symlink_probe(p(), b"Xanything"),
            Err(ProbeError::UnexpectedOutput { .. })
        ));
        assert!(matches!(
            parse_symlink_probe(p(), b""),
            Err(ProbeError::UnexpectedOutput { .. })
        ));
    }

    // --- Behavioural tests for the non-sudo path (sudo=false) ----------
    //
    // These cover the most common cases - the sudo=true path is exercised
    // by the per-resource integration through `lusid-apply` and isn't
    // reproducible in a unit test without escalating to sudo.

    #[tokio::test]
    async fn path_exists_without_sudo_matches_fs() {
        let dir = tempfile::tempdir().unwrap();
        let present = dir.path().join("present");
        let missing = dir.path().join("missing");
        tokio::fs::write(&present, b"x").await.unwrap();

        assert!(path_exists(&present, false).await.unwrap());
        assert!(!path_exists(&missing, false).await.unwrap());
    }

    #[tokio::test]
    async fn read_file_to_bytes_without_sudo_matches_fs() {
        let dir = tempfile::tempdir().unwrap();
        let f = dir.path().join("f");
        tokio::fs::write(&f, b"hello").await.unwrap();
        assert_eq!(read_file_to_bytes(&f, false).await.unwrap(), b"hello");
    }

    #[tokio::test]
    async fn get_mode_without_sudo_matches_fs() {
        use std::os::unix::fs::PermissionsExt;
        let dir = tempfile::tempdir().unwrap();
        let f = dir.path().join("f");
        tokio::fs::write(&f, b"x").await.unwrap();
        tokio::fs::set_permissions(&f, std::fs::Permissions::from_mode(0o640))
            .await
            .unwrap();
        let mode = get_mode(&f, false).await.unwrap() & 0o7777;
        assert_eq!(mode, 0o640);
    }

    #[tokio::test]
    async fn probe_symlink_without_sudo_matches_fs() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("target");
        let link = dir.path().join("link");
        let missing = dir.path().join("missing");
        tokio::fs::write(&target, b"x").await.unwrap();
        tokio::fs::symlink(&target, &link).await.unwrap();

        match probe_symlink(&link, false).await.unwrap() {
            SymlinkTarget::Symlink(t) => assert_eq!(t, target),
            other => panic!("expected Symlink, got {other:?}"),
        }
        assert!(matches!(
            probe_symlink(&target, false).await.unwrap(),
            SymlinkTarget::NotASymlink
        ));
        assert!(matches!(
            probe_symlink(&missing, false).await.unwrap(),
            SymlinkTarget::Missing
        ));
    }
}
