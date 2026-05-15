//! `lusid secrets check` - read-only audit of `<secrets_dir>` against
//! `lusid-secrets.toml`. CI-friendly: exits non-zero on any finding.
//!
//! Findings collected:
//!
//! - **orphan** - a `*.age` file exists with no matching `[files]` entry.
//!   These won't decrypt via the normal apply path (plans only see files
//!   listed in `lusid-secrets.toml`) so they're either stale or mis-named.
//! - **missing** - a `[files]` entry has no `*.age` file on disk.
//! - **resolve** - a `[files]` entry references an unknown key alias or
//!   group (surfaces the same errors `rekey` would hit).
//! - **drift** - the ciphertext header's recipients don't match the
//!   current `lusid-secrets.toml` (needs a `rekey`).
//!
//! ## Drift precision
//!
//! SSH recipients are identified by a 4-byte tag in the stanza header, so
//! swaps (e.g. old ed25519 key → new ed25519 key) are detected. **X25519
//! stanzas are anonymous** - the header only contains an ephemeral pubkey,
//! not anything derived from the recipient. We therefore only compare
//! x25519 stanza *counts*. Add/remove of operators is caught; a swap between
//! two x25519 keys (same count) is not.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use displaydoc::Display;
use thiserror::Error;
use tokio::fs;

use crate::crypto::{self, HeaderError};
use crate::key::Key;
use crate::recipients::{Recipients, ResolveError, ResolvedRecipient};

#[derive(Debug, Default)]
pub(crate) struct CheckReport {
    pub orphans: Vec<PathBuf>,
    pub missing: Vec<String>,
    pub resolve_errors: Vec<ResolveError>,
    pub drifted: Vec<DriftedFile>,
    pub read_errors: Vec<ReadError>,
}

#[derive(Debug)]
pub(crate) struct DriftedFile {
    pub stem: String,
    pub reason: DriftReason,
}

#[derive(Debug)]
pub(crate) enum DriftReason {
    /// Header stanza counts don't match: expected `{expected_x25519}`
    /// x25519 + `{expected_ssh}` SSH, found `{actual_x25519}` x25519 +
    /// `{actual_ssh}` SSH.
    CountMismatch {
        expected_x25519: usize,
        expected_ssh: usize,
        actual_x25519: usize,
        actual_ssh: usize,
    },

    /// Expected SSH tag `{tag}` (alias `{alias}`) is missing from the
    /// ciphertext header.
    MissingSshTag { alias: String, tag: String },

    /// Ciphertext header contains an SSH stanza with tag `{tag}` that
    /// `lusid-secrets.toml` doesn't list.
    UnexpectedSshTag { tag: String },

    /// Failed to read the age header.
    UnreadableHeader(HeaderError),
}

#[derive(Debug)]
pub(crate) struct ReadError {
    pub path: PathBuf,
    pub source: std::io::Error,
}

impl CheckReport {
    pub fn is_clean(&self) -> bool {
        self.orphans.is_empty()
            && self.missing.is_empty()
            && self.resolve_errors.is_empty()
            && self.drifted.is_empty()
            && self.read_errors.is_empty()
    }
}

#[derive(Debug, Error, Display)]
pub enum CheckError {
    /// Failed to scan {dir}: {source}
    ScanDir {
        dir: PathBuf,
        #[source]
        source: std::io::Error,
    },
}

/// Walk `secrets_dir` and cross-check against `recipients`. Does not
/// decrypt - everything here is either a file existence / stanza count
/// check or a header stanza read.
pub(crate) async fn check(
    secrets_dir: &Path,
    recipients: &Recipients,
) -> Result<CheckReport, CheckError> {
    let mut report = CheckReport::default();

    let mut on_disk: BTreeMap<String, PathBuf> = BTreeMap::new();
    if fs::try_exists(secrets_dir).await.unwrap_or(false) {
        let mut rd = fs::read_dir(secrets_dir)
            .await
            .map_err(|source| CheckError::ScanDir {
                dir: secrets_dir.to_path_buf(),
                source,
            })?;
        while let Some(entry) = rd
            .next_entry()
            .await
            .map_err(|source| CheckError::ScanDir {
                dir: secrets_dir.to_path_buf(),
                source,
            })?
        {
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) != Some("age") {
                continue;
            }
            if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                on_disk.insert(stem.to_owned(), path);
            }
        }
    }

    // Orphans: .age files with no lusid-secrets.toml entry.
    for (stem, path) in &on_disk {
        if !recipients.files.contains_key(stem) {
            report.orphans.push(path.clone());
        }
    }

    // Missing + resolve + drift: walk lusid-secrets.toml's [files].
    for stem in recipients.file_stems() {
        let Some(path) = on_disk.get(stem) else {
            report.missing.push(stem.to_owned());
            continue;
        };

        let resolved = match recipients.resolve(stem) {
            Ok(r) => r,
            Err(err) => {
                report.resolve_errors.push(err);
                continue;
            }
        };

        let bytes = match fs::read(path).await {
            Ok(b) => b,
            Err(source) => {
                report.read_errors.push(ReadError {
                    path: path.clone(),
                    source,
                });
                continue;
            }
        };

        let stanzas = match crypto::read_header_stanzas(&bytes) {
            Ok(s) => s,
            Err(err) => {
                report.drifted.push(DriftedFile {
                    stem: stem.to_owned(),
                    reason: DriftReason::UnreadableHeader(err),
                });
                continue;
            }
        };

        if let Some(reason) = compare_stanzas(&resolved, &stanzas) {
            report.drifted.push(DriftedFile {
                stem: stem.to_owned(),
                reason,
            });
        }
    }

    Ok(report)
}

/// Compare the intended recipients against an age ciphertext's header.
///
/// Precise for SSH recipients (stanza `args[0]` is a deterministic 4-byte
/// tag derived from the pubkey), count-only for X25519 (stanzas are
/// anonymous - the header only carries an ephemeral pubkey). Returns `None`
/// when the header matches the resolved recipients; `Some(reason)` describes
/// the first mismatch found.
pub(crate) fn compare_stanzas(
    resolved: &[ResolvedRecipient],
    stanzas: &[age_core::format::Stanza],
) -> Option<DriftReason> {
    let expected_x25519 = resolved
        .iter()
        .filter(|r| matches!(r.key, Key::X25519(_)))
        .count();
    let expected_ssh_tags: Vec<(String, String)> = resolved
        .iter()
        .filter_map(|r| ssh_stanza_tag(&r.key).map(|tag| (r.alias.clone(), tag)))
        .collect();

    // Stanzas from the header: filter to the recipient types we know about;
    // age inserts a random "grease" stanza for forward-compat, which we
    // ignore. (Its tag is `<chars>-grease`.)
    let is_grease = |s: &age_core::format::Stanza| s.tag.ends_with("-grease");

    let actual_x25519 = stanzas
        .iter()
        .filter(|s| s.tag == "X25519" && !is_grease(s))
        .count();
    let actual_ssh_tags: Vec<String> = stanzas
        .iter()
        .filter(|s| matches!(s.tag.as_str(), "ssh-ed25519" | "ssh-rsa"))
        .filter_map(|s| s.args.first().cloned())
        .collect();

    if expected_x25519 != actual_x25519 || expected_ssh_tags.len() != actual_ssh_tags.len() {
        return Some(DriftReason::CountMismatch {
            expected_x25519,
            expected_ssh: expected_ssh_tags.len(),
            actual_x25519,
            actual_ssh: actual_ssh_tags.len(),
        });
    }

    // Every expected SSH tag must appear in the ciphertext.
    for (alias, tag) in &expected_ssh_tags {
        if !actual_ssh_tags.iter().any(|a| a == tag) {
            return Some(DriftReason::MissingSshTag {
                alias: alias.clone(),
                tag: tag.clone(),
            });
        }
    }
    // Every actual SSH tag must match an expected one.
    for tag in &actual_ssh_tags {
        if !expected_ssh_tags.iter().any(|(_, t)| t == tag) {
            return Some(DriftReason::UnexpectedSshTag { tag: tag.clone() });
        }
    }

    None
}

/// Return the deterministic 4-byte tag age uses to identify an SSH
/// recipient in a ciphertext header. `None` for X25519 keys - those
/// stanzas are anonymous (only an ephemeral pubkey, nothing recipient-
/// derived).
///
/// The tag is the first arg of an `ssh-ed25519` / `ssh-rsa` stanza -
/// derived from the SHA-256 of the SSH wire-format pubkey. We round-trip
/// through a one-byte encrypt to extract it without depending on `age`'s
/// internals.
fn ssh_stanza_tag(key: &Key) -> Option<String> {
    let ssh = match key {
        Key::Ssh(k) => k,
        Key::X25519(_) => return None,
    };
    let recipient: Box<dyn age::Recipient + Send> = Box::new(ssh.clone());
    let ct = crypto::encrypt_bytes(
        std::slice::from_ref(&recipient),
        Path::new("__tag_probe__"),
        b"",
    )
    .ok()?;
    let stanzas = crypto::read_header_stanzas(&ct).ok()?;
    stanzas
        .into_iter()
        .find(|s| matches!(s.tag.as_str(), "ssh-ed25519" | "ssh-rsa"))
        .and_then(|s| s.args.into_iter().next())
}

impl std::fmt::Display for DriftReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DriftReason::CountMismatch {
                expected_x25519,
                expected_ssh,
                actual_x25519,
                actual_ssh,
            } => write!(
                f,
                "expected {expected_x25519} x25519 + {expected_ssh} SSH, found {actual_x25519} x25519 + {actual_ssh} SSH",
            ),
            DriftReason::MissingSshTag { alias, tag } => {
                write!(f, "missing SSH tag {tag} (alias {alias})")
            }
            DriftReason::UnexpectedSshTag { tag } => {
                write!(
                    f,
                    "unexpected SSH tag {tag} not listed in lusid-secrets.toml"
                )
            }
            DriftReason::UnreadableHeader(err) => write!(f, "unreadable header: {err}"),
        }
    }
}
