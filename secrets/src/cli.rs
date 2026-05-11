//! `lusid secrets ...` subcommands.
//!
//! Dispatched from the `lusid` CLI wrapper (`lusid/src/lib.rs`). Every
//! command takes a [`CliEnv`] describing the project's secrets layout
//! (resolved by the wrapper from `lusid.toml` + CLI flags) and delegates to
//! helpers in [`crate::crypto`], [`crate::identity`], [`crate::recipients`],
//! and [`crate::check`].

use std::os::unix::fs::OpenOptionsExt;
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::time::{SystemTime, UNIX_EPOCH};

use clap::Subcommand;
use displaydoc::Display;
use secrecy::ExposeSecret;
use secrecy::zeroize::Zeroizing;
use thiserror::Error;
use tokio::fs;
use tokio::process::Command;

use crate::check::{self, CheckError, CheckReport};
use crate::crypto::{self, DecryptError, EncryptError, HeaderError};
use crate::identity::{Identity, IdentityError};
use crate::recipients::{
    Recipients, RecipientsError, ResolveError, ResolvedRecipient, to_boxed_recipients,
};

/// Subcommand surface for `lusid secrets`.
#[derive(Subcommand, Debug)]
pub enum SecretsCommand {
    /// List `*.age` files and their declared recipients.
    Ls,

    /// Decrypt into a tmpfile, open in `$EDITOR`, re-encrypt on save.
    Edit {
        /// File stem (no `.age` suffix).
        name: String,
    },

    /// Re-encrypt to the current recipients. No-op when the ciphertext
    /// header already matches `lusid-secrets.toml`.
    Rekey {
        /// Single file stem; when omitted, rekey every entry in
        /// `lusid-secrets.toml`.
        name: Option<String>,
    },

    /// Generate a fresh x25519 identity and write it to disk.
    Keygen {
        /// Output path. Defaults to `$XDG_CONFIG_HOME/lusid/identity`
        /// (or `$HOME/.config/lusid/identity`). Refuses to overwrite an
        /// existing file.
        #[arg(short = 'o', long = "output")]
        output: Option<PathBuf>,
    },

    /// Audit `<secrets_dir>` against `lusid-secrets.toml`. Non-zero exit on
    /// any finding; suitable for CI.
    Check,

    /// Print a secret's plaintext to stdout.
    Cat {
        /// File stem (no `.age` suffix).
        name: String,
    },
}

/// Inputs the subcommands need from the `lusid` CLI wrapper.
#[derive(Debug, Clone)]
pub struct CliEnv {
    /// Absolute path to the project's `secrets/` directory. Callers resolve
    /// this from `lusid.toml` (falling back to `<root>/secrets`) before
    /// invoking `run`.
    pub secrets_dir: PathBuf,

    /// Optional path to the operator's decryption identity. Required only
    /// by subcommands that need to decrypt (`edit`, `rekey`, `cat`).
    pub identity_path: Option<PathBuf>,
}

/// Dispatch a [`SecretsCommand`].
pub async fn run(cmd: SecretsCommand, env: CliEnv) -> Result<(), CliError> {
    match cmd {
        SecretsCommand::Ls => cmd_ls(&env).await,
        SecretsCommand::Edit { name } => cmd_edit(&env, &name).await,
        SecretsCommand::Rekey { name } => cmd_rekey(&env, name.as_deref()).await,
        SecretsCommand::Keygen { output } => cmd_keygen(output.as_deref()).await,
        SecretsCommand::Check => cmd_check(&env).await,
        SecretsCommand::Cat { name } => cmd_cat(&env, &name).await,
    }
}

async fn cmd_ls(env: &CliEnv) -> Result<(), CliError> {
    let recipients = Recipients::load(&env.secrets_dir).await?;
    if recipients.files.is_empty() {
        return Ok(());
    }
    for stem in recipients.files.keys() {
        // resolve() can't fail here — we just iterated the keys, so the
        // stem is in [files] by construction. Validation at load time
        // already pinned every reference.
        let resolved = recipients
            .resolve(stem)
            .expect("stem from files.keys() is always resolvable");
        println!(
            "{stem}  {}",
            format_effective_recipients(&recipients, &resolved)
        );
    }
    Ok(())
}

/// `<operators>  +  <machines>` — operators on the left, machine
/// recipients on the right (with `@group` refs expanded), joined by `+`.
///
/// Output rules:
/// - both populated: `<ops>  +  <machines>`
/// - operators only (no machine recipients listed): `<ops>`
/// - machines only (`[operators]` empty, rare): `<machines>`
/// - both empty: unreachable — rejected at load as `EmptyEffectiveRecipients`
fn format_effective_recipients(recipients: &Recipients, resolved: &[ResolvedRecipient]) -> String {
    // `resolve` puts operators first (in [operators] declaration order),
    // then unique machines. Split on the operator/machine boundary by
    // checking each resolved alias against the operators map.
    let (ops, machines): (Vec<&str>, Vec<&str>) = resolved
        .iter()
        .map(|r| r.alias.as_str())
        .partition(|alias| recipients.operators.contains_key(*alias));

    match (ops.is_empty(), machines.is_empty()) {
        (false, false) => format!("{}  +  {}", ops.join(", "), machines.join(", ")),
        (false, true) => ops.join(", "),
        (true, false) => machines.join(", "),
        (true, true) => String::new(),
    }
}

async fn cmd_cat(env: &CliEnv, stem: &str) -> Result<(), CliError> {
    let identity_path = require_identity(env)?;
    let identity = Identity::from_file(identity_path).await?;
    let path = env.secrets_dir.join(format!("{stem}.age"));
    let ciphertext = fs::read(&path).await.map_err(|source| CliError::ReadFile {
        path: path.clone(),
        source,
    })?;
    let plaintext = crypto::decrypt_bytes(&identity, &path, &ciphertext)?;
    print!("{}", plaintext.expose_secret());
    Ok(())
}

async fn cmd_check(env: &CliEnv) -> Result<(), CliError> {
    let recipients = Recipients::load(&env.secrets_dir).await?;
    let report = check::check(&env.secrets_dir, &recipients).await?;
    print_check_report(&report);
    if report.is_clean() {
        Ok(())
    } else {
        Err(CliError::CheckFindings)
    }
}

fn print_check_report(report: &CheckReport) {
    for path in &report.orphans {
        println!("orphan  {}", path.display());
    }
    for stem in &report.missing {
        println!("missing {stem}");
    }
    for err in &report.resolve_errors {
        println!("resolve {err}");
    }
    for drifted in &report.drifted {
        println!("drift   {} — {}", drifted.stem, drifted.reason);
    }
    for read_err in &report.read_errors {
        println!("unread  {}: {}", read_err.path.display(), read_err.source);
    }
    if report.is_clean() {
        println!("ok");
    }
}

async fn cmd_rekey(env: &CliEnv, only: Option<&str>) -> Result<(), CliError> {
    let identity_path = require_identity(env)?;
    let identity = Identity::from_file(identity_path).await?;
    let recipients = Recipients::load(&env.secrets_dir).await?;

    let targets: Vec<String> = match only {
        Some(name) => {
            if !recipients.files.contains_key(name) {
                return Err(CliError::UnknownFile {
                    stem: name.to_owned(),
                });
            }
            vec![name.to_owned()]
        }
        None => recipients.file_stems().map(str::to_owned).collect(),
    };

    for stem in &targets {
        let resolved = recipients.resolve(stem)?;
        let path = env.secrets_dir.join(format!("{stem}.age"));

        let ciphertext = fs::read(&path).await.map_err(|source| CliError::ReadFile {
            path: path.clone(),
            source,
        })?;

        // No-op when the ciphertext header already matches the intended
        // recipients. Each x25519 re-encryption produces different bytes
        // (ephemeral pubkey), so without this check every `rekey` would
        // rewrite every file and churn git history.
        let stanzas = crypto::read_header_stanzas(&ciphertext)?;
        if check::compare_stanzas(&resolved, &stanzas).is_none() {
            tracing::debug!(stem, "header already matches, skipping");
            continue;
        }

        let plaintext = crypto::decrypt_bytes(&identity, &path, &ciphertext)?;
        let new_ciphertext = encrypt_to(&resolved, &path, plaintext.expose_secret().as_bytes())?;
        atomic_write(&path, &new_ciphertext).await?;
        println!("rekeyed {stem}");
    }

    Ok(())
}

async fn cmd_edit(env: &CliEnv, stem: &str) -> Result<(), CliError> {
    let recipients = Recipients::load(&env.secrets_dir).await?;
    // Resolve up-front so a mis-spelled stem errors before we spin up the
    // editor (rather than after the user's done typing).
    let resolved = recipients.resolve(stem)?;

    let path = env.secrets_dir.join(format!("{stem}.age"));
    let existing = match fs::read(&path).await {
        Ok(bytes) => Some(bytes),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
        Err(source) => {
            return Err(CliError::ReadFile {
                path: path.clone(),
                source,
            });
        }
    };

    let initial_plaintext: Vec<u8> = match &existing {
        None => Vec::new(),
        Some(bytes) => {
            let identity_path = require_identity(env)?;
            let identity = Identity::from_file(identity_path).await?;
            let pt = crypto::decrypt_bytes(&identity, &path, bytes)?;
            pt.expose_secret().as_bytes().to_vec()
        }
    };

    let tmp_path = make_tmpfile_path(stem);
    write_private_tmpfile(&tmp_path, &initial_plaintext)?;

    let editor_result = run_editor(&tmp_path).await;

    // Always best-effort cleanup, even on editor failure.
    let read_back = fs::read(&tmp_path).await;
    best_effort_scrub(&tmp_path).await;

    editor_result?;

    let new_plaintext = read_back.map_err(|source| CliError::ReadFile {
        path: tmp_path.clone(),
        source,
    })?;

    // Note(cc): we always re-encrypt when the editor exits cleanly, even if
    // the plaintext is unchanged. Detecting "no change" would require
    // decrypt-then-compare which is equivalent work; the caller can
    // `:q!` out of the editor if they don't want to save.
    let ciphertext = encrypt_to(&resolved, &path, &new_plaintext)?;
    atomic_write(&path, &ciphertext).await?;
    println!("wrote {}", path.display());
    Ok(())
}

async fn cmd_keygen(output: Option<&Path>) -> Result<(), CliError> {
    let default = default_identity_path()?;
    let dest = output.unwrap_or(default.as_path()).to_path_buf();

    if fs::try_exists(&dest).await.unwrap_or(false) {
        return Err(CliError::IdentityExists { path: dest });
    }

    if let Some(parent) = dest.parent() {
        fs::create_dir_all(parent)
            .await
            .map_err(|source| CliError::CreateParentDir {
                path: parent.to_path_buf(),
                source,
            })?;
    }

    let identity = age::x25519::Identity::generate();
    let pubkey = identity.to_public();
    let privkey = identity.to_string();

    // Wrap the file body in `Zeroizing<String>` so the bytes are scrubbed
    // when the function returns. Without this, the plain `String`
    // holding the private-key block would just be deallocated on drop —
    // contents potentially lingering in freed heap until reused.
    let mut contents: Zeroizing<String> = Zeroizing::new(String::new());
    contents.push_str(&format!("# created at unix:{}\n", now_unix_secs()));
    contents.push_str(&format!("# public key: {pubkey}\n"));
    contents.push_str(privkey.expose_secret());
    contents.push('\n');

    // 0600 — this is the long-lived decryption key.
    let mut opts = std::fs::OpenOptions::new();
    opts.create_new(true).write(true).mode(0o600);
    let mut file = opts.open(&dest).map_err(|source| CliError::WriteIdentity {
        path: dest.clone(),
        source,
    })?;
    std::io::Write::write_all(&mut file, contents.as_bytes()).map_err(|source| {
        CliError::WriteIdentity {
            path: dest.clone(),
            source,
        }
    })?;

    println!("wrote identity to {}", dest.display());
    println!("public key: {pubkey}");
    Ok(())
}

// -- helpers ---------------------------------------------------------------

fn require_identity(env: &CliEnv) -> Result<&Path, CliError> {
    env.identity_path
        .as_deref()
        .ok_or(CliError::MissingIdentity)
}

fn encrypt_to(
    resolved: &[ResolvedRecipient],
    path: &Path,
    plaintext: &[u8],
) -> Result<Vec<u8>, CliError> {
    let boxed = to_boxed_recipients(resolved);
    crypto::encrypt_bytes(&boxed, path, plaintext).map_err(CliError::from)
}

async fn atomic_write(dest: &Path, bytes: &[u8]) -> Result<(), CliError> {
    let tmp = dest.with_extension("age.tmp");
    fs::write(&tmp, bytes)
        .await
        .map_err(|source| CliError::WriteFile {
            path: tmp.clone(),
            source,
        })?;
    fs::rename(&tmp, dest)
        .await
        .map_err(|source| CliError::WriteFile {
            path: dest.to_path_buf(),
            source,
        })?;
    Ok(())
}

fn make_tmpfile_path(stem: &str) -> PathBuf {
    let dir = std::env::var_os("XDG_RUNTIME_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("/tmp"));
    let pid = std::process::id();
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    dir.join(format!("lusid-secrets-{stem}-{pid}-{nanos}"))
}

/// Create `path` with mode 0600 and write `contents` to it. Fails if the
/// file already exists (`O_CREAT|O_EXCL` via `create_new`).
fn write_private_tmpfile(path: &Path, contents: &[u8]) -> Result<(), CliError> {
    use std::io::Write;
    let mut opts = std::fs::OpenOptions::new();
    opts.create_new(true).write(true).mode(0o600);
    let mut file = opts.open(path).map_err(|source| CliError::TmpfileCreate {
        path: path.to_path_buf(),
        source,
    })?;
    file.write_all(contents)
        .map_err(|source| CliError::TmpfileWrite {
            path: path.to_path_buf(),
            source,
        })?;
    Ok(())
}

async fn run_editor(path: &Path) -> Result<(), CliError> {
    let editor = std::env::var("EDITOR").unwrap_or_else(|_| "vi".to_string());
    // Editors expect an interactive TTY — inherit std{in,out,err}.
    let status = Command::new(&editor)
        .arg(path)
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status()
        .await
        .map_err(|source| CliError::SpawnEditor {
            editor: editor.clone(),
            source,
        })?;
    if !status.success() {
        return Err(CliError::EditorFailed {
            editor,
            status: status.code().unwrap_or(-1),
        });
    }
    Ok(())
}

async fn best_effort_scrub(path: &Path) {
    // Courtesy overwrite before unlink. COW/journaled/SSD filesystems don't
    // guarantee the old blocks are gone; the real defence is keeping the
    // tmpfile in `$XDG_RUNTIME_DIR` (tmpfs).
    if let Ok(metadata) = fs::metadata(path).await {
        let zeros = vec![0u8; metadata.len() as usize];
        let _ = fs::write(path, &zeros).await;
    }
    let _ = fs::remove_file(path).await;
}

fn default_identity_path() -> Result<PathBuf, CliError> {
    if let Some(base) = std::env::var_os("XDG_CONFIG_HOME") {
        return Ok(PathBuf::from(base).join("lusid").join("identity"));
    }
    let home = std::env::var_os("HOME").ok_or(CliError::NoHome)?;
    Ok(PathBuf::from(home)
        .join(".config")
        .join("lusid")
        .join("identity"))
}

fn now_unix_secs() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

// -- errors ----------------------------------------------------------------

#[derive(Debug, Error, Display)]
pub enum CliError {
    /// Identity path is required for this subcommand but not configured (set via `--identity`, `LUSID_IDENTITY`, or `identity` in `lusid.toml`)
    MissingIdentity,

    /// No [files] entry for {stem:?} in lusid-secrets.toml
    UnknownFile { stem: String },

    /// Cannot determine default identity path (no HOME or XDG_CONFIG_HOME)
    NoHome,

    /// Refusing to overwrite existing identity at {path}
    IdentityExists { path: PathBuf },

    /// Failed to create parent dir {path}: {source}
    CreateParentDir {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// Failed to write identity to {path}: {source}
    WriteIdentity {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// Failed to read {path}: {source}
    ReadFile {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// Failed to write {path}: {source}
    WriteFile {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// Failed to create tmpfile {path}: {source}
    TmpfileCreate {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// Failed to write tmpfile {path}: {source}
    TmpfileWrite {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// Failed to spawn editor {editor}: {source}
    SpawnEditor {
        editor: String,
        #[source]
        source: std::io::Error,
    },

    /// Editor {editor} exited non-zero (status {status})
    EditorFailed { editor: String, status: i32 },

    /// `lusid secrets check` found problems
    CheckFindings,

    /// {0}
    Recipients(#[from] RecipientsError),

    /// {0}
    Resolve(#[from] ResolveError),

    /// {0}
    Identity(#[from] IdentityError),

    /// {0}
    Decrypt(#[from] DecryptError),

    /// {0}
    Encrypt(#[from] EncryptError),

    /// {0}
    Header(#[from] HeaderError),

    /// {0}
    Check(#[from] CheckError),
}

#[cfg(test)]
mod tests {
    use indexmap::IndexMap;

    use super::*;
    use crate::key::Key;

    /// Build a `Recipients` with the given operator aliases (each gets a
    /// fresh x25519 pubkey) and machine aliases (also x25519). The `Key`
    /// values aren't load-bearing for these tests; only `operators.keys()`
    /// matters for the format helper's operator/machine partition.
    fn recipients_with(op_aliases: &[&str], machine_aliases: &[&str]) -> Recipients {
        let mut operators = IndexMap::new();
        for &alias in op_aliases {
            operators.insert(alias.to_owned(), fresh_x25519_key());
        }
        let mut machines = IndexMap::new();
        for &alias in machine_aliases {
            machines.insert(alias.to_owned(), fresh_x25519_key());
        }
        Recipients {
            operators,
            machines,
            groups: IndexMap::new(),
            files: IndexMap::new(),
        }
    }

    fn fresh_x25519_key() -> Key {
        age::x25519::Identity::generate()
            .to_public()
            .to_string()
            .parse()
            .unwrap()
    }

    fn resolved(r: &Recipients, aliases: &[&str]) -> Vec<ResolvedRecipient> {
        // Mimic the shape `cmd_ls` hands the formatter: aliases in
        // resolve()'s order (operators first, then machines).
        aliases
            .iter()
            .map(|&a| {
                let key = r
                    .operators
                    .get(a)
                    .or_else(|| r.machines.get(a))
                    .cloned()
                    .expect("alias must exist in fixture");
                ResolvedRecipient {
                    alias: a.to_owned(),
                    key,
                }
            })
            .collect()
    }

    #[test]
    fn ls_format_ops_and_machines() {
        let r = recipients_with(&["mikey", "alice"], &["rpi", "web1"]);
        let res = resolved(&r, &["mikey", "alice", "rpi", "web1"]);
        assert_eq!(
            format_effective_recipients(&r, &res),
            "mikey, alice  +  rpi, web1"
        );
    }

    #[test]
    fn ls_format_ops_only() {
        let r = recipients_with(&["mikey"], &[]);
        let res = resolved(&r, &["mikey"]);
        assert_eq!(format_effective_recipients(&r, &res), "mikey");
    }

    #[test]
    fn ls_format_machines_only() {
        // [operators] empty + machine recipient — valid per the schema.
        let r = recipients_with(&[], &["rpi"]);
        let res = resolved(&r, &["rpi"]);
        assert_eq!(format_effective_recipients(&r, &res), "rpi");
    }

    /// `cmd_ls` calls `recipients.resolve(stem)` rather than printing the
    /// literal `entry.recipients` list, so a `recipients = ["@prod"]`
    /// entry with `prod = ["rpi", "web1"]` should render the expanded
    /// machines, not `@prod`.
    #[tokio::test]
    async fn ls_format_expands_groups_via_resolve() {
        let dir = tempfile::TempDir::new().unwrap();
        let op_pub = age::x25519::Identity::generate().to_public().to_string();
        let m1_pub = age::x25519::Identity::generate().to_public().to_string();
        let m2_pub = age::x25519::Identity::generate().to_public().to_string();
        std::fs::write(
            dir.path().join("lusid-secrets.toml"),
            format!(
                r#"
[operators]
op = "{op_pub}"

[machines]
rpi  = "{m1_pub}"
web1 = "{m2_pub}"

[groups]
prod = ["rpi", "web1"]

[files]
"f" = {{ recipients = ["@prod"] }}
"#
            ),
        )
        .unwrap();
        let r = Recipients::load(dir.path()).await.unwrap();
        let resolved = r.resolve("f").unwrap();
        assert_eq!(
            format_effective_recipients(&r, &resolved),
            "op  +  rpi, web1"
        );
    }

    /// Sanity: an `entry.recipients = ["rpi"]` that doesn't reference a
    /// group still renders the bare alias, not the literal `@`-prefixed
    /// form.
    #[test]
    fn ls_format_bare_machine_alias() {
        let r = recipients_with(&["op"], &["rpi"]);
        let res = resolved(&r, &["op", "rpi"]);
        assert_eq!(format_effective_recipients(&r, &res), "op  +  rpi");
    }
}
