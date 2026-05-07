//! Host-side re-encryption of a secrets directory for a single target.
//!
//! Two flows:
//!
//! - [`reencrypt_all`] — walks the dir, ships every `*.age`. Used by
//!   `dev apply` (target is a fresh VM not declared in `[machines]`).
//! - [`reencrypt_for_machine`] — looks the machine up in `[machines]` and
//!   ships only the files it's a recipient on per `[files]`. Used by
//!   `remote apply`.

use std::path::{Path, PathBuf};

use secrecy::ExposeSecret;
use thiserror::Error;
use tokio::fs;

use crate::crypto::{DecryptError, EncryptError, decrypt_bytes, encrypt_bytes};
use crate::decrypt_all::{DecryptAllError, decrypt_all};
use crate::identity::{Identity, IdentityError};
use crate::key::{Key, KeyParseError};
use crate::recipients::{Recipients, RecipientsError};

/// A re-encrypted secret: file stem (e.g. `api_token`) and the new age
/// ciphertext encrypted to the target's key alone. Callers typically write
/// each back as `<remote_secrets_dir>/<stem>.age` on the target.
#[derive(Debug, Clone)]
pub struct ReencryptedSecret {
    pub stem: String,
    pub ciphertext: Vec<u8>,
}

/// Errors from either re-encryption flow. Single enum because the two
/// helpers share most of their failure modes; the toml-lookup path adds
/// `Recipients` / `Decrypt` / `ReadFile` / `UnknownMachine` arms.
#[derive(Debug, Error)]
pub enum ReencryptForMachineError {
    #[error(transparent)]
    Identity(#[from] IdentityError),

    #[error(transparent)]
    MachineKey(#[from] KeyParseError),

    #[error(transparent)]
    DecryptAll(#[from] DecryptAllError),

    #[error(transparent)]
    Encrypt(#[from] EncryptError),

    #[error(transparent)]
    Recipients(#[from] RecipientsError),

    #[error("failed to read {path} for re-encryption: {source}")]
    ReadFile {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error(transparent)]
    Decrypt(#[from] DecryptError),

    #[error("[machines] has no entry for {machine_id:?} in lusid-secrets.toml")]
    UnknownMachine { machine_id: String },
}

/// Walk `secrets_dir`, decrypt every `*.age` with the operator identity,
/// re-encrypt each to `recipient_pubkey` alone, return the ciphertexts.
///
/// Used by `lusid dev apply`: the target is a fresh VM whose ephemeral
/// keypair isn't declared in `[machines]`, so we trust the operator and
/// ship every secret. Does NOT consult `lusid-secrets.toml`.
///
/// `recipient_pubkey` is the target's age recipient as a string — either
/// `age1...` for an x25519 recipient or `ssh-ed25519 ...` / `ssh-rsa ...`
/// for an SSH public key. Trailing SSH comments (`... user@host`) are
/// tolerated.
///
/// Plaintexts live only inside the intermediate [`crate::Secrets`] and are
/// zeroised when it drops at function return. The operator identity never
/// leaves the host.
#[tracing::instrument(fields(identity = %host_identity_path.display(), dir = %secrets_dir.display()))]
pub async fn reencrypt_all(
    host_identity_path: &Path,
    secrets_dir: &Path,
    recipient_pubkey: &str,
) -> Result<Vec<ReencryptedSecret>, ReencryptForMachineError> {
    let host_identity = Identity::from_file(host_identity_path).await?;
    let recipient_key: Key = recipient_pubkey.parse()?;

    let secrets = decrypt_all(&host_identity, secrets_dir).await?;
    let recipients = boxed_recipient(recipient_key);

    let mut out = Vec::with_capacity(secrets.len());
    for (stem, secret) in secrets.iter() {
        // `path` is only used for error labelling inside encrypt_bytes. A
        // virtual `<stem>.age` keeps diagnostics meaningful without a
        // filesystem round-trip.
        let virtual_path = Path::new(stem);
        let ciphertext =
            encrypt_bytes(&recipients, virtual_path, secret.expose_secret().as_bytes())?;
        out.push(ReencryptedSecret {
            stem: stem.to_owned(),
            ciphertext,
        });
    }

    tracing::info!(count = out.len(), "re-encrypted every secret in dir");
    Ok(out)
}

/// Look up `machine_id` in `[machines]`, find the files it's listed on via
/// `[files]`, decrypt each with the operator identity, re-encrypt to the
/// machine's key alone.
///
/// Used by `lusid remote apply`: the target is declared in
/// `lusid-secrets.toml`, so filtering by `[files]` keeps the target from
/// receiving secrets it isn't a declared recipient of.
///
/// Returns `Ok(vec![])` when the machine appears in `[machines]` but isn't
/// listed on any file — a valid "this target gets no project secrets"
/// outcome, distinguished by a `tracing::warn!` from "no secrets in repo".
///
/// Returns [`ReencryptForMachineError::UnknownMachine`] when `machine_id`
/// is absent from `[machines]`.
#[tracing::instrument(skip(host_identity_path, secrets_dir), fields(machine_id))]
pub async fn reencrypt_for_machine(
    host_identity_path: &Path,
    secrets_dir: &Path,
    machine_id: &str,
) -> Result<Vec<ReencryptedSecret>, ReencryptForMachineError> {
    let recipients = Recipients::load(secrets_dir).await?;
    let machine_key = recipients
        .machines
        .get(machine_id)
        .cloned()
        .ok_or_else(|| ReencryptForMachineError::UnknownMachine {
            machine_id: machine_id.to_owned(),
        })?;

    let stems: Vec<String> = recipients
        .files_for_alias(machine_id)
        .into_iter()
        .map(str::to_owned)
        .collect();

    if stems.is_empty() {
        tracing::warn!(
            machine_id,
            "machine declared in [machines] but listed on no [files] entry; \
             shipping zero secrets"
        );
        return Ok(Vec::new());
    }

    let host_identity = Identity::from_file(host_identity_path).await?;
    let target_recipients = boxed_recipient(machine_key);

    let mut out = Vec::with_capacity(stems.len());
    for stem in &stems {
        let path = secrets_dir.join(format!("{stem}.age"));
        let ciphertext =
            fs::read(&path)
                .await
                .map_err(|source| ReencryptForMachineError::ReadFile {
                    path: path.clone(),
                    source,
                })?;
        let plaintext = decrypt_bytes(&host_identity, &path, &ciphertext)?;
        let virtual_path = Path::new(stem);
        let new_ciphertext = encrypt_bytes(
            &target_recipients,
            virtual_path,
            plaintext.expose_secret().as_bytes(),
        )?;
        out.push(ReencryptedSecret {
            stem: stem.to_owned(),
            ciphertext: new_ciphertext,
        });
    }

    tracing::info!(
        machine_id,
        count = out.len(),
        "re-encrypted [files]-scoped secrets for machine"
    );
    Ok(out)
}

fn boxed_recipient(key: Key) -> Vec<Box<dyn age::Recipient + Send>> {
    match key {
        Key::X25519(k) => vec![Box::new(k)],
        Key::Ssh(k) => vec![Box::new(k)],
    }
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use secrecy::ExposeSecret;
    use tempfile::TempDir;

    use super::*;
    use crate::crypto::{decrypt_bytes, encrypt_bytes};

    /// Set up a temp dir with: an operator x25519 identity file at
    /// `host_identity`, a `secrets/` subdir, and `*.age` ciphertexts for
    /// each `(stem, plaintext)` encrypted to the operator's pubkey.
    /// Returns (TempDir, identity_path, secrets_dir, operator_age_id).
    fn write_host_and_secrets(
        files: &[(&str, &[u8])],
    ) -> (TempDir, PathBuf, PathBuf, age::x25519::Identity) {
        let dir = TempDir::new().unwrap();
        let host_age = age::x25519::Identity::generate();
        let host_identity_path = dir.path().join("host_identity");
        std::fs::write(&host_identity_path, host_age.to_string().expose_secret()).unwrap();

        let secrets_dir = dir.path().join("secrets");
        std::fs::create_dir(&secrets_dir).unwrap();
        for (stem, value) in files {
            let ct =
                encrypt_bytes(&[Box::new(host_age.to_public())], Path::new(stem), value).unwrap();
            std::fs::write(secrets_dir.join(format!("{stem}.age")), &ct).unwrap();
        }

        (dir, host_identity_path, secrets_dir, host_age)
    }

    // -- reencrypt_all -------------------------------------------------

    #[tokio::test]
    async fn reencrypt_all_round_trips() {
        let target_age = age::x25519::Identity::generate();
        let target_identity: Identity = target_age.to_string().expose_secret().parse().unwrap();
        let target_pubkey = target_age.to_public().to_string();

        let (_dir, host_identity_path, secrets_dir, host_age) =
            write_host_and_secrets(&[("alpha", b"alphaplain"), ("beta", b"betaplain")]);

        let reencrypted = reencrypt_all(&host_identity_path, &secrets_dir, &target_pubkey)
            .await
            .unwrap();
        assert_eq!(reencrypted.len(), 2);

        let by_stem: std::collections::HashMap<&str, &Vec<u8>> = reencrypted
            .iter()
            .map(|r| (r.stem.as_str(), &r.ciphertext))
            .collect();
        let alpha_ct = by_stem["alpha"];
        let beta_ct = by_stem["beta"];

        let alpha_pt = decrypt_bytes(&target_identity, Path::new("alpha"), alpha_ct).unwrap();
        let beta_pt = decrypt_bytes(&target_identity, Path::new("beta"), beta_ct).unwrap();
        assert_eq!(alpha_pt.expose_secret().as_str(), "alphaplain");
        assert_eq!(beta_pt.expose_secret().as_str(), "betaplain");

        // Host identity can no longer decrypt the re-encrypted payload —
        // only the target recipient is on the ciphertext.
        let host_identity: Identity = host_age.to_string().expose_secret().parse().unwrap();
        assert!(decrypt_bytes(&host_identity, Path::new("alpha"), alpha_ct).is_err());
    }

    #[tokio::test]
    async fn reencrypt_all_rejects_malformed_pubkey() {
        let (_dir, host_identity_path, secrets_dir, _) = write_host_and_secrets(&[]);
        let err = reencrypt_all(&host_identity_path, &secrets_dir, "not-a-key")
            .await
            .unwrap_err();
        assert!(matches!(err, ReencryptForMachineError::MachineKey(_)));
    }

    // -- reencrypt_for_machine ----------------------------------------

    /// Two machines, two files; calling `reencrypt_for_machine("rpi")`
    /// re-encrypts the file `rpi` is listed for and skips the one only
    /// `web1` is on.
    #[tokio::test]
    async fn reencrypt_for_machine_filters_by_files() {
        let target_rpi = age::x25519::Identity::generate();
        let target_web = age::x25519::Identity::generate();
        let rpi_pub = target_rpi.to_public().to_string();
        let web_pub = target_web.to_public().to_string();

        let (_dir, host_identity_path, secrets_dir, _) =
            write_host_and_secrets(&[("rpi_only", b"rpipayload"), ("web_only", b"webpayload")]);

        std::fs::write(
            secrets_dir.join("lusid-secrets.toml"),
            format!(
                r#"
[machines]
rpi  = "{rpi_pub}"
web1 = "{web_pub}"

[files]
"rpi_only" = {{ recipients = ["rpi"] }}
"web_only" = {{ recipients = ["web1"] }}
"#
            ),
        )
        .unwrap();

        let reencrypted = reencrypt_for_machine(&host_identity_path, &secrets_dir, "rpi")
            .await
            .unwrap();
        assert_eq!(reencrypted.len(), 1);
        assert_eq!(reencrypted[0].stem, "rpi_only");

        // The one file rpi gets must decrypt with rpi's identity.
        let rpi_identity: Identity = target_rpi.to_string().expose_secret().parse().unwrap();
        let pt = decrypt_bytes(
            &rpi_identity,
            Path::new("rpi_only"),
            &reencrypted[0].ciphertext,
        )
        .unwrap();
        assert_eq!(pt.expose_secret().as_str(), "rpipayload");
    }

    #[tokio::test]
    async fn reencrypt_for_machine_unknown_machine_errors() {
        let target = age::x25519::Identity::generate();
        let target_pub = target.to_public().to_string();

        let (_dir, host_identity_path, secrets_dir, _) = write_host_and_secrets(&[]);
        std::fs::write(
            secrets_dir.join("lusid-secrets.toml"),
            format!(
                r#"
[machines]
known = "{target_pub}"
"#
            ),
        )
        .unwrap();

        let err = reencrypt_for_machine(&host_identity_path, &secrets_dir, "missing")
            .await
            .unwrap_err();
        match err {
            ReencryptForMachineError::UnknownMachine { machine_id } => {
                assert_eq!(machine_id, "missing");
            }
            other => panic!("wrong variant: {other:?}"),
        }
    }

    /// Machine appears in `[machines]` but is listed on no file in
    /// `[files]`. Returns Ok(empty), not an error — and skips the operator
    /// identity load entirely (no need to decrypt anything).
    #[tokio::test]
    async fn reencrypt_for_machine_machine_with_no_files_returns_empty() {
        let target = age::x25519::Identity::generate();
        let target_pub = target.to_public().to_string();
        let other = age::x25519::Identity::generate();
        let other_pub = other.to_public().to_string();

        let (_dir, host_identity_path, secrets_dir, _) =
            write_host_and_secrets(&[("only_other", b"someplain")]);
        std::fs::write(
            secrets_dir.join("lusid-secrets.toml"),
            format!(
                r#"
[machines]
target = "{target_pub}"
other  = "{other_pub}"

[files]
"only_other" = {{ recipients = ["other"] }}
"#
            ),
        )
        .unwrap();

        let reencrypted = reencrypt_for_machine(&host_identity_path, &secrets_dir, "target")
            .await
            .unwrap();
        assert!(reencrypted.is_empty());
    }
}
