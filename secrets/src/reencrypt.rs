//! Host-side re-encryption of a secrets directory for a single target.
//!
//! Two entry points, both scope by `[files]` membership for `machine_id`
//! and then re-encrypt each in-scope ciphertext to a single recipient:
//!
//! - [`reencrypt_for_declared_machine`] - recipient is `machine_id`'s own
//!   key from `[machines]`. The `lusid remote apply` flavour: target IS
//!   the declared machine.
//! - [`reencrypt_for_target`] - recipient is supplied by the caller. The
//!   `lusid dev apply` flavour: target SHADOWS the declared machine
//!   (ephemeral VM keypair). `machine_id` only drives `[files]` scoping;
//!   the cryptographic recipient is whatever the caller passes in.

use std::path::{Path, PathBuf};

use secrecy::ExposeSecret;
use thiserror::Error;
use tokio::fs;

use crate::crypto::{DecryptError, EncryptError, decrypt_bytes, encrypt_bytes};
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

#[derive(Debug, Error)]
pub enum ReencryptForTargetError {
    #[error(transparent)]
    Identity(#[from] IdentityError),

    #[error(transparent)]
    RecipientKey(#[from] KeyParseError),

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

/// Scope by `[files]` for `machine_id`, decrypt each with the operator
/// identity, re-encrypt to `target_pubkey` alone.
///
/// `target_pubkey` is an `ssh-ed25519 ...` or `ssh-rsa ...` SSH public key;
/// trailing SSH comments are tolerated.
///
/// Returns `Ok(vec![])` when `machine_id` is in `[machines]` but listed
/// on no file (warn-logged - "this target gets no project secrets").
/// Returns [`ReencryptForTargetError::UnknownMachine`] when `machine_id`
/// is absent from `[machines]`.
#[tracing::instrument(
    skip(host_identity_path, secrets_dir, target_pubkey),
    fields(machine_id)
)]
pub async fn reencrypt_for_target(
    host_identity_path: &Path,
    secrets_dir: &Path,
    machine_id: &str,
    target_pubkey: &str,
) -> Result<Vec<ReencryptedSecret>, ReencryptForTargetError> {
    let recipients = Recipients::load(secrets_dir).await?;
    reencrypt_with_recipients(
        &recipients,
        host_identity_path,
        secrets_dir,
        machine_id,
        target_pubkey,
    )
    .await
}

/// Scope by `[files]` for `machine_id`, decrypt each with the operator
/// identity, re-encrypt to `machine_id`'s own key from `[machines]`. The
/// `lusid remote apply` flavour - target IS the declared machine, so we
/// resolve the recipient internally instead of asking the caller to look
/// it up.
///
/// Same return-shape and error semantics as [`reencrypt_for_target`]:
/// `Ok(vec![])` when the machine is declared but listed on no file,
/// [`ReencryptForTargetError::UnknownMachine`] when it isn't declared.
#[tracing::instrument(skip(host_identity_path, secrets_dir), fields(machine_id))]
pub async fn reencrypt_for_declared_machine(
    host_identity_path: &Path,
    secrets_dir: &Path,
    machine_id: &str,
) -> Result<Vec<ReencryptedSecret>, ReencryptForTargetError> {
    let recipients = Recipients::load(secrets_dir).await?;
    // Resolve `[machines][machine_id]` here so the inner helper sees the
    // pubkey as a string, identical to the caller-supplied path. Errors
    // here look identical to what `reencrypt_for_target` would raise if a
    // caller passed an unknown machine_id, so the public surface is the
    // same shape from either entry point.
    let target_pubkey = recipients
        .machines
        .get(machine_id)
        .ok_or_else(|| ReencryptForTargetError::UnknownMachine {
            machine_id: machine_id.to_owned(),
        })?
        .to_string();
    reencrypt_with_recipients(
        &recipients,
        host_identity_path,
        secrets_dir,
        machine_id,
        &target_pubkey,
    )
    .await
}

/// Shared body of both public entry points: takes an already-parsed
/// `Recipients`, validates `machine_id`, scopes by `[files]`, decrypts +
/// re-encrypts each in-scope ciphertext to `target_pubkey`. Private - the
/// callers above are the only intended consumers and `&Recipients`
/// isn't part of our stable surface.
async fn reencrypt_with_recipients(
    recipients: &Recipients,
    host_identity_path: &Path,
    secrets_dir: &Path,
    machine_id: &str,
    target_pubkey: &str,
) -> Result<Vec<ReencryptedSecret>, ReencryptForTargetError> {
    if !recipients.machines.contains_key(machine_id) {
        return Err(ReencryptForTargetError::UnknownMachine {
            machine_id: machine_id.to_owned(),
        });
    }

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

    let target_key: Key = target_pubkey.parse()?;
    let target_recipients: Vec<Box<dyn age::Recipient + Send>> =
        vec![Box::new(target_key.recipient().clone())];

    let host_identity = Identity::from_file(host_identity_path).await?;

    let mut out = Vec::with_capacity(stems.len());
    for stem in &stems {
        let path = secrets_dir.join(format!("{stem}.age"));
        let ciphertext =
            fs::read(&path)
                .await
                .map_err(|source| ReencryptForTargetError::ReadFile {
                    path: path.clone(),
                    source,
                })?;
        let plaintext = decrypt_bytes(&host_identity, &path, &ciphertext)?;
        // `virtual_path` is only used for error labelling in encrypt_bytes.
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
        "re-encrypted [files]-scoped secrets"
    );
    Ok(out)
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};
    use std::str::FromStr;

    use secrecy::ExposeSecret;
    use tempfile::TempDir;

    use super::*;
    use crate::crypto::{decrypt_bytes, encrypt_bytes};
    use crate::test_fixtures::{
        TEST_SSH_ED25519_A_PRIV, TEST_SSH_ED25519_A_PUB, TEST_SSH_ED25519_B_PRIV,
        TEST_SSH_ED25519_B_PUB, TEST_SSH_ED25519_C_PUB,
    };

    fn ssh_recipient(pubkey: &str) -> Box<dyn age::Recipient + Send> {
        let mut parts = pubkey.split_whitespace();
        let kind = parts.next().unwrap();
        let body = parts.next().unwrap();
        Box::new(age::ssh::Recipient::from_str(&format!("{kind} {body}")).unwrap())
    }

    /// Set up a temp dir with: an operator SSH identity file (keypair A) at
    /// `host_identity`, a `secrets/` subdir, and `*.age` ciphertexts for
    /// each `(stem, plaintext)` encrypted to the operator's pubkey.
    fn write_host_and_secrets(files: &[(&str, &[u8])]) -> (TempDir, PathBuf, PathBuf) {
        let dir = TempDir::new().unwrap();
        let host_identity_path = dir.path().join("host_identity");
        std::fs::write(&host_identity_path, TEST_SSH_ED25519_A_PRIV).unwrap();

        let secrets_dir = dir.path().join("secrets");
        std::fs::create_dir(&secrets_dir).unwrap();
        for (stem, value) in files {
            let ct = encrypt_bytes(
                &[ssh_recipient(TEST_SSH_ED25519_A_PUB)],
                Path::new(stem),
                value,
            )
            .unwrap();
            std::fs::write(secrets_dir.join(format!("{stem}.age")), &ct).unwrap();
        }

        (dir, host_identity_path, secrets_dir)
    }

    /// Common fixture: two machines (rpi → keypair B, web1 → keypair C),
    /// two files (one per machine), operator is keypair A.
    fn two_machine_fixture() -> (TempDir, PathBuf, PathBuf) {
        let (dir, host_identity_path, secrets_dir) =
            write_host_and_secrets(&[("rpi_only", b"rpipayload"), ("web_only", b"webpayload")]);
        std::fs::write(
            secrets_dir.join("lusid-secrets.toml"),
            format!(
                r#"
[machines]
rpi  = "{TEST_SSH_ED25519_B_PUB}"
web1 = "{TEST_SSH_ED25519_C_PUB}"

[files]
"rpi_only" = {{ recipients = ["rpi"] }}
"web_only" = {{ recipients = ["web1"] }}
"#,
            ),
        )
        .unwrap();
        (dir, host_identity_path, secrets_dir)
    }

    /// `[files]` scope picks rpi_only (not web_only); ciphertext
    /// decrypts under the target key.
    #[tokio::test]
    async fn filters_by_files_and_uses_target_key() {
        let (_dir, host_identity_path, secrets_dir) = two_machine_fixture();

        let reencrypted = reencrypt_for_target(
            &host_identity_path,
            &secrets_dir,
            "rpi",
            TEST_SSH_ED25519_B_PUB,
        )
        .await
        .unwrap();
        assert_eq!(reencrypted.len(), 1);
        assert_eq!(reencrypted[0].stem, "rpi_only");

        let target_id: Identity = TEST_SSH_ED25519_B_PRIV.parse().unwrap();
        let pt = decrypt_bytes(
            &target_id,
            Path::new("rpi_only"),
            &reencrypted[0].ciphertext,
        )
        .unwrap();
        assert_eq!(pt.expose_secret().as_str(), "rpipayload");
    }

    /// `target_pubkey` is independent of `[machines]`: an ephemeral dev VM
    /// key (here, keypair C used as if it were a fresh VM) is a valid
    /// recipient, and rpi's own declared key (keypair B) MUST NOT decrypt
    /// the result.
    #[tokio::test]
    async fn target_pubkey_independent_of_machines_table() {
        let (_dir, host_identity_path, secrets_dir) = two_machine_fixture();

        let reencrypted = reencrypt_for_target(
            &host_identity_path,
            &secrets_dir,
            "rpi",
            TEST_SSH_ED25519_C_PUB,
        )
        .await
        .unwrap();
        assert_eq!(reencrypted.len(), 1);

        let rpi_id: Identity = TEST_SSH_ED25519_B_PRIV.parse().unwrap();
        assert!(decrypt_bytes(&rpi_id, Path::new("rpi_only"), &reencrypted[0].ciphertext).is_err());
    }

    #[tokio::test]
    async fn unknown_machine_errors() {
        let (_dir, host_identity_path, secrets_dir) = write_host_and_secrets(&[]);
        std::fs::write(
            secrets_dir.join("lusid-secrets.toml"),
            format!(
                r#"
[machines]
known = "{TEST_SSH_ED25519_B_PUB}"
"#,
            ),
        )
        .unwrap();

        let err = reencrypt_for_target(
            &host_identity_path,
            &secrets_dir,
            "missing",
            TEST_SSH_ED25519_B_PUB,
        )
        .await
        .unwrap_err();
        assert!(matches!(
            err,
            ReencryptForTargetError::UnknownMachine { machine_id } if machine_id == "missing"
        ));
    }

    /// Machine appears in `[machines]` but is listed on no file. Returns
    /// Ok(empty), not an error - and skips the operator identity load.
    #[tokio::test]
    async fn machine_with_no_files_returns_empty() {
        let (_dir, host_identity_path, secrets_dir) =
            write_host_and_secrets(&[("only_other", b"someplain")]);
        std::fs::write(
            secrets_dir.join("lusid-secrets.toml"),
            format!(
                r#"
[machines]
target = "{TEST_SSH_ED25519_B_PUB}"
other  = "{TEST_SSH_ED25519_C_PUB}"

[files]
"only_other" = {{ recipients = ["other"] }}
"#,
            ),
        )
        .unwrap();

        let reencrypted = reencrypt_for_target(
            &host_identity_path,
            &secrets_dir,
            "target",
            TEST_SSH_ED25519_B_PUB,
        )
        .await
        .unwrap();
        assert!(reencrypted.is_empty());
    }

    #[tokio::test]
    async fn rejects_malformed_target_pubkey() {
        let (_dir, host_identity_path, secrets_dir) = two_machine_fixture();
        let err = reencrypt_for_target(&host_identity_path, &secrets_dir, "rpi", "not-a-key")
            .await
            .unwrap_err();
        assert!(matches!(err, ReencryptForTargetError::RecipientKey(_)));
    }

    /// `reencrypt_for_declared_machine` resolves the recipient from
    /// `[machines]` itself; the resulting ciphertext decrypts under the
    /// declared key without the caller having to pass it in.
    #[tokio::test]
    async fn declared_machine_uses_machines_table_pubkey() {
        let (_dir, host_identity_path, secrets_dir) = two_machine_fixture();

        let reencrypted = reencrypt_for_declared_machine(&host_identity_path, &secrets_dir, "rpi")
            .await
            .unwrap();
        assert_eq!(reencrypted.len(), 1);
        assert_eq!(reencrypted[0].stem, "rpi_only");

        let rpi_id: Identity = TEST_SSH_ED25519_B_PRIV.parse().unwrap();
        let pt = decrypt_bytes(&rpi_id, Path::new("rpi_only"), &reencrypted[0].ciphertext).unwrap();
        assert_eq!(pt.expose_secret().as_str(), "rpipayload");
    }

    #[tokio::test]
    async fn declared_machine_unknown_errors() {
        let (_dir, host_identity_path, secrets_dir) = two_machine_fixture();
        let err = reencrypt_for_declared_machine(&host_identity_path, &secrets_dir, "missing")
            .await
            .unwrap_err();
        assert!(matches!(
            err,
            ReencryptForTargetError::UnknownMachine { machine_id } if machine_id == "missing"
        ));
    }

    #[tokio::test]
    async fn declared_machine_missing_toml_errors() {
        let dir = TempDir::new().unwrap();
        let host_identity_path = dir.path().join("host_identity");
        std::fs::write(&host_identity_path, TEST_SSH_ED25519_A_PRIV).unwrap();
        let secrets_dir = dir.path().join("secrets");
        std::fs::create_dir(&secrets_dir).unwrap();

        let err = reencrypt_for_declared_machine(&host_identity_path, &secrets_dir, "any")
            .await
            .unwrap_err();
        assert!(matches!(
            err,
            ReencryptForTargetError::Recipients(RecipientsError::Missing { .. })
        ));
    }
}
