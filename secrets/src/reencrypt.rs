//! Host-side re-encryption of a secrets directory for a single target.
//!
//! [`reencrypt_for_target`] scopes the bundle to what `machine_id` is
//! declared a recipient of in `[files]`, then re-encrypts each file to
//! `target_pubkey` alone.
//!
//! Two callers:
//!
//! - `remote apply`: target IS the declared machine. Caller passes
//!   `machine_id`'s own key from `[machines]`.
//! - `dev apply`: target SHADOWS the declared machine (an ephemeral VM
//!   keypair). Caller passes `machine_id` for `[files]` scoping and the
//!   VM's pubkey as the cryptographic recipient.

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
/// `target_pubkey` is an `age1...` x25519 recipient or an
/// `ssh-ed25519 ...` / `ssh-rsa ...` SSH public key; trailing SSH
/// comments are tolerated.
///
/// Returns `Ok(vec![])` when `machine_id` is in `[machines]` but listed
/// on no file (warn-logged — "this target gets no project secrets").
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
    let target_recipients: Vec<Box<dyn age::Recipient + Send>> = match target_key {
        Key::X25519(k) => vec![Box::new(k)],
        Key::Ssh(k) => vec![Box::new(k)],
    };

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

    use secrecy::ExposeSecret;
    use tempfile::TempDir;

    use super::*;
    use crate::crypto::{decrypt_bytes, encrypt_bytes};

    /// Set up a temp dir with: an operator x25519 identity file at
    /// `host_identity`, a `secrets/` subdir, and `*.age` ciphertexts for
    /// each `(stem, plaintext)` encrypted to the operator's pubkey.
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

    /// Common fixture: two machines (rpi, web1), two files (one per
    /// machine), one operator identity.
    fn two_machine_fixture() -> (
        TempDir,
        PathBuf,
        PathBuf,
        age::x25519::Identity, // rpi target identity
    ) {
        let target_rpi = age::x25519::Identity::generate();
        let target_web = age::x25519::Identity::generate();
        let (dir, host_identity_path, secrets_dir, _) =
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
"#,
                rpi_pub = target_rpi.to_public(),
                web_pub = target_web.to_public(),
            ),
        )
        .unwrap();
        (dir, host_identity_path, secrets_dir, target_rpi)
    }

    /// `[files]` scope picks rpi_only (not web_only); ciphertext
    /// decrypts under the target key.
    #[tokio::test]
    async fn filters_by_files_and_uses_target_key() {
        let target = age::x25519::Identity::generate();
        let target_pub = target.to_public().to_string();

        let (_dir, host_identity_path, secrets_dir, _) = two_machine_fixture();

        let reencrypted =
            reencrypt_for_target(&host_identity_path, &secrets_dir, "rpi", &target_pub)
                .await
                .unwrap();
        assert_eq!(reencrypted.len(), 1);
        assert_eq!(reencrypted[0].stem, "rpi_only");

        let target_id: Identity = target.to_string().expose_secret().parse().unwrap();
        let pt = decrypt_bytes(
            &target_id,
            Path::new("rpi_only"),
            &reencrypted[0].ciphertext,
        )
        .unwrap();
        assert_eq!(pt.expose_secret().as_str(), "rpipayload");
    }

    /// `target_pubkey` is independent of `[machines]`: a dev VM key
    /// that isn't in `[machines]` is a valid recipient, and rpi's
    /// own production key MUST NOT decrypt the result.
    #[tokio::test]
    async fn target_pubkey_independent_of_machines_table() {
        let vm = age::x25519::Identity::generate();
        let vm_pub = vm.to_public().to_string();

        let (_dir, host_identity_path, secrets_dir, target_rpi) = two_machine_fixture();

        let reencrypted = reencrypt_for_target(&host_identity_path, &secrets_dir, "rpi", &vm_pub)
            .await
            .unwrap();
        assert_eq!(reencrypted.len(), 1);

        let rpi_id: Identity = target_rpi.to_string().expose_secret().parse().unwrap();
        assert!(decrypt_bytes(&rpi_id, Path::new("rpi_only"), &reencrypted[0].ciphertext).is_err());
    }

    #[tokio::test]
    async fn unknown_machine_errors() {
        let target = age::x25519::Identity::generate();
        let target_pub = target.to_public().to_string();

        let (_dir, host_identity_path, secrets_dir, _) = write_host_and_secrets(&[]);
        std::fs::write(
            secrets_dir.join("lusid-secrets.toml"),
            format!(
                r#"
[machines]
known = "{}"
"#,
                target.to_public(),
            ),
        )
        .unwrap();

        let err = reencrypt_for_target(&host_identity_path, &secrets_dir, "missing", &target_pub)
            .await
            .unwrap_err();
        assert!(matches!(
            err,
            ReencryptForTargetError::UnknownMachine { machine_id } if machine_id == "missing"
        ));
    }

    /// Machine appears in `[machines]` but is listed on no file. Returns
    /// Ok(empty), not an error — and skips the operator identity load.
    #[tokio::test]
    async fn machine_with_no_files_returns_empty() {
        let target = age::x25519::Identity::generate();
        let other = age::x25519::Identity::generate();
        let target_pub = target.to_public().to_string();

        let (_dir, host_identity_path, secrets_dir, _) =
            write_host_and_secrets(&[("only_other", b"someplain")]);
        std::fs::write(
            secrets_dir.join("lusid-secrets.toml"),
            format!(
                r#"
[machines]
target = "{}"
other  = "{}"

[files]
"only_other" = {{ recipients = ["other"] }}
"#,
                target.to_public(),
                other.to_public(),
            ),
        )
        .unwrap();

        let reencrypted =
            reencrypt_for_target(&host_identity_path, &secrets_dir, "target", &target_pub)
                .await
                .unwrap();
        assert!(reencrypted.is_empty());
    }

    #[tokio::test]
    async fn rejects_malformed_target_pubkey() {
        let (_dir, host_identity_path, secrets_dir, _) = two_machine_fixture();
        let err = reencrypt_for_target(&host_identity_path, &secrets_dir, "rpi", "not-a-key")
            .await
            .unwrap_err();
        assert!(matches!(err, ReencryptForTargetError::RecipientKey(_)));
    }
}
