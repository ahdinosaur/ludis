//! Host-side re-encryption of a secrets directory for a single target.
//!
//! Both flows scope the bundle to what the named machine is declared a
//! recipient of in `[files]`. They differ only in the cryptographic
//! recipient:
//!
//! - [`reencrypt_for_machine`] — encrypts to `[machines][machine_id]`.
//!   Used by `remote apply`: the target IS the declared machine.
//! - [`reencrypt_for_dev_vm`] — encrypts to a separate `vm_pubkey` (an
//!   ephemeral dev-VM keypair). Used by `dev apply`: the target SHADOWS
//!   the declared machine, so it should see the same set of secrets but
//!   under its own throwaway key.

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

/// Errors from either re-encryption flow. Single enum because the two
/// helpers share the same toml-lookup + per-file decrypt+encrypt body.
#[derive(Debug, Error)]
pub enum ReencryptForMachineError {
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

/// Look up `machine_id` in `[machines]`, find the files it's listed on via
/// `[files]`, decrypt each with the operator identity, re-encrypt to the
/// machine's key alone.
///
/// Used by `lusid remote apply`: the target IS the declared machine.
/// Filtering by `[files]` keeps the target from receiving secrets it
/// isn't a declared recipient of.
///
/// See [`reencrypt_for_dev_vm`] for the dev variant that scopes the same
/// way but encrypts to a stand-in recipient.
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
    reencrypt_filtered(
        host_identity_path,
        secrets_dir,
        &recipients,
        machine_id,
        boxed_recipient(machine_key),
    )
    .await
}

/// Like [`reencrypt_for_machine`], but encrypt the resulting ciphertexts
/// to `vm_pubkey` (an ephemeral dev-VM keypair) instead of
/// `[machines][machine_id]`. The `[files]` scoping is unchanged: the dev
/// VM sees exactly what `lusid remote apply --machine machine_id` would
/// ship to production, just under a throwaway key.
///
/// Used by `lusid dev apply`: developer iterates against a VM that
/// shadows the declared production target. The VM's keypair isn't in
/// `[machines]`, so we use `machine_id` purely for `[files]` scoping
/// and pass the VM's pubkey as the cryptographic recipient.
///
/// `vm_pubkey` is an `age1...` x25519 recipient or an `ssh-ed25519 ...`
/// / `ssh-rsa ...` SSH public key; trailing SSH comments are tolerated.
///
/// Returns `Ok(vec![])` when `machine_id` is in `[machines]` but listed
/// on no file (warn-logged), and
/// [`ReencryptForMachineError::UnknownMachine`] when it's absent.
#[tracing::instrument(skip(host_identity_path, secrets_dir, vm_pubkey), fields(machine_id))]
pub async fn reencrypt_for_dev_vm(
    host_identity_path: &Path,
    secrets_dir: &Path,
    machine_id: &str,
    vm_pubkey: &str,
) -> Result<Vec<ReencryptedSecret>, ReencryptForMachineError> {
    let recipients = Recipients::load(secrets_dir).await?;
    if !recipients.machines.contains_key(machine_id) {
        return Err(ReencryptForMachineError::UnknownMachine {
            machine_id: machine_id.to_owned(),
        });
    }
    let vm_key: Key = vm_pubkey.parse()?;
    reencrypt_filtered(
        host_identity_path,
        secrets_dir,
        &recipients,
        machine_id,
        boxed_recipient(vm_key),
    )
    .await
}

/// Shared body for the two public re-encrypt entry points. Computes
/// `files_for_alias(machine_id)`, walks each stem, decrypts with the
/// operator identity, re-encrypts to `target_recipients`. Skips the
/// operator identity load entirely when the file list is empty.
async fn reencrypt_filtered(
    host_identity_path: &Path,
    secrets_dir: &Path,
    recipients: &Recipients,
    machine_id: &str,
    target_recipients: Vec<Box<dyn age::Recipient + Send>>,
) -> Result<Vec<ReencryptedSecret>, ReencryptForMachineError> {
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
        // `path` here is only used for error labelling in encrypt_bytes.
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

    /// Common fixture: two machines (rpi, web1), two files (one per
    /// machine), one operator identity. Returns everything callers need
    /// to drive either re-encrypt entry point.
    fn two_machine_fixture() -> (
        TempDir,
        PathBuf,
        PathBuf,
        age::x25519::Identity, // rpi target identity
        age::x25519::Identity, // web1 target identity
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
        (dir, host_identity_path, secrets_dir, target_rpi, target_web)
    }

    // -- reencrypt_for_machine ----------------------------------------

    /// rpi is listed on `rpi_only`; calling for `rpi` re-encrypts that
    /// file (skipping `web_only`) to rpi's own [machines] key.
    #[tokio::test]
    async fn reencrypt_for_machine_filters_by_files_and_uses_machine_key() {
        let (_dir, host_identity_path, secrets_dir, target_rpi, _) = two_machine_fixture();

        let reencrypted = reencrypt_for_machine(&host_identity_path, &secrets_dir, "rpi")
            .await
            .unwrap();
        assert_eq!(reencrypted.len(), 1);
        assert_eq!(reencrypted[0].stem, "rpi_only");

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
        let other = age::x25519::Identity::generate();

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

        let reencrypted = reencrypt_for_machine(&host_identity_path, &secrets_dir, "target")
            .await
            .unwrap();
        assert!(reencrypted.is_empty());
    }

    // -- reencrypt_for_dev_vm -----------------------------------------

    /// Same `[files]` scope as `reencrypt_for_machine` for `rpi`, but the
    /// resulting ciphertext is decryptable by the dev VM key, NOT by
    /// rpi's production key. Verifies the two key axes are independent.
    #[tokio::test]
    async fn reencrypt_for_dev_vm_filters_by_machine_encrypts_to_vm() {
        let vm_identity = age::x25519::Identity::generate();
        let vm_pubkey = vm_identity.to_public().to_string();

        let (_dir, host_identity_path, secrets_dir, target_rpi, _) = two_machine_fixture();

        let reencrypted =
            reencrypt_for_dev_vm(&host_identity_path, &secrets_dir, "rpi", &vm_pubkey)
                .await
                .unwrap();
        assert_eq!(reencrypted.len(), 1);
        assert_eq!(reencrypted[0].stem, "rpi_only");

        // VM key decrypts.
        let vm_id: Identity = vm_identity.to_string().expose_secret().parse().unwrap();
        let pt = decrypt_bytes(&vm_id, Path::new("rpi_only"), &reencrypted[0].ciphertext).unwrap();
        assert_eq!(pt.expose_secret().as_str(), "rpipayload");

        // rpi's own production key MUST NOT decrypt — the dev ciphertext
        // is for the VM only, even though [files] scope was rpi's.
        let rpi_id: Identity = target_rpi.to_string().expose_secret().parse().unwrap();
        assert!(decrypt_bytes(&rpi_id, Path::new("rpi_only"), &reencrypted[0].ciphertext).is_err());
    }

    /// Dev VM shadowing a machine that isn't declared anywhere errors with
    /// `UnknownMachine`, same as the production path. Caller decides
    /// whether to fall through (typical for dev) or propagate.
    #[tokio::test]
    async fn reencrypt_for_dev_vm_unknown_machine_errors() {
        let vm_identity = age::x25519::Identity::generate();
        let vm_pubkey = vm_identity.to_public().to_string();

        let (_dir, host_identity_path, secrets_dir, _) = write_host_and_secrets(&[]);
        std::fs::write(
            secrets_dir.join("lusid-secrets.toml"),
            r#"
[operators]
[machines]
[files]
"#,
        )
        .unwrap();

        let err = reencrypt_for_dev_vm(&host_identity_path, &secrets_dir, "undeclared", &vm_pubkey)
            .await
            .unwrap_err();
        assert!(matches!(
            err,
            ReencryptForMachineError::UnknownMachine { machine_id } if machine_id == "undeclared"
        ));
    }

    #[tokio::test]
    async fn reencrypt_for_dev_vm_rejects_malformed_vm_pubkey() {
        let (_dir, host_identity_path, secrets_dir, _, _) = two_machine_fixture();
        let err = reencrypt_for_dev_vm(&host_identity_path, &secrets_dir, "rpi", "not-a-key")
            .await
            .unwrap_err();
        assert!(matches!(err, ReencryptForMachineError::RecipientKey(_)));
    }
}
