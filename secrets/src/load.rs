//! Apply-time loading: one entry point from `(secrets_dir, identity_path,
//! guest_mode)` to a ready-to-use [`Secrets`] bundle. Wraps the lower-level
//! primitives ([`Identity::from_file`], [`Recipients::load`],
//! [`alias_for_identity`], [`decrypt_dir`] / [`decrypt_all`]) that consumers
//! used to chain by hand.

use std::path::Path;

use displaydoc::Display;
use thiserror::Error;
use tracing::{debug, info};

use crate::alias::alias_for_identity;
use crate::decrypt_all::{DecryptAllError, decrypt_all};
use crate::decrypt_dir::{DecryptDirError, decrypt_dir};
use crate::identity::{Identity, IdentityError};
use crate::recipients::{Recipients, RecipientsError};
use crate::secrets::Secrets;

impl Secrets {
    /// Load the decrypted secrets bundle for an apply invocation.
    ///
    /// Behaviour matrix on `(identity_path, guest_mode)`:
    ///
    /// - `(None, false)` — no identity, not in guest mode: returns an empty
    ///   bundle. Plans that reference `@core/secret` will fail later at apply
    ///   with a missing-secret error.
    /// - `(None, true)` — guest mode without an identity: [`LoadError::GuestModeWithoutIdentity`].
    /// - `(Some(_), false)` — host mode: reads `lusid-secrets.toml` from
    ///   `secrets_dir`, matches the identity to an alias, and decrypts the
    ///   subset of `*.age` files declared for that alias.
    /// - `(Some(_), true)` — guest mode: skips `lusid-secrets.toml` and
    ///   decrypts every `*.age` under `secrets_dir` with the single supplied
    ///   identity. Intended for `dev apply` / `remote apply` targets where
    ///   the host has already filtered ciphertexts to exactly the set this
    ///   guest should see.
    pub async fn load(
        secrets_dir: &Path,
        identity_path: Option<&Path>,
        guest_mode: bool,
    ) -> Result<Self, LoadError> {
        match (identity_path, guest_mode) {
            (None, true) => Err(LoadError::GuestModeWithoutIdentity),
            (None, false) => {
                debug!("no identity supplied; proceeding without secrets");
                Ok(Secrets::empty())
            }
            (Some(identity_path), true) => {
                info!(
                    identity = %identity_path.display(),
                    secrets_dir = %secrets_dir.display(),
                    "loading secrets (guest mode)",
                );
                let identity = Identity::from_file(identity_path).await?;
                let secrets = decrypt_all(&identity, secrets_dir).await?;
                info!(count = secrets.len(), "secrets loaded");
                Ok(secrets)
            }
            (Some(identity_path), false) => {
                info!(
                    identity = %identity_path.display(),
                    secrets_dir = %secrets_dir.display(),
                    "loading secrets",
                );
                let identity = Identity::from_file(identity_path).await?;
                let recipients = Recipients::load(secrets_dir).await?;
                let alias = alias_for_identity(&identity, &recipients)
                    .ok_or(LoadError::NoAliasForIdentity)?;
                let stems = recipients.files_for_alias(alias);
                debug!(alias, count = stems.len(), "alias matched");
                let secrets = decrypt_dir(&identity, secrets_dir, &stems).await?;
                info!(count = secrets.len(), "secrets loaded");
                Ok(secrets)
            }
        }
    }
}

#[derive(Debug, Error, Display)]
pub enum LoadError {
    /// {0}
    Identity(#[from] IdentityError),

    /// {0}
    Recipients(#[from] RecipientsError),

    /// {0}
    DecryptDir(#[from] DecryptDirError),

    /// {0}
    DecryptAll(#[from] DecryptAllError),

    /// supplied identity matches no alias in lusid-secrets.toml
    NoAliasForIdentity,

    /// guest mode requires an identity (none provided)
    GuestModeWithoutIdentity,
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use secrecy::ExposeSecret;
    use tempfile::TempDir;

    use super::*;
    use crate::crypto::encrypt_bytes;

    fn write_identity(dir: &Path, id: &age::x25519::Identity) -> std::path::PathBuf {
        let path = dir.join("identity");
        std::fs::write(&path, id.to_string().expose_secret()).unwrap();
        path
    }

    #[tokio::test]
    async fn no_identity_returns_empty() {
        let dir = TempDir::new().unwrap();
        let secrets = Secrets::load(dir.path(), None, false).await.unwrap();
        assert!(secrets.is_empty());
    }

    #[tokio::test]
    async fn guest_mode_without_identity_errors() {
        let dir = TempDir::new().unwrap();
        let err = Secrets::load(dir.path(), None, true).await.unwrap_err();
        assert!(matches!(err, LoadError::GuestModeWithoutIdentity));
    }

    #[tokio::test]
    async fn guest_mode_decrypts_every_age_file() {
        let id = age::x25519::Identity::generate();
        let dir = TempDir::new().unwrap();
        let identity_path = write_identity(dir.path(), &id);

        for (stem, value) in &[("a", b"aaaaaaaa" as &[u8]), ("b", b"bbbbbbbb")] {
            let ct = encrypt_bytes(&[Box::new(id.to_public())], Path::new(stem), value).unwrap();
            std::fs::write(dir.path().join(format!("{stem}.age")), &ct).unwrap();
        }

        let secrets = Secrets::load(dir.path(), Some(&identity_path), true)
            .await
            .unwrap();
        assert_eq!(secrets.len(), 2);
    }

    /// Operators are implicit recipients on every `[files]` entry, so a
    /// host-mode load with the operator identity decrypts every file —
    /// including ones whose `recipients` list doesn't name an operator
    /// (which it can't, under the current schema).
    #[tokio::test]
    async fn host_mode_decrypts_every_file_for_operator() {
        let id = age::x25519::Identity::generate();
        let pub_key = id.to_public().to_string();
        let dir = TempDir::new().unwrap();
        let identity_path = write_identity(dir.path(), &id);

        std::fs::write(
            dir.path().join("lusid-secrets.toml"),
            format!(
                r#"
[operators]
me = "{pub_key}"

[files]
"mine"      = {{ recipients = [] }}
"shared"    = {{ recipients = [] }}
"#
            ),
        )
        .unwrap();

        for (stem, value) in &[("mine", b"hunter22" as &[u8]), ("shared", b"shareme!")] {
            let ct = encrypt_bytes(&[Box::new(id.to_public())], Path::new(stem), value).unwrap();
            std::fs::write(dir.path().join(format!("{stem}.age")), &ct).unwrap();
        }

        let secrets = Secrets::load(dir.path(), Some(&identity_path), false)
            .await
            .unwrap();
        assert_eq!(secrets.len(), 2);
        assert_eq!(
            secrets.get("mine").unwrap().expose_secret().as_str(),
            "hunter22"
        );
    }

    #[tokio::test]
    async fn host_mode_unknown_identity_errors() {
        let id_listed = age::x25519::Identity::generate();
        let id_other = age::x25519::Identity::generate();
        let listed_pub = id_listed.to_public().to_string();
        let dir = TempDir::new().unwrap();
        let identity_path = write_identity(dir.path(), &id_other);

        std::fs::write(
            dir.path().join("lusid-secrets.toml"),
            format!(
                r#"
[operators]
listed = "{listed_pub}"

[files]
"x" = {{ recipients = [] }}
"#
            ),
        )
        .unwrap();

        let err = Secrets::load(dir.path(), Some(&identity_path), false)
            .await
            .unwrap_err();
        assert!(matches!(err, LoadError::NoAliasForIdentity));
    }
}
