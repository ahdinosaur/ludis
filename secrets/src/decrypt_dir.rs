//! Decrypt a specific subset of `*.age` files from a directory.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use displaydoc::Display;
use thiserror::Error;
use tokio::fs;

use crate::crypto::{DecryptError, decrypt_bytes};
use crate::identity::Identity;
use crate::secrets::{Secret, Secrets};

/// Decrypt the named `*.age` files under `secrets_dir` with `identity`.
///
/// `file_stems` is the subset of secrets the caller wants to read - typically
/// the result of [`crate::Recipients::files_for_alias`] for the alias matching
/// the host identity. Files outside this list are not opened. Each stem maps
/// to `<secrets_dir>/<stem>.age`; a missing file or a decrypt failure is fatal
/// (no silent fallback to an empty bundle).
///
/// An empty `file_stems` returns an empty [`Secrets`] without touching the
/// filesystem.
#[tracing::instrument(skip(identity, file_stems), fields(dir = %secrets_dir.display(), count = file_stems.len()))]
pub async fn decrypt_dir(
    identity: &Identity,
    secrets_dir: &Path,
    file_stems: &[&str],
) -> Result<Secrets, DecryptDirError> {
    if file_stems.is_empty() {
        return Ok(Secrets::empty());
    }

    let mut values: HashMap<String, Secret> = HashMap::with_capacity(file_stems.len());
    for stem in file_stems {
        let path = secrets_dir.join(format!("{stem}.age"));
        let ciphertext = fs::read(&path)
            .await
            .map_err(|source| DecryptDirError::ReadFile {
                path: path.clone(),
                source,
            })?;
        let plaintext = decrypt_bytes(identity, &path, &ciphertext)?;
        values.insert((*stem).to_owned(), plaintext);
    }

    tracing::info!(count = values.len(), "decrypted secrets");
    Ok(Secrets::from_values(values))
}

#[derive(Debug, Error, Display)]
pub enum DecryptDirError {
    /// Failed to read encrypted file {path}: {source}
    ReadFile {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// {0}
    Decrypt(#[from] DecryptError),
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use std::str::FromStr;

    use secrecy::ExposeSecret;
    use tempfile::TempDir;

    use super::*;
    use crate::crypto::encrypt_bytes;
    use crate::test_fixtures::{TEST_SSH_ED25519_A_PRIV, TEST_SSH_ED25519_A_PUB};

    fn ssh_recipient(pubkey: &str) -> Box<dyn age::Recipient + Send> {
        let mut parts = pubkey.split_whitespace();
        let kind = parts.next().unwrap();
        let body = parts.next().unwrap();
        Box::new(age::ssh::Recipient::from_str(&format!("{kind} {body}")).unwrap())
    }

    #[tokio::test]
    async fn round_trips() {
        let identity: Identity = TEST_SSH_ED25519_A_PRIV.parse().unwrap();
        let ct = encrypt_bytes(
            &[ssh_recipient(TEST_SSH_ED25519_A_PUB)],
            Path::new("hello"),
            b"world",
        )
        .unwrap();

        let dir = TempDir::new().unwrap();
        std::fs::write(dir.path().join("hello.age"), &ct).unwrap();

        let secrets = decrypt_dir(&identity, dir.path(), &["hello"])
            .await
            .unwrap();
        assert_eq!(secrets.len(), 1);
        assert_eq!(
            secrets.get("hello").unwrap().expose_secret().as_str(),
            "world"
        );
    }

    #[tokio::test]
    async fn empty_stems_returns_empty() {
        let identity: Identity = TEST_SSH_ED25519_A_PRIV.parse().unwrap();
        let secrets = decrypt_dir(&identity, Path::new("/nonexistent"), &[])
            .await
            .unwrap();
        assert!(secrets.is_empty());
    }

    #[tokio::test]
    async fn missing_file_errors() {
        let identity: Identity = TEST_SSH_ED25519_A_PRIV.parse().unwrap();
        let dir = TempDir::new().unwrap();
        let err = decrypt_dir(&identity, dir.path(), &["absent"])
            .await
            .unwrap_err();
        assert!(matches!(err, DecryptDirError::ReadFile { .. }));
    }
}
