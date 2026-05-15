//! Decrypt every `*.age` file in a directory, ignoring `lusid-secrets.toml`.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use displaydoc::Display;
use thiserror::Error;
use tokio::fs;

use crate::crypto::{DecryptError, decrypt_bytes};
use crate::identity::Identity;
use crate::secrets::{Secret, Secrets};

/// Decrypt every `*.age` file directly under `secrets_dir` with `identity`,
/// returning a [`Secrets`] keyed by filename stem.
///
/// Unlike [`crate::decrypt_dir`], this does not consult `lusid-secrets.toml` -
/// it decrypts whatever ciphertexts happen to be in the directory. Used on
/// guest-mode applies (dev / remote re-encryption targets) where the host
/// has already filtered the set of files to exactly what this target should
/// see, and there's no `Recipients` config on the guest.
///
/// Missing `secrets_dir` returns an empty [`Secrets`]. Non-`.age` files are
/// ignored.
#[tracing::instrument(skip(identity), fields(dir = %secrets_dir.display()))]
pub async fn decrypt_all(
    identity: &Identity,
    secrets_dir: &Path,
) -> Result<Secrets, DecryptAllError> {
    if !fs::try_exists(secrets_dir)
        .await
        .map_err(|source| DecryptAllError::ScanDir {
            dir: secrets_dir.to_path_buf(),
            source,
        })?
    {
        tracing::debug!("secrets dir does not exist; returning empty Secrets");
        return Ok(Secrets::empty());
    }

    let mut values: HashMap<String, Secret> = HashMap::new();
    let mut read_dir =
        fs::read_dir(secrets_dir)
            .await
            .map_err(|source| DecryptAllError::ScanDir {
                dir: secrets_dir.to_path_buf(),
                source,
            })?;

    while let Some(entry) =
        read_dir
            .next_entry()
            .await
            .map_err(|source| DecryptAllError::ScanDir {
                dir: secrets_dir.to_path_buf(),
                source,
            })?
    {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("age") {
            continue;
        }
        let Some(stem) = path.file_stem().and_then(|s| s.to_str()).map(str::to_owned) else {
            continue;
        };

        let ciphertext = fs::read(&path)
            .await
            .map_err(|source| DecryptAllError::ReadFile {
                path: path.clone(),
                source,
            })?;

        let plaintext = decrypt_bytes(identity, &path, &ciphertext)?;
        values.insert(stem, plaintext);
    }

    tracing::info!(count = values.len(), "decrypted secrets");
    Ok(Secrets::from_values(values))
}

#[derive(Debug, Error, Display)]
pub enum DecryptAllError {
    /// Failed to scan secrets dir {dir}: {source}
    ScanDir {
        dir: PathBuf,
        #[source]
        source: std::io::Error,
    },

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

    use secrecy::ExposeSecret;
    use tempfile::TempDir;

    use super::*;
    use crate::crypto::encrypt_bytes;

    #[tokio::test]
    async fn reads_every_age_file() {
        let id = age::x25519::Identity::generate();
        let identity: Identity = id.to_string().expose_secret().parse().unwrap();

        let dir = TempDir::new().unwrap();
        for (stem, value) in &[("a", b"aaa" as &[u8]), ("b", b"bbb")] {
            let ct = encrypt_bytes(&[Box::new(id.to_public())], Path::new(stem), value).unwrap();
            std::fs::write(dir.path().join(format!("{stem}.age")), &ct).unwrap();
        }
        // Non-.age entries alongside the ciphertexts are ignored.
        std::fs::write(dir.path().join("lusid-secrets.toml"), "# ignored").unwrap();

        let secrets = decrypt_all(&identity, dir.path()).await.unwrap();
        assert_eq!(secrets.len(), 2);
        assert_eq!(secrets.get("a").unwrap().expose_secret().as_str(), "aaa");
        assert_eq!(secrets.get("b").unwrap().expose_secret().as_str(), "bbb");
    }

    #[tokio::test]
    async fn missing_dir_returns_empty() {
        let id = age::x25519::Identity::generate();
        let identity: Identity = id.to_string().expose_secret().parse().unwrap();
        let secrets = decrypt_all(&identity, Path::new("/nonexistent-lusid-dir"))
            .await
            .unwrap();
        assert!(secrets.is_empty());
    }
}
