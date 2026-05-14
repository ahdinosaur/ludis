use base64ct::LineEnding;
use lusid_fs::{self as fs, FsError};
use russh::keys::key::safe_rng;
use russh::keys::ssh_key::private::Ed25519Keypair;
use russh::keys::{PrivateKey, PublicKey};
use std::path::Path;
use thiserror::Error;
use tracing::debug;

#[derive(Error, Debug)]
pub enum SshKeypairError {
    #[error("filesystem error: {0}")]
    Fs(#[from] FsError),

    #[error("SSH key encode/decode error: {0}")]
    RusshKey(#[from] russh::keys::ssh_key::Error),
}

impl SshKeypairError {
    /// True iff the underlying ssh-key error indicates a
    /// passphrase-protected private key. Callers (e.g. `lusid`) branch on
    /// this to surface a "decrypt the key first" hint without taking a
    /// direct dependency on russh's error types.
    pub fn is_encrypted(&self) -> bool {
        matches!(
            self,
            SshKeypairError::RusshKey(russh::keys::ssh_key::Error::Encrypted)
        )
    }
}

#[derive(Clone, Debug)]
pub struct SshKeypair {
    pub public_key: PublicKey,
    pub private_key: PrivateKey,
}

const PRIVATE_KEY_FILE: &str = "id_ed25519";
const PUBLIC_KEY_FILE: &str = "id_ed25519.pub";

impl SshKeypair {
    /// Load an existing keypair if present, otherwise create and save a new one.
    #[tracing::instrument(skip_all)]
    pub async fn load_or_create(directory: &Path) -> Result<Self, SshKeypairError> {
        if Self::exists(directory).await? {
            debug!("SSH keypair exists; loading");
            return Self::load(directory).await;
        }

        debug!("SSH keypair doesn't exist, creating");
        let keypair = Self::create()?;
        keypair.save(directory).await?;
        Ok(keypair)
    }

    /// Create a new in-memory keypair.
    #[tracing::instrument(skip_all)]
    pub fn create() -> Result<Self, SshKeypairError> {
        let ed25519 = Ed25519Keypair::random(&mut safe_rng());
        let public_key = PublicKey::from(ed25519.public);
        let private_key = PrivateKey::from(ed25519);
        debug!("Created new SSH keypair");
        Ok(Self {
            public_key,
            private_key,
        })
    }

    /// Save the keypair as OpenSSH files.
    #[tracing::instrument(skip_all)]
    pub async fn save(&self, directory: &Path) -> Result<(), SshKeypairError> {
        fs::setup_directory_access(directory).await?;

        let public_key_path = directory.join(PUBLIC_KEY_FILE);
        let private_key_path = directory.join(PRIVATE_KEY_FILE);

        let public_key_string = self.public_key.to_openssh()?;
        let private_key_string = self.private_key.to_openssh(LineEnding::default())?;

        fs::write_file(&public_key_path, public_key_string.as_bytes()).await?;
        fs::write_file(&private_key_path, private_key_string.as_bytes()).await?;

        fs::change_mode(&private_key_path, 0o600).await?;

        debug!(
            public_key = %public_key_path.display(),
            private_key = %private_key_path.display(),
            "Saved SSH keypair"
        );

        Ok(())
    }

    /// Whether a keypair exists on disk in the directory.
    #[tracing::instrument(skip_all)]
    pub async fn exists(directory: &Path) -> Result<bool, SshKeypairError> {
        let public_key_path = directory.join(PUBLIC_KEY_FILE);
        let private_key_path = directory.join(PRIVATE_KEY_FILE);
        let public_key_exists = fs::path_exists(&public_key_path).await?;
        let private_key_exists = fs::path_exists(&private_key_path).await?;
        Ok(public_key_exists && private_key_exists)
    }

    /// Public key as a single-line OpenSSH string (`ssh-ed25519 AAAA...`).
    /// Suitable for parsing into an age recipient or pushing into
    /// `authorized_keys`.
    pub fn public_openssh(&self) -> Result<String, SshKeypairError> {
        Ok(self.public_key.to_openssh()?)
    }

    /// Private key as a multi-line OpenSSH PEM string. Use [`SshKeypair::save`]
    /// to write to disk with the right mode; this getter exists for callers
    /// that need to ship the bytes elsewhere (e.g. SFTP an identity to a
    /// dev VM).
    pub fn private_openssh(&self) -> Result<String, SshKeypairError> {
        Ok(self
            .private_key
            .to_openssh(LineEnding::default())?
            .to_string())
    }

    /// Load a keypair from the directory.
    #[tracing::instrument(skip_all)]
    pub async fn load(directory: &Path) -> Result<Self, SshKeypairError> {
        let public_key_path = directory.join(PUBLIC_KEY_FILE);
        let private_key_path = directory.join(PRIVATE_KEY_FILE);

        let public_key_string = fs::read_file_to_string(&public_key_path).await?;
        let private_key_string = fs::read_file_to_string(&private_key_path).await?;

        let public_key = PublicKey::from_openssh(&public_key_string)?;
        let private_key = PrivateKey::from_openssh(&private_key_string)?;

        debug!(
            public_key = %public_key_path.display(),
            private_key = %private_key_path.display(),
            "Loaded SSH keypair"
        );

        Ok(Self {
            public_key,
            private_key,
        })
    }
}

/// Load just the OpenSSH private key from a file path. Use when only the
/// private key is needed (e.g. SSH-client auth where the matching `.pub`
/// isn't on disk in the conventional location). Distinct from
/// [`SshKeypair::load`] which expects both `id_ed25519` and `id_ed25519.pub`
/// in the same directory.
#[tracing::instrument(skip_all, fields(path = %path.display()))]
pub async fn load_private_key(path: &Path) -> Result<PrivateKey, SshKeypairError> {
    let private_key_string = fs::read_file_to_string(path).await?;
    let private_key = PrivateKey::from_openssh(&private_key_string)?;
    Ok(private_key)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn load_private_key_round_trips_saved_keypair() {
        let dir = tempfile::TempDir::new().unwrap();
        let saved = SshKeypair::create().unwrap();
        saved.save(dir.path()).await.unwrap();

        let loaded = load_private_key(&dir.path().join(PRIVATE_KEY_FILE))
            .await
            .unwrap();

        // Compare via canonical OpenSSH serialization — `PrivateKey` itself
        // isn't `PartialEq`, and round-tripping the encoded form is the
        // contract callers depend on anyway.
        let saved_pem = saved
            .private_key
            .to_openssh(LineEnding::default())
            .unwrap()
            .to_string();
        let loaded_pem = loaded
            .to_openssh(LineEnding::default())
            .unwrap()
            .to_string();
        assert_eq!(saved_pem, loaded_pem);
    }

    #[tokio::test]
    async fn load_private_key_errors_on_garbage() {
        let dir = tempfile::TempDir::new().unwrap();
        let path = dir.path().join("garbage");
        tokio::fs::write(&path, b"not an openssh key")
            .await
            .unwrap();
        let err = load_private_key(&path).await.unwrap_err();
        assert!(matches!(err, SshKeypairError::RusshKey(_)));
        // Garbage bytes are NOT a passphrase-protected key; the classifier
        // must say no so callers don't surface a misleading "decrypt with
        // ssh-keygen -p" hint.
        assert!(!err.is_encrypted());
    }

    #[test]
    fn is_encrypted_true_for_encrypted_variant() {
        let err: SshKeypairError = russh::keys::ssh_key::Error::Encrypted.into();
        assert!(err.is_encrypted());
    }
}
