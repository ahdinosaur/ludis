use base64ct::LineEnding;
use lusid_fs::{self as fs, FsError};
use russh::keys::ssh_key::{private::Ed25519Keypair, rand_core::OsRng};
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
        let ed25519 = Ed25519Keypair::random(&mut OsRng);
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
