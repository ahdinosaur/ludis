//! Async SSH client for lusid's VM provisioning pipeline.
//!
//! Built on [`russh`]. Provides:
//!
//! - [`Ssh::connect`] — connect with retry + public key auth.
//! - [`Ssh::command`] — run a remote command and tail stdout/stderr as
//!   [`tokio::io::AsyncRead`] streams.
//! - [`Ssh::sync`] — SFTP a local file / directory / bytes onto the remote.
//! - [`Ssh::terminal`] — forward the current TTY to an interactive remote shell.
//! - [`SshKeypair`] — create / load an ed25519 keypair on disk.
//!
//! Host-key verification is configured per-connection via
//! [`HostKeyVerification`] on [`SshConnectOptions`]: real remote machines use
//! [`HostKeyVerification::Tofu`] against a `known_hosts` file (standard
//! OpenSSH trust-on-first-use), while ephemeral targets (dev VMs the caller
//! has just booted with a fresh keypair) opt into
//! [`HostKeyVerification::Disabled`].

mod command;
mod connect;
mod keypair;
mod session;
mod stream;
mod sync;
mod terminal;

pub use crate::command::{SshCommandError, SshCommandHandle};
pub use crate::connect::{SshConnectError, SshConnectOptions};
pub use crate::keypair::{SshKeypair, SshKeypairError, load_private_key};
pub use crate::session::HostKeyVerification;
pub use crate::sync::{SshSyncError, SshVolume};
pub use crate::terminal::SshTerminalError;

use thiserror::Error;
use tokio::net::ToSocketAddrs;

use crate::connect::connect_with_retry;
use crate::session::{AsyncSession, HostKeyHandler};

type Session = AsyncSession<HostKeyHandler>;

#[derive(Error, Debug)]
pub enum SshError {
    #[error(transparent)]
    Connect(#[from] SshConnectError),

    #[error(transparent)]
    Command(#[from] SshCommandError),

    #[error(transparent)]
    Terminal(#[from] SshTerminalError),

    #[error(transparent)]
    Sync(#[from] SshSyncError),

    #[error(transparent)]
    Keypair(#[from] SshKeypairError),

    #[error("failed to disconnect: {error}")]
    Disconnect {
        #[source]
        error: russh::Error,
    },
}

/// High-level SSH client built on the async channel/session abstractions.
pub struct Ssh {
    session: Session,
}

impl Ssh {
    /// Connect to the SSH server with retry/backoff and public key auth.
    #[tracing::instrument(skip(options))]
    pub async fn connect<Addrs>(options: SshConnectOptions<Addrs>) -> Result<Self, SshError>
    where
        Addrs: ToSocketAddrs + Clone + Send,
    {
        let session = connect_with_retry(options).await?;
        Ok(Self { session })
    }

    /// Execute a remote command and get a streaming handle.
    #[tracing::instrument(skip(self))]
    pub async fn command(&mut self, command: &str) -> Result<SshCommandHandle, SshError> {
        command::ssh_command(&self.session, command)
            .await
            .map_err(SshError::Command)
    }

    /// Synchronize a volume (directory, file, or raw bytes) via SFTP.
    #[tracing::instrument(skip(self))]
    pub async fn sync(&mut self, volume: SshVolume) -> Result<(), SshError> {
        sync::ssh_sync(&self.session, volume)
            .await
            .map_err(SshError::Sync)
    }

    /// Synchronize a volume (directory, file, or raw bytes) via SFTP.
    #[tracing::instrument(skip(self))]
    pub async fn terminal(&mut self) -> Result<Option<u32>, SshError> {
        terminal::ssh_terminal(&self.session)
            .await
            .map_err(SshError::Terminal)
    }

    /// Disconnect the SSH session.
    #[tracing::instrument(skip(self))]
    pub async fn disconnect(&mut self) -> Result<(), SshError> {
        self.session
            .disconnect(russh::Disconnect::ByApplication, "", "English")
            .await
            .map_err(|error| SshError::Disconnect { error })
    }
}
