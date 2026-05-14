use std::sync::Arc;
use std::time::Duration;

use russh::client::Config;
use russh::keys::PrivateKey;
use thiserror::Error;
use tokio::net::ToSocketAddrs;
use tokio::time::{Instant, sleep};

use crate::session::{AsyncSession, HostKeyHandler, HostKeyVerification};

#[derive(Debug, Clone)]
pub struct SshConnectOptions<Addrs>
where
    Addrs: ToSocketAddrs + Clone + Send,
{
    pub private_key: PrivateKey,
    pub addrs: Addrs,
    pub username: String,
    pub config: Arc<Config>,
    pub timeout: Duration,
    /// Strategy for verifying the server's host key. Pick
    /// [`HostKeyVerification::Tofu`] for real remote machines and
    /// [`HostKeyVerification::Disabled`] only for ephemeral local targets
    /// (e.g. dev VMs the caller has just booted).
    pub host_key_verification: HostKeyVerification,
}

#[derive(Error, Debug)]
pub enum SshConnectError {
    #[error("timed out connecting to SSH server")]
    Timeout,

    #[error("SSH protocol error: {0}")]
    Russh(#[from] russh::Error),
}

/// Connect with retry/backoff using the AsyncSession abstraction.
///
/// - Retries transient IO errors until timeout is exceeded.
/// - Authenticates via public key.
/// - Verifies the host key according to
///   [`SshConnectOptions::host_key_verification`].
#[tracing::instrument(skip(options))]
pub(super) async fn connect_with_retry<Addrs>(
    options: SshConnectOptions<Addrs>,
) -> Result<AsyncSession<HostKeyHandler>, SshConnectError>
where
    Addrs: ToSocketAddrs + Clone + Send,
{
    let SshConnectOptions {
        private_key,
        addrs,
        username,
        config,
        timeout,
        host_key_verification,
    } = options;

    let start = Instant::now();
    tracing::info!("Connecting to SSH");

    let mut session = loop {
        let handler = HostKeyHandler::new(host_key_verification.clone());
        match AsyncSession::connect(config.clone(), addrs.clone(), handler).await {
            Ok(session) => {
                tracing::trace!("SSH transport established");
                break session;
            }
            Err(russh::Error::IO(ref error))
                if matches!(
                    error.kind(),
                    std::io::ErrorKind::TimedOut
                        | std::io::ErrorKind::ConnectionRefused
                        | std::io::ErrorKind::ConnectionReset
                        | std::io::ErrorKind::NotFound
                ) =>
            {
                if start.elapsed() > timeout {
                    tracing::warn!("Connect retry timeout exceeded");
                    return Err(SshConnectError::Timeout);
                }
                tracing::debug!(
                    err = %error,
                    elapsed_ms = start.elapsed().as_millis(),
                    "SSH transport not ready; will retry"
                );
            }
            Err(error) => {
                tracing::warn!(err = %error, "Non-retryable SSH error");
                return Err(SshConnectError::Russh(error));
            }
        }

        sleep(Duration::from_millis(100)).await;
    };

    tracing::debug!(username = %username, "Authenticating over SSH");

    session.auth_publickey(username, private_key).await?;

    Ok(session)
}
