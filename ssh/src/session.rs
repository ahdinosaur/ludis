//! Async SSH channel and session abstractions.
//!
//! Provides:
//! - AsyncSession: thin wrapper around russh::client::Handle with convenience
//!   connect and open_channel methods.
//! - AsyncChannel: wrapper around russh::Channel with async stdout/stderr
//!   streams, stdin writer, and event promises (success/failure, EOF, exit
//!   status). Also exposes a wait_close() method.

use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::path::PathBuf;
use std::sync::Arc;

use async_promise::Promise;
use bytes::Bytes;
use russh::client::{Config, Handle, Handler, Msg, connect};
use russh::keys::{PrivateKey, PrivateKeyWithHashAlg, ssh_key};
use russh::{ChannelMsg, ChannelWriteHalf, Error as SshError};
use tokio::io::AsyncWrite;
use tokio::net::ToSocketAddrs;
use tokio::sync::mpsc;
use tokio::task::JoinHandle;
use tracing::Instrument;

use crate::stream::ReadStream;

/// How to verify the remote server's host key during the SSH handshake.
///
/// Stored on [`SshConnectOptions`](crate::SshConnectOptions); the
/// [`HostKeyHandler`] picks the matching arm in `check_server_key`.
#[derive(Debug, Clone)]
pub enum HostKeyVerification {
    /// Skip verification entirely.
    ///
    /// Only safe for ephemeral targets whose host key is locally generated
    /// and not reused — e.g. a dev VM we just booted with a fresh keypair,
    /// or a CI sandbox. **Never** use against real remote infrastructure;
    /// the connection becomes trivially MITM-able.
    Disabled,
    /// Trust-on-first-use against an OpenSSH-format `known_hosts` file.
    ///
    /// On first connection the server's pubkey is appended to
    /// `known_hosts_path`. On subsequent connections an exact match is
    /// required; mismatch surfaces as [`russh::Error::KeyChanged`] (the
    /// standard OpenSSH "REMOTE HOST IDENTIFICATION HAS CHANGED" failure).
    Tofu {
        host: String,
        port: u16,
        known_hosts_path: PathBuf,
    },
}

/// russh [`Handler`] enforcing a configured [`HostKeyVerification`] strategy.
///
/// The handler is `Clone` so it can be rebuilt per attempt inside a retry
/// loop without restating the policy.
#[derive(Clone)]
pub struct HostKeyHandler {
    verification: HostKeyVerification,
}

impl HostKeyHandler {
    pub fn new(verification: HostKeyVerification) -> Self {
        Self { verification }
    }
}

impl Handler for HostKeyHandler {
    type Error = SshError;

    async fn check_server_key(
        &mut self,
        server_public_key: &ssh_key::PublicKey,
    ) -> Result<bool, Self::Error> {
        let HostKeyVerification::Tofu {
            host,
            port,
            known_hosts_path,
        } = &self.verification
        else {
            return Ok(true);
        };

        match russh::keys::check_known_hosts_path(host, *port, server_public_key, known_hosts_path)
        {
            // Known host with matching key.
            Ok(true) => Ok(true),
            // Host not in `known_hosts` — trust on first use and record it.
            Ok(false) => {
                russh::keys::known_hosts::learn_known_hosts_path(
                    host,
                    *port,
                    server_public_key,
                    known_hosts_path,
                )
                .map_err(SshError::Keys)?;
                tracing::warn!(
                    host = %host,
                    port = port,
                    known_hosts = %known_hosts_path.display(),
                    "added host key to known_hosts on first connection (trust-on-first-use)"
                );
                Ok(true)
            }
            // Known host but key differs — refuse and surface loudly.
            Err(err @ russh::keys::Error::KeyChanged { line }) => {
                tracing::error!(
                    host = %host,
                    port = port,
                    line = line,
                    known_hosts = %known_hosts_path.display(),
                    "REMOTE HOST IDENTIFICATION HAS CHANGED — possible MITM, \
                     or the host key was rotated; remove the offending line \
                     from `known_hosts` if the change is expected"
                );
                Err(SshError::Keys(err))
            }
            Err(err) => Err(SshError::Keys(err)),
        }
    }
}

/// An SSH session that can open multiple AsyncChannels.
///
/// Implements Deref to the underlying russh::client::Handle.
pub struct AsyncSession<H: Handler> {
    session: Handle<H>,
}

impl<H: 'static + Handler> AsyncSession<H> {
    /// Connect to an SSH server using the provided configuration and handler,
    /// without beginning authentication.
    pub async fn connect(
        config: Arc<Config>,
        addrs: impl ToSocketAddrs,
        handler: H,
    ) -> Result<Self, H::Error> {
        let session = connect(config, addrs, handler).await?;
        Ok(Self { session })
    }

    /// Open an asynchronous channel in this session.
    pub async fn open_channel(&self) -> Result<AsyncChannel, SshError> {
        let russh_channel = self.session.channel_open_session().await?;
        Ok(AsyncChannel::from(russh_channel))
    }
}

impl AsyncSession<HostKeyHandler> {
    /// Connect and authenticate with the given user and key_path via public key.
    pub async fn auth_publickey(
        &mut self,
        username: impl AsRef<str>,
        private_key: PrivateKey,
    ) -> Result<(), SshError> {
        let hash_alg = self.best_supported_rsa_hash().await?.flatten();
        let auth = self
            .authenticate_publickey(
                username.as_ref(),
                PrivateKeyWithHashAlg::new(Arc::new(private_key), hash_alg),
            )
            .await?;

        if !auth.success() {
            tracing::warn!("SSH authentication failed");
            return Err(SshError::NotAuthenticated);
        }

        tracing::info!("SSH authentication successful");
        Ok(())
    }
}

impl<H: Handler> Deref for AsyncSession<H> {
    type Target = Handle<H>;
    fn deref(&self) -> &Self::Target {
        &self.session
    }
}

impl<H: Handler> DerefMut for AsyncSession<H> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.session
    }
}

/// An asynchronous SSH channel with ReadStream stdout/stderr, AsyncWrite stdin,
/// and event promises for exec success/failure, EOF, and exit status.
///
/// Implements Deref to the underlying ChannelWriteHalf.
pub struct AsyncChannel {
    write_half: ChannelWriteHalf<Msg>,
    subscribe_send: mpsc::UnboundedSender<(Option<u32>, mpsc::UnboundedSender<Bytes>)>,
    success_failure: Promise<bool>,
    eof: Promise<()>,
    exit_status: Promise<u32>,
    reader: JoinHandle<()>,
}

impl From<russh::Channel<Msg>> for AsyncChannel {
    fn from(inner: russh::Channel<Msg>) -> Self {
        let (mut read_half, write_half) = inner.split();
        let (mut resolve_success_failure, success_failure) = async_promise::channel();
        let (mut resolve_eof, eof) = async_promise::channel();
        let (mut resolve_exit_status, exit_status) = async_promise::channel();
        let (subscribe_send, mut subscribe_recv) = mpsc::unbounded_channel();

        let reader = async move {
            // Map from `ext` to a sender for Bytess of data.
            type Subscribers = HashMap<Option<u32>, mpsc::UnboundedSender<Bytes>>;
            let mut subscribers = Some(Subscribers::new());

            #[tracing::instrument(level = "INFO", skip_all, fields(?ext))]
            fn receive_data(subscribers: &Option<Subscribers>, ext: Option<u32>, data: Bytes) {
                if let Some(subscribers) = subscribers {
                    if let Some(send) = subscribers.get(&ext) {
                        if let Err(e) = send.send(data) {
                            tracing::warn!("Failed to send data to subscriber: {e}");
                        } else {
                            tracing::debug!("Successfully sent data to subscriber.");
                        }
                    } else {
                        tracing::debug!("No subscriber for ext, dropping data.");
                    }
                } else {
                    tracing::warn!("Unexpectedly received data from server after receiving EOF.");
                }
            }

            loop {
                tokio::select! {
                    biased;

                    Some((ext, send)) = subscribe_recv.recv() => {
                        if let Some(subscribers) = &mut subscribers {
                            subscribers.insert(ext, send);
                        } else {
                            tracing::debug!(ext, "Received stream subscriber after EOF, ignoring.");
                        }
                    },

                    opt_msg = read_half.wait() => {
                        let Some(msg) = opt_msg else {
                            break;
                        };

                        tracing::info_span!("Message", ?msg).in_scope(|| {
                            match msg {
                                ChannelMsg::Data { data } => {
                                    receive_data(&subscribers, None, data)
                                }
                                ChannelMsg::ExtendedData { data, ext } => {
                                    receive_data(&subscribers, Some(ext), data)
                                }
                                ChannelMsg::Success | ChannelMsg::Failure => {
                                    tracing::debug!("Resolving success/failure.");
                                    let is_success = matches!(msg, ChannelMsg::Success);
                                    if resolve_success_failure.resolve(is_success).is_err() {
                                        tracing::warn!(
                                            "Success/failure already resolved, ignoring."
                                        );
                                    }
                                }
                                ChannelMsg::Eof => {
                                    tracing::debug!(
                                        "Resolving EOF and dropping stream subscribers."
                                    );
                                    if resolve_eof.resolve(()).is_err() {
                                        tracing::warn!("EOF already resolved, ignoring.");
                                    }
                                    drop(std::mem::take(&mut subscribers));
                                }
                                ChannelMsg::ExitStatus { exit_status } => {
                                    tracing::debug!(exit_status, "Resolving exit status.");
                                    if resolve_exit_status.resolve(exit_status).is_err() {
                                        tracing::warn!(
                                            "Exit status already resolved, ignoring."
                                        );
                                    }
                                }
                                _ => {
                                    tracing::trace!("Ignoring message.");
                                }
                            }
                        });
                    },
                }
            }

            tracing::debug!("Channel read half finished, reader exiting.");
        };

        let reader = tokio::task::spawn(reader.instrument(tracing::info_span!("Reader")));

        Self {
            write_half,
            subscribe_send,
            success_failure,
            eof,
            exit_status,
            reader,
        }
    }
}

impl AsyncChannel {
    /// Returns the specified stream as a ReadStream.
    ///
    /// Call this before exec so output isn't missed. Re-calling for the same
    /// ext replaces the previous subscriber.
    pub fn read_stream(&self, ext: Option<u32>) -> ReadStream {
        let (send, recv) = mpsc::unbounded_channel();
        let _ = self.subscribe_send.send((ext, send));
        ReadStream::from_recv(recv)
    }

    /// Returns stdout as a ReadStream.
    pub fn stdout(&self) -> ReadStream {
        self.read_stream(None)
    }

    /// Returns stderr as a ReadStream.
    pub fn stderr(&self) -> ReadStream {
        self.read_stream(Some(1))
    }

    /// Returns the specified stream as an AsyncWrite.
    pub fn write_stream(&self, ext: Option<u32>) -> impl AsyncWrite + 'static {
        self.write_half.make_writer_ext(ext)
    }

    /// Returns stdin as an AsyncWrite.
    pub fn stdin(&self) -> impl AsyncWrite + 'static {
        self.write_stream(None)
    }

    /// Resolves when success or failure has been received.
    pub fn recv_success_failure(&self) -> &Promise<bool> {
        &self.success_failure
    }

    /// Resolves when EOF has been received (no more stdout/stderr).
    pub fn recv_eof(&self) -> &Promise<()> {
        &self.eof
    }

    /// Resolves when the command exit status has been received.
    pub fn recv_exit_status(&self) -> &Promise<u32> {
        &self.exit_status
    }

    /// Await until the channel has been closed.
    pub async fn wait_close(&mut self) {
        let _ = (&mut self.reader).await;
    }

    /// Whether the channel has been closed.
    pub fn is_closed(&self) -> bool {
        self.reader.is_finished()
    }

    /// Change the window size
    pub async fn window_change(
        &self,
        col_width: u32,
        row_height: u32,
        pix_width: u32,
        pix_height: u32,
    ) -> Result<(), SshError> {
        self.write_half
            .window_change(col_width, row_height, pix_width, pix_height)
            .await
    }
}

impl Deref for AsyncChannel {
    type Target = ChannelWriteHalf<Msg>;
    fn deref(&self) -> &Self::Target {
        &self.write_half
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Two stable ed25519 host pubkeys (OpenSSH format). Distinct so the
    /// mismatch test reliably triggers `KeyChanged`.
    const HOSTKEY_A: &str =
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJdD7y3aLq454yWBdwLWbieU1ebz9/cu7/QEXn9OIeZJ";
    const HOSTKEY_B: &str =
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILIG2T/B0l0gaqj3puu510tu9N1OkQ4znY3LYuEm5zCF";

    fn pubkey(openssh: &str) -> ssh_key::PublicKey {
        ssh_key::PublicKey::from_openssh(openssh).expect("static fixture parses")
    }

    fn handler(known_hosts_path: PathBuf) -> HostKeyHandler {
        HostKeyHandler::new(HostKeyVerification::Tofu {
            host: "test.example".to_owned(),
            port: 22,
            known_hosts_path,
        })
    }

    #[tokio::test]
    async fn disabled_accepts_anything() {
        let mut h = HostKeyHandler::new(HostKeyVerification::Disabled);
        assert!(h.check_server_key(&pubkey(HOSTKEY_A)).await.unwrap());
    }

    #[tokio::test]
    async fn tofu_first_connection_writes_known_hosts() {
        let dir = tempfile::TempDir::new().unwrap();
        let path = dir.path().join("known_hosts");
        let mut h = handler(path.clone());

        assert!(h.check_server_key(&pubkey(HOSTKEY_A)).await.unwrap());

        let contents = std::fs::read_to_string(&path).unwrap();
        assert!(
            contents.contains("test.example") && contents.contains(HOSTKEY_A),
            "expected TOFU to record host+key; got: {contents:?}"
        );
    }

    #[tokio::test]
    async fn tofu_returning_connection_accepts_matching_key() {
        let dir = tempfile::TempDir::new().unwrap();
        let path = dir.path().join("known_hosts");
        let mut h = handler(path.clone());

        // Seed via first call, then second call must match without rewriting.
        h.check_server_key(&pubkey(HOSTKEY_A)).await.unwrap();
        let before = std::fs::read_to_string(&path).unwrap();
        assert!(h.check_server_key(&pubkey(HOSTKEY_A)).await.unwrap());
        let after = std::fs::read_to_string(&path).unwrap();
        assert_eq!(before, after, "second connection must not append");
    }

    #[tokio::test]
    async fn tofu_rejects_changed_key() {
        let dir = tempfile::TempDir::new().unwrap();
        let path = dir.path().join("known_hosts");
        let mut h = handler(path);

        h.check_server_key(&pubkey(HOSTKEY_A)).await.unwrap();
        let err = h.check_server_key(&pubkey(HOSTKEY_B)).await.unwrap_err();
        assert!(
            matches!(err, SshError::Keys(russh::keys::Error::KeyChanged { .. })),
            "expected KeyChanged on host-key mismatch, got: {err:?}"
        );
    }
}
