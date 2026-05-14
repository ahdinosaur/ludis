use async_promise::Promise;
use thiserror::Error;
use tokio::io::AsyncWrite;
use tracing::info;

use crate::SshError;
use crate::session::{AsyncChannel, AsyncSession, HostKeyHandler};
use crate::stream::ReadStream;

/// Command execution specific errors.
#[derive(Error, Debug)]
pub enum SshCommandError {
    #[error("failed to open SSH session channel: {0}")]
    ChannelOpen(#[source] russh::Error),

    #[error("failed to execute remote command `{command}`: {source}")]
    Exec {
        command: String,
        #[source]
        source: russh::Error,
    },

    #[error("SSH protocol error: {0}")]
    Russh(#[from] russh::Error),
}

pub struct SshChannelHandle {
    channel: AsyncChannel,
}

impl SshChannelHandle {
    /// Obtain a writer for the command's stdin.
    pub fn stdin(&self) -> impl AsyncWrite + 'static {
        self.channel.stdin()
    }

    /// Promise that resolves to the remote exit code when received.
    pub fn exit_code(&self) -> &Promise<u32> {
        self.channel.recv_exit_status()
    }

    /// Promise that resolves when EOF is received for stdout/stderr.
    pub fn eof(&self) -> &Promise<()> {
        self.channel.recv_eof()
    }

    /// Promise that resolves when the server replies Success/Failure to exec.
    pub fn success_failure(&self) -> &Promise<bool> {
        self.channel.recv_success_failure()
    }

    /// Close the channel cleanly and wait for it to be closed, returning exit
    /// code if received.
    #[tracing::instrument(skip(self))]
    pub async fn wait(&mut self) -> Result<Option<u32>, SshError> {
        let exit_code = self.exit_code().wait().await.copied();

        if !self.channel.is_closed() {
            self.channel
                .close()
                .await
                .map_err(SshCommandError::Russh)
                .map_err(SshError::Command)?;
            self.channel.wait_close().await;
        }

        info!(exit_code = exit_code, "Remote command completed");

        Ok(exit_code)
    }
}

/// A streaming handle to a running SSH command.
///
/// - stdout/stderr are AsyncBufRead (and AsyncRead) via ReadStream.
/// - stdin is available via stdin().
/// - exit code and other events exposed as Promises.
/// - call wait() to await completion and get the exit code.
pub struct SshCommandHandle {
    pub stdout: ReadStream,
    pub stderr: ReadStream,
    pub channel: SshChannelHandle,
    pub command: String,
}

/// Execute a remote command and return a streaming handle.
///
/// - stdout/stderr streams are created before exec to avoid missing data.
/// - exec requests a reply, so success_failure() will resolve.
#[tracing::instrument(skip(session))]
pub(super) async fn ssh_command(
    session: &AsyncSession<HostKeyHandler>,
    command: &str,
) -> Result<SshCommandHandle, SshCommandError> {
    let channel = session
        .open_channel()
        .await
        .map_err(SshCommandError::ChannelOpen)?;

    let stdout = channel.stdout();
    let stderr = channel.stderr();

    channel
        .exec(true, command)
        .await
        .map_err(|e| SshCommandError::Exec {
            command: command.to_string(),
            source: e,
        })?;

    Ok(SshCommandHandle {
        stdout,
        stderr,
        channel: SshChannelHandle { channel },
        command: command.to_owned(),
    })
}
