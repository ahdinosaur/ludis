use futures_util::StreamExt;
use signal_hook::consts::SIGWINCH;
use signal_hook_tokio::Signals;
use std::fmt::Debug;
use std::{env, io};
use termion::raw::IntoRawMode;
use thiserror::Error;
use tokio::io::copy;

use crate::session::{AsyncSession, HostKeyHandler};

#[derive(Error, Debug)]
pub enum SshTerminalError {
    #[error("failed to open SSH session channel: {0}")]
    ChannelOpen(#[source] russh::Error),

    #[error("SSH protocol error: {0}")]
    Russh(#[from] russh::Error),

    #[error("failed to request pty: {0}")]
    RequestPty(#[source] russh::Error),

    #[error("failed to get terminal size: {0}")]
    TerminalSize(#[source] io::Error),

    #[error("failed to create signals stream")]
    Signals(#[source] io::Error),

    #[error("failed to put stdout into raw mode: {0}")]
    StdoutRawMode(#[source] io::Error),

    #[error("stdin piping failed: {0}")]
    StdinPipe(#[source] io::Error),

    #[error("stdout piping failed: {0}")]
    StdoutPipe(#[source] io::Error),

    #[error("stderr piping failed: {0}")]
    StderrPipe(#[source] io::Error),
}

#[tracing::instrument(skip(session))]
pub(super) async fn ssh_terminal(
    session: &AsyncSession<HostKeyHandler>,
) -> Result<Option<u32>, SshTerminalError> {
    let mut channel = session
        .open_channel()
        .await
        .map_err(SshTerminalError::ChannelOpen)?;

    let mut signals = Signals::new([SIGWINCH]).map_err(SshTerminalError::Signals)?;
    let signals_handle = signals.handle();

    // We're using `termion` to put the terminal into raw mode, so that we can
    // display the output of interactive applications correctly.
    let _raw_term = std::io::stdout()
        .into_raw_mode()
        .map_err(SshTerminalError::StdoutRawMode)?;

    let want_reply = true;
    let term = &env::var("TERM").unwrap_or("xterm-256color".into());
    let (col_width, row_height) =
        termion::terminal_size().map_err(SshTerminalError::TerminalSize)?;
    let pix_width = 0;
    let pix_height = 0;
    let terminal_modes = &[]; // ideally you want to pass the actual terminal modes here

    channel
        .request_pty(
            want_reply,
            term,
            col_width.into(),
            row_height.into(),
            pix_width,
            pix_height,
            terminal_modes,
        )
        .await
        .map_err(SshTerminalError::RequestPty)?;

    // Start an interactive shell
    channel.request_shell(want_reply).await?;

    let exit_code = {
        // Handle I/O streams: stdin -> channel_stdin, channel_stdout/err -> terminal

        let mut terminal_stdin = tokio::io::stdin();
        let mut channel_stdin = channel.stdin();
        let stdin_future = copy(&mut terminal_stdin, &mut channel_stdin);
        tokio::pin!(stdin_future);

        let mut channel_stdout = channel.stdout();
        let mut terminal_stdout = tokio::io::stdout();
        let stdout_future = copy(&mut channel_stdout, &mut terminal_stdout);
        tokio::pin!(stdout_future);

        let mut channel_stderr = channel.stderr();
        let mut terminal_stderr = tokio::io::stderr();
        let stderr_future = copy(&mut channel_stderr, &mut terminal_stderr);
        tokio::pin!(stderr_future);

        let mut stdin_done = false;
        let mut stdout_done = false;
        let mut stderr_done = false;

        let exit_code_future = channel.recv_exit_status().wait();
        tokio::pin!(exit_code_future);

        loop {
            tokio::select! {
                // Remote program finished
                exit_code = &mut exit_code_future => {
                    signals_handle.close();
                    break exit_code.copied()
                }

                // Window resize
                maybe_signal = signals.next() => {
                    if let Some(signal) = maybe_signal {
                        match signal {
                            SIGWINCH => {
                                let (col_width, row_height) =
                                    termion::terminal_size().map_err(SshTerminalError::TerminalSize)?;
                                channel.window_change(col_width.into(), row_height.into(), 0, 0).await?;
                            }
                            _ => unreachable!(),
                        }
                    }
                }

                res = &mut stdout_future, if !stdout_done => {
                    let _ = res.map_err(SshTerminalError::StdoutPipe)?;
                    stdout_done = true
                }

                res = &mut stderr_future, if !stderr_done => {
                    let _ = res.map_err(SshTerminalError::StderrPipe)?;
                    stderr_done = true
                }
                res = &mut stdin_future, if !stdin_done => {
                    #[allow(clippy::collapsible_if)]
                    if let Err(error) = res {
                        if error.kind() != io::ErrorKind::BrokenPipe {
                            return Err(SshTerminalError::StdinPipe(error));
                        }
                    }
                    stdin_done = true;
                }
            }
        }
    };

    if !channel.is_closed() {
        channel.close().await?;
        channel.wait_close().await;
    }

    tracing::info!(exit_code = exit_code, "Remote terminal completed");

    Ok(exit_code)
}
