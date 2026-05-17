//! Thin wrapper around [`tokio::process::Command`] that lusid operations use to shell
//! out (apt, pacman, file ownership changes, user command resources, …).
//!
//! Why wrap it at all:
//! - Boolean `stdout` / `stderr` knobs that toggle between piped (captured) and
//!   inherited (streamed directly to the parent's stdio).
//! - A [`Command::sudo`] helper that rewraps the command under `sudo -n`, preserving
//!   explicitly-set env vars and the working directory.
//! - Uniform `CommandError` variants for the common failure modes.
//! - [`Command::handle`] for commands where success and failure both produce the
//!   same value type (e.g. apt's `dpkg-query` check classifying a package as
//!   installed or not).
//! - [`Command::outcome`] for commands where the caller wants to branch on the
//!   exit status itself (e.g. `getent` returning non-zero for an absent name).
//! - [`Command::from_str`] parses shell-style argument strings via `shell-words`, so
//!   plan authors can write a single string instead of a vector.
//
// TODO(cc): `async-promise` is declared in `Cargo.toml` but not used anywhere in this
// crate - it's only used by `lusid-ssh`. Drop it from this manifest.
//
// TODO(cc): this crate uses `tokio::io::{AsyncReadExt, AsyncWriteExt}` but doesn't enable
// tokio's `io-util` feature locally. It currently compiles only because another workspace
// member (e.g. `lusid-http`) turns that feature on, and Cargo feature-unification leaks it
// here. `cargo check -p lusid-cmd` in isolation fails. Add `io-util` to the crate's own
// `tokio` feature list.

use std::ffi::{OsStr, OsString};
use std::fmt::Display;
use std::path::Path;
use std::pin::Pin;
use std::process::{ExitStatus, Stdio};
use std::str::FromStr;
use tokio::io::AsyncReadExt;
use tokio::process::{Child, ChildStderr, ChildStdin, ChildStdout, Command as BaseCommand};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum CommandError {
    #[error("failed to spawn command: {command}")]
    Spawn {
        command: String,
        #[source]
        error: tokio::io::Error,
    },

    #[error("failed to get command output: {command}")]
    Output {
        command: String,
        #[source]
        error: tokio::io::Error,
    },

    #[error("command failed: {command}\n{stderr}")]
    Failure { command: String, stderr: String },

    #[error("unable to capture stdout")]
    NoStdout,

    #[error("failed to read stdout")]
    ReadStdout(#[source] tokio::io::Error),

    #[error("unable to capture stderr")]
    NoStderr,

    #[error("failed to read stderr")]
    ReadStderr(#[source] tokio::io::Error),

    #[error("unable to capture stdin")]
    NoStdin,
}

#[derive(Debug)]
pub struct Command {
    cmd: BaseCommand,
    stdout: bool,
    stderr: bool,
}

impl Display for Command {
    // Note(cc): `to_str().unwrap()` here will panic on non-UTF-8 program/args (rare on
    // modern Linux, impossible on strings that came in from Rimu, but still a sharp
    // edge). Switch to `to_string_lossy()` if this ever fires.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let cmd = self.cmd.as_std();
        let program = cmd.get_program().to_str().unwrap();
        let args = cmd
            .get_args()
            .map(|a| a.to_str().unwrap())
            .collect::<Vec<_>>()
            .join(" ");
        if args.is_empty() {
            write!(f, "{program}",)
        } else {
            write!(f, "{program} {args}",)
        }
    }
}

impl Command {
    pub fn new<S: AsRef<OsStr>>(program: S) -> Self {
        Self {
            cmd: BaseCommand::new(program),
            stdout: false,
            stderr: false,
        }
    }

    pub fn arg<S: AsRef<OsStr>>(&mut self, arg: S) -> &mut Self {
        self.cmd.arg(arg);
        self
    }

    pub fn args<I, S>(&mut self, args: I) -> &mut Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        self.cmd.args(args);
        self
    }

    pub fn env<K, V>(&mut self, key: K, value: V) -> &mut Self
    where
        K: AsRef<OsStr>,
        V: AsRef<OsStr>,
    {
        self.cmd.env(key, value);
        self
    }

    pub fn envs<I, K, V>(&mut self, vars: I) -> &mut Self
    where
        I: IntoIterator<Item = (K, V)>,
        K: AsRef<OsStr>,
        V: AsRef<OsStr>,
    {
        self.cmd.envs(vars);
        self
    }

    pub fn current_dir<P: AsRef<Path>>(&mut self, dir: P) -> &mut Command {
        self.cmd.current_dir(dir);
        self
    }

    /// If `true`, the child's stdout is inherited from the parent (streamed live);
    /// if `false`, it is piped so the parent can capture it. Default: `false`.
    pub fn stdout(&mut self, stdout: bool) -> &mut Command {
        self.stdout = stdout;
        self
    }

    /// Same semantics as [`stdout`](Self::stdout) but for the stderr stream.
    pub fn stderr(&mut self, stderr: bool) -> &mut Command {
        self.stderr = stderr;
        self
    }

    pub fn get_stdout(&self) -> bool {
        self.stdout
    }

    pub fn get_stderr(&self) -> bool {
        self.stderr
    }

    /// Rewrap this command as `sudo -n <program> <args>`, preserving explicitly-set
    /// env vars (passed as `KEY=VALUE` args so sudo forwards them) and the working
    /// directory. The `-n` flag makes sudo fail fast rather than block for a password
    /// prompt - lusid operations must be non-interactive.
    pub fn sudo(self) -> Self {
        let mut privileged_cmd = Command::new("sudo");

        let cmd = self.cmd.as_std();

        privileged_cmd.arg("-n"); // non-interactive

        for env in cmd.get_envs() {
            if let (key, Some(value)) = env {
                let mut env_arg = OsString::new();
                env_arg.push(key);
                env_arg.push("=");
                env_arg.push(value);
                privileged_cmd.arg(env_arg);
            }
        }

        privileged_cmd
            .arg(cmd.get_program())
            .args(cmd.get_args())
            .stdout(self.get_stdout())
            .stderr(self.get_stderr());

        if let Some(dir) = cmd.get_current_dir() {
            privileged_cmd.current_dir(dir);
        }

        privileged_cmd
    }

    pub fn spawn(&mut self) -> Result<Child, CommandError> {
        self.cmd
            .stdin(Stdio::piped())
            .stdout(if self.stdout {
                Stdio::inherit()
            } else {
                Stdio::piped()
            })
            .stderr(if self.stderr {
                Stdio::inherit()
            } else {
                Stdio::piped()
            })
            .spawn()
            .map_err(|error| CommandError::Spawn {
                command: self.to_string(),
                error,
            })
    }
}

pub struct CommandOutput {
    pub stdout: ChildStdout,
    pub stderr: ChildStderr,
    pub stdin: ChildStdin,
    pub status: Pin<Box<dyn Future<Output = Result<ExitStatus, CommandError>> + Send + 'static>>,
}

/// Buffered result of a command run to completion. Produced by [`Command::outcome`].
pub struct CommandOutcome {
    pub status: ExitStatus,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}

impl Command {
    pub async fn output(&mut self) -> Result<CommandOutput, CommandError> {
        // NOTE (mw): we use spawn() because output() doesn't work
        //   with stdout or stderr as we expect.
        //
        // See: https://docs.rs/tokio/latest/tokio/process/struct.Command.html#method.output
        //
        // > Note: `output()`, unlike the standard library, will unconditionally configure
        // > the stdout/stderr handles to be pipes, even if they have been previously configured.
        // > If this is not desired then the `spawn` method should be used in combination with the
        // > `wait_with_output` method on child.
        let mut child = self.spawn()?;

        let stdout = child.stdout.take().ok_or(CommandError::NoStdout)?;
        let stderr = child.stderr.take().ok_or(CommandError::NoStderr)?;
        let stdin = child.stdin.take().ok_or(CommandError::NoStdin)?;

        let command_str = self.to_string();
        let status = Box::pin(async move {
            child.wait().await.map_err(|error| CommandError::Output {
                command: command_str,
                error,
            })
        });

        Ok(CommandOutput {
            stdout,
            stderr,
            stdin,
            status,
        })
    }

    /// Run the command to completion and return the exit status plus fully-captured
    /// stdout and stderr. Use when a non-zero exit carries information the caller
    /// wants to branch on directly - e.g. `getent` returns non-zero for an absent
    /// name. Unlike [`Self::handle`], the success and failure paths don't need to
    /// share a return type; unlike [`Self::run`], non-zero exits are not errors.
    pub async fn outcome(&mut self) -> Result<CommandOutcome, CommandError> {
        let mut output = self.output().await?;
        let status = output.status.await?;
        let mut stdout = Vec::new();
        output
            .stdout
            .read_to_end(&mut stdout)
            .await
            .map_err(CommandError::ReadStdout)?;
        let mut stderr = Vec::new();
        output
            .stderr
            .read_to_end(&mut stderr)
            .await
            .map_err(CommandError::ReadStderr)?;
        Ok(CommandOutcome {
            status,
            stdout,
            stderr,
        })
    }

    /// Run the command to completion, returning captured stdout on success or a
    /// [`CommandError::Failure`] (with stderr attached) on non-zero exit.
    pub async fn run(&mut self) -> Result<Vec<u8>, CommandError> {
        let mut output = self.output().await?;
        let status = output.status.await?;
        if status.success() {
            let mut stdout = Vec::new();
            output
                .stdout
                .read_to_end(&mut stdout)
                .await
                .map_err(CommandError::ReadStdout)?;
            Ok(stdout)
        } else {
            let mut stderr = String::new();
            output
                .stderr
                .read_to_string(&mut stderr)
                .await
                .map_err(CommandError::ReadStderr)?;
            Err(CommandError::Failure {
                command: self.to_string(),
                stderr,
            })
        }
    }

    /// Run with pluggable success/failure handlers. Use this when a non-zero exit
    /// carries meaning - e.g. the `command` resource's `is_installed` check probes
    /// a boolean via exit code, not via error.
    ///
    /// If `stderr_handler` returns `Ok(Some(value))` the exit is treated as a success;
    /// `Ok(None)` falls through to [`CommandError::Failure`]. The outer `Result` is for
    /// I/O failures, the inner `Result` for domain-level handler errors.
    pub async fn handle<OutHandler, ErrHandler, HandlerValue, HandlerError>(
        &mut self,
        stdout_handler: OutHandler,
        stderr_handler: ErrHandler,
    ) -> Result<Result<HandlerValue, HandlerError>, CommandError>
    where
        ErrHandler: Fn(&Vec<u8>) -> Result<Option<HandlerValue>, HandlerError>,
        OutHandler: Fn(&Vec<u8>) -> Result<HandlerValue, HandlerError>,
    {
        let mut output = self.output().await?;
        let status = output.status.await?;
        if status.success() {
            let mut stdout = Vec::new();
            output
                .stdout
                .read_to_end(&mut stdout)
                .await
                .map_err(CommandError::ReadStdout)?;
            return Ok(stdout_handler(&stdout));
        }

        let mut stderr = Vec::new();
        output
            .stderr
            .read_to_end(&mut stderr)
            .await
            .map_err(CommandError::ReadStderr)?;

        match stderr_handler(&stderr) {
            Err(error) => Ok(Err(error)),
            Ok(Some(value)) => Ok(Ok(value)),
            Ok(None) => Err(CommandError::Failure {
                command: self.to_string(),
                stderr: String::from_utf8_lossy(&stderr).to_string(),
            }),
        }
    }
}

impl FromStr for Command {
    type Err = shell_words::ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let command_words = shell_words::split(s)?;
        if command_words.is_empty() {
            Ok(Command::new(""))
        } else {
            let mut cmd = Command::new(&command_words[0]);
            cmd.args(&command_words[1..]);
            Ok(cmd)
        }
    }
}

impl Command {
    /// Wrap a shell string as `sh -c "<command>"`. Use when the plan author wants
    /// shell features (pipes, globs, `&&`); prefer structured args otherwise.
    pub fn new_sh(command: &str) -> Self {
        let mut cmd = Command::new("sh");
        cmd.arg("-c");
        cmd.arg(command);
        cmd
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_command() {
        assert_eq!(Command::new("lusid").to_string(), "lusid")
    }

    #[test]
    fn test_get_command_with_one_arg() {
        assert_eq!(Command::new("lusid").arg("-a").to_string(), "lusid -a")
    }

    #[test]
    fn test_get_command_with_two_args() {
        assert_eq!(
            Command::new("lusid").arg("-a").arg("-b").to_string(),
            "lusid -a -b"
        )
    }
}
