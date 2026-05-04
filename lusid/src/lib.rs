//! The `lusid` CLI: user-facing front door for applying plans to local,
//! remote, and VM-dev targets.
//!
//! Architecture: the CLI doesn't run the apply pipeline in-process. It
//! spawns [`lusid-apply`](lusid_apply) (either locally for `local apply`,
//! or inside a dev VM / a remote machine over SSH for `dev apply` /
//! `remote apply`) and pipes its stdout JSON into the [`tui`] module to
//! render a live pipeline view. stderr is buffered and shown on a separate
//! pane.
//!
//! ## Subcommands
//!
//! - `machines list` — table of all machines in `lusid.toml`.
//! - `local apply` — apply the machine matching `$(hostname)` to this host.
//! - `remote apply`/`ssh` — connect over SSH to a real machine declared
//!   with a `remote = { host = "..." }` block; SFTP the plan +
//!   `lusid-apply` binary into `/var/lib/lusid/`, run apply, stream output
//!   through the TUI. Same secret-forwarding pattern as `dev apply` but the
//!   guest identity is the target's existing `/etc/ssh/ssh_host_ed25519_key`.
//! - `dev apply`/`ssh` — spin up a local QEMU VM (via [`lusid-vm`]), SFTP
//!   the plan + `lusid-apply` binary into it, and run apply over SSH (or
//!   open an interactive shell).

mod config;
mod tui;

use std::{env, net::Ipv4Addr, path::{Path, PathBuf}, sync::Arc, time::Duration};

use clap::{Parser, Subcommand};
use lusid_apply_stdio::AppViewError;
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_machine::Remote;
use lusid_secrets::cli::{CliEnv as SecretsCliEnv, CliError as SecretsCliError, SecretsCommand};
use lusid_secrets::{
    MachinePubkeyError, RecipientsError, ReencryptForMachineError, machine_pubkey,
    reencrypt_for_machine,
};
use lusid_ssh::{Ssh, SshConnectOptions, SshError, SshKeypair, SshKeypairError, SshVolume};
use lusid_system::Arch;
use lusid_vm::{Vm, VmError, VmOptions};
use thiserror::Error;
use tokio::io::AsyncReadExt;
use tracing::error;
use which::which;

use crate::config::{Config, ConfigError, MachineConfig};
use crate::tui::{TuiError, tui};

/// Parsed CLI. `lusid_apply_linux_*_path` point at prebuilt apply binaries
/// for each target arch — the dev workflow uploads these to VMs rather than
/// compiling inside the guest. Both fall back to `lusid.toml` → defaults.
///
/// Note(cc): only x86_64 and aarch64 are plumbed. Adding a new target arch
/// means adding a new field + env var here *and* a selector wherever the
/// arch is matched. Worth revisiting as a `HashMap<Arch, PathBuf>` if the
/// list grows.
#[derive(Parser, Debug)]
#[command(name = "lusid", version, about = "Lusid CLI")]
pub struct Cli {
    #[command(subcommand)]
    pub command: Cmd,

    #[arg(long = "config", env = "LUSID_CONFIG", global = true)]
    pub config_path: Option<PathBuf>,

    #[arg(long = "log", env = "LUSID_LOG", global = true)]
    pub log: Option<String>,

    #[arg(env = "LUSID_APPLY_LINUX_X86_64", global = true)]
    pub lusid_apply_linux_x86_64_path: Option<String>,

    #[arg(env = "LUSID_APPLY_LINUX_AARCH64", global = true)]
    pub lusid_apply_linux_aarch64_path: Option<String>,

    /// Override `<root>/secrets` as the secrets directory (location of
    /// `lusid-secrets.toml` and `*.age` ciphertexts).
    #[arg(long = "secrets-dir", env = "LUSID_SECRETS_DIR", global = true)]
    pub secrets_dir: Option<PathBuf>,

    /// Path to an age identity file. Required by `local apply`,
    /// `secrets cat`, `secrets edit`, and `secrets rekey`; ignored by
    /// `secrets ls`, `secrets check`, and `secrets keygen`.
    #[arg(long = "identity", env = "LUSID_IDENTITY", global = true)]
    pub identity: Option<PathBuf>,
}

#[derive(Subcommand, Debug)]
pub enum Cmd {
    #[doc = " Manage machine definitions"]
    Machines {
        #[command(subcommand)]
        command: MachinesCmd,
    },
    #[doc = " Manage local machine"]
    Local {
        #[command(subcommand)]
        command: LocalCmd,
    },
    #[doc = " Manage remote machines"]
    Remote {
        #[command(subcommand)]
        command: RemoteCmd,
    },
    #[doc = " Develop using virtual machines"]
    Dev {
        #[command(subcommand)]
        command: DevCmd,
    },
    #[doc = " Manage age-encrypted project secrets"]
    Secrets {
        #[command(subcommand)]
        command: SecretsCommand,
    },
}

#[derive(Subcommand, Debug)]
pub enum MachinesCmd {
    #[doc = " List machines from machines.toml"]
    List,
}

#[derive(Subcommand, Debug)]
pub enum LocalCmd {
    Apply,
}

#[derive(Subcommand, Debug)]
pub enum RemoteCmd {
    Apply {
        #[doc = " Machine identifier"]
        #[arg(long = "machine")]
        machine_id: String,
    },
    Ssh {
        #[arg(long = "machine")]
        machine_id: String,
    },
}

#[derive(Subcommand, Debug)]
pub enum DevCmd {
    Apply {
        #[doc = " Machine identifier"]
        #[arg(long = "machine")]
        machine_id: String,
    },
    Ssh {
        #[arg(long = "machine")]
        machine_id: String,
    },
}

#[derive(Error, Debug)]
pub enum AppError {
    #[error(transparent)]
    Config(#[from] ConfigError),

    #[error(transparent)]
    EnvVar(#[from] env::VarError),

    #[error(transparent)]
    Command(#[from] CommandError),

    #[error(transparent)]
    Vm(#[from] VmError),

    #[error(transparent)]
    Ssh(#[from] SshError),

    #[error(transparent)]
    View(#[from] AppViewError),

    #[error("failed to convert params toml to json: {0}")]
    ParamsTomlToJson(#[from] serde_json::Error),

    #[error("failed to read stdout from apply")]
    ReadApplyStdout(#[source] tokio::io::Error),

    #[error("failed to parse stdout from lusid-apply as json")]
    ParseApplyStdoutJson(#[source] serde_json::Error),

    #[error("failed to forward stderr from lusid-apply")]
    ForwardApplyStderr(#[source] tokio::io::Error),

    #[error(transparent)]
    Which(#[from] which::Error),

    #[error("unexpected view state")]
    UnexpectedViewState,

    #[error(transparent)]
    Tui(#[from] TuiError),

    #[error(transparent)]
    Secrets(#[from] SecretsCliError),

    #[error("failed to re-encrypt secrets for target: {0}")]
    ReencryptSecrets(#[from] ReencryptForMachineError),

    #[error("failed to serialize VM SSH keypair: {0}")]
    SshKeypair(#[from] SshKeypairError),

    #[error("failed to read output from remote SSH command: {0}")]
    ReadSshOutput(#[source] tokio::io::Error),

    #[error(
        "machine {machine_id:?} is not configured for remote management — \
         add a [machines.{machine_id}] `remote = {{ host = \"...\" }}` block"
    )]
    NoRemoteConfig { machine_id: String },

    #[error("HOME env var is unset and no `ssh_key` configured for remote machine")]
    HomeUnset,

    #[error("invalid `remote.user`: {user:?} (must match useradd's NAME_REGEX, length 1..=32)")]
    InvalidSshUser { user: String },

    #[error(
        "failed to load operator SSH private key from {}: {source}\n\
         hint: passphrase-protected keys are not supported (v1); decrypt with \
         `ssh-keygen -p -f <path>` or use an unencrypted key",
        path.display()
    )]
    LoadOperatorKey {
        path: PathBuf,
        #[source]
        source: SshKeypairError,
    },

    #[error("failed to bootstrap /var/lib/lusid on target (sudo -n exit {exit:?}): {stderr}")]
    BootstrapRemoteDir { exit: Option<u32>, stderr: String },

    #[error("failed to install lusid-apply on target (sudo -n exit {exit:?}): {stderr}")]
    InstallApplyBinary { exit: Option<u32>, stderr: String },

    #[error(transparent)]
    MachinePubkey(#[from] MachinePubkeyError),
}

/// Resolve the config path (CLI flag → `LUSID_CONFIG` env → CWD → `.`) and
/// load `lusid.toml` from it.
pub async fn get_config(cli: &Cli) -> Result<Config, AppError> {
    let config_path = cli
        .config_path
        .clone()
        .or_else(|| env::var("LUSID_CONFIG").ok().map(PathBuf::from))
        .or_else(|| env::current_dir().ok())
        .unwrap_or_else(|| PathBuf::from("."));
    let config = Config::load(&config_path, cli).await?;
    Ok(config)
}

/// Dispatch on the parsed subcommand.
pub async fn run(cli: Cli, config: Config) -> Result<(), AppError> {
    let secrets_dir = resolve_secrets_dir(&cli, &config);
    let identity_path = cli.identity.clone();
    match cli.command {
        Cmd::Machines { command } => match command {
            MachinesCmd::List => cmd_machines_list(config).await,
        },
        Cmd::Local { command } => match command {
            LocalCmd::Apply => cmd_local_apply(config, secrets_dir, identity_path).await,
        },
        Cmd::Remote { command } => match command {
            RemoteCmd::Apply { machine_id } => {
                cmd_remote_apply(config, machine_id, secrets_dir, identity_path).await
            }
            RemoteCmd::Ssh { machine_id } => cmd_remote_ssh(config, machine_id).await,
        },
        Cmd::Dev { command } => match command {
            DevCmd::Apply { machine_id } => {
                cmd_dev_apply(config, machine_id, secrets_dir, identity_path).await
            }
            DevCmd::Ssh { machine_id } => cmd_dev_ssh(config, machine_id).await,
        },
        Cmd::Secrets { command } => cmd_secrets(command, secrets_dir, identity_path).await,
    }
}

/// CLI flag wins over `<root>/secrets` default. No `lusid.toml` field for
/// this yet — add one only once a real project needs to override.
fn resolve_secrets_dir(cli: &Cli, config: &Config) -> PathBuf {
    cli.secrets_dir
        .clone()
        .unwrap_or_else(|| config.root().join("secrets"))
}

async fn cmd_machines_list(config: Config) -> Result<(), AppError> {
    config.print_machines();
    Ok(())
}

async fn cmd_secrets(
    command: SecretsCommand,
    secrets_dir: PathBuf,
    identity_path: Option<PathBuf>,
) -> Result<(), AppError> {
    let env = SecretsCliEnv {
        secrets_dir,
        identity_path,
    };
    lusid_secrets::cli::run(command, env).await?;
    Ok(())
}

// Spawns `lusid-apply` as a subprocess and pipes its stdout + stderr into
// the TUI.
async fn cmd_local_apply(
    config: Config,
    secrets_dir: PathBuf,
    identity_path: Option<PathBuf>,
) -> Result<(), AppError> {
    let Config {
        ref lusid_apply_linux_x86_64_path,
        ..
    } = config;
    let MachineConfig { plan, params, .. } = config.local_machine()?;

    let mut command = Command::new(lusid_apply_linux_x86_64_path);
    command
        .args(["--root", &config.root().to_string_lossy()])
        .args(["--plan", &plan.to_string_lossy()])
        .args(["--log", &config.log])
        .args(["--secrets-dir", &secrets_dir.to_string_lossy()]);

    if let Some(identity_path) = identity_path.as_deref() {
        command.args(["--identity", &identity_path.to_string_lossy()]);
    }

    if let Some(params) = params {
        let params_json = serde_json::to_string(&params)?;
        command.args(["--params", &params_json]);
    }

    let output = command.output().await?;

    let wait = Box::pin(async move {
        output.status.await?;
        Ok::<_, CommandError>(())
    });
    tui(output.stdout, output.stderr, wait).await?;

    Ok(())
}

/// Working directory on the target for `remote apply`. Persistent across
/// applies. Holds the uploaded `lusid-apply` binary, the uploaded plan dir,
/// and (transiently) the per-apply re-encrypted secrets ciphertexts.
const REMOTE_ROOT: &str = "/var/lib/lusid";

/// `remote apply`: connect to the target over SSH, ship the plan + binary
/// (+ optionally re-encrypted secrets), then exec `lusid-apply` and stream
/// its output through the TUI. Mirrors `cmd_dev_apply` but without VM
/// bring-up; the guest's age identity is the target's existing
/// `/etc/ssh/ssh_host_ed25519_key`.
///
/// Note(cc): `cmd_dev_apply` and `cmd_remote_apply` share ~85% of the
/// SSH-and-stream flow. After both stabilise, extract a shared helper.
async fn cmd_remote_apply(
    config: Config,
    machine_id: String,
    secrets_dir: PathBuf,
    identity_path: Option<PathBuf>,
) -> Result<(), AppError> {
    let MachineConfig {
        plan,
        machine,
        params,
    } = config.get_machine(&machine_id)?;
    let remote = machine
        .remote
        .as_ref()
        .ok_or_else(|| AppError::NoRemoteConfig {
            machine_id: machine_id.clone(),
        })?;
    validate_ssh_user(remote.user())?;

    let mut ssh = connect_remote(remote).await?;

    let guest_secrets_dir = format!("{REMOTE_ROOT}/secrets");
    let guest_plan_dir = format!("{REMOTE_ROOT}/plan");
    let guest_apply_path = format!("{REMOTE_ROOT}/lusid-apply");

    // 1. Bootstrap (non-root only): create dirs and chown them so SFTP can write.
    if !remote.is_root() {
        bootstrap_remote_dirs(&mut ssh, remote.user(), REMOTE_ROOT).await?;
    }

    // 2. Pre-cleanup: drop any leftover ciphertexts from a previous run.
    //    Best-effort — never fail the apply over this.
    let _ = run_clear_secrets_dir(&mut ssh, &guest_secrets_dir, remote.is_root()).await;

    // 3. Re-encrypt secrets per-target if --identity supplied AND machine is
    //    listed in lusid-secrets.toml. Missing toml or absent [machines]
    //    entry both fall through to "no secrets forwarded" — operators can
    //    set --identity globally and only configure machines that need
    //    secrets. Plans that reference @core/secret will fail at apply time
    //    with a clear missing-secret error.
    let forward_secrets = if let Some(host_identity_path) = identity_path.as_deref() {
        match machine_pubkey(&secrets_dir, &machine_id).await {
            Ok(pubkey) => {
                let reencrypted =
                    reencrypt_for_machine(host_identity_path, &secrets_dir, &pubkey).await?;
                for secret in &reencrypted {
                    ssh.sync(SshVolume::FileBytes {
                        local: secret.ciphertext.clone(),
                        permissions: Some(0o600),
                        remote: format!("{guest_secrets_dir}/{}.age", secret.stem),
                    })
                    .await?;
                }
                !reencrypted.is_empty()
            }
            Err(MachinePubkeyError::Recipients(RecipientsError::Missing { .. }))
            | Err(MachinePubkeyError::UnknownMachine { .. }) => false,
            Err(other) => return Err(other.into()),
        }
    } else {
        false
    };

    // 4. Upload plan dir.
    let plan_local_dir = plan.parent().unwrap();
    let plan_filename = plan.file_name().unwrap().to_string_lossy().to_string();
    ssh.sync(SshVolume::DirPath {
        local: plan_local_dir.to_path_buf(),
        remote: guest_plan_dir.clone(),
    })
    .await?;

    // 5. Upload binary. For non-root, install root-owned via sudo to defend
    //    against between-SFTP-and-exec swaps.
    let apply_bin = which(select_apply_binary(&config, &machine.arch))?;
    if remote.is_root() {
        ssh.sync(SshVolume::FilePath {
            local: apply_bin,
            remote: guest_apply_path.clone(),
        })
        .await?;
        let chmod_cmd = format!("chmod 0755 {}", shell_words::quote(&guest_apply_path));
        let (_, _, _) = ssh_run(&mut ssh, &chmod_cmd).await?;
    } else {
        let upload_path = format!("{REMOTE_ROOT}/lusid-apply.upload");
        ssh.sync(SshVolume::FilePath {
            local: apply_bin,
            remote: upload_path.clone(),
        })
        .await?;
        let install_cmd = format!(
            "sudo -n install -m 0755 -o root -g root {} {} && rm -f {}",
            shell_words::quote(&upload_path),
            shell_words::quote(&guest_apply_path),
            shell_words::quote(&upload_path),
        );
        let (exit, _, stderr) = ssh_run(&mut ssh, &install_cmd).await?;
        if exit != Some(0) {
            return Err(AppError::InstallApplyBinary { exit, stderr });
        }
    }

    // 6. Build the apply command.
    //
    // Note(cc): `--root` is the operator's local lusid root path. The dev
    // path does the same; `lusid-apply` uses it to anchor relative
    // `host-path` resolution and the cache dir, but plans typically
    // anchor host-paths on the source span (the uploaded plan file's
    // location). Worth a future audit.
    let log = &config.log;
    let mut command = format!(
        "{} --root {} --plan {}/{} --log {}",
        shell_words::quote(&guest_apply_path),
        shell_words::quote(&config.root().to_string_lossy()),
        shell_words::quote(&guest_plan_dir),
        shell_words::quote(&plan_filename),
        shell_words::quote(log),
    );
    if forward_secrets {
        command.push_str(&format!(
            " --guest-mode --identity /etc/ssh/ssh_host_ed25519_key --secrets-dir {}",
            shell_words::quote(&guest_secrets_dir),
        ));
    }
    if let Some(params) = params {
        let params_json = serde_json::to_string(&params)?;
        command.push_str(&format!(" --params {}", shell_words::quote(&params_json)));
    }
    if !remote.is_root() {
        command = format!("sudo -n {command}");
    }

    // 7. Stream apply output through the TUI. Mirror cmd_dev_apply's
    //    pattern: the `async move { handle.channel.wait()... }` future
    //    field-captures `handle.channel`, leaving `handle.stdout`/
    //    `handle.stderr` borrowable in the surrounding scope.
    let mut handle = ssh.command(&command).await?;
    let wait = Box::pin(async move {
        handle.channel.wait().await?;
        Ok::<_, SshError>(())
    });
    let apply_result = tui(&mut handle.stdout, &mut handle.stderr, wait).await;

    // 8. Best-effort post-cleanup. Never shadows apply_result.
    if forward_secrets {
        let _ = run_clear_secrets_dir(&mut ssh, &guest_secrets_dir, remote.is_root()).await;
    }
    let _ = ssh.disconnect().await;

    apply_result?;
    Ok(())
}

/// `remote ssh`: open an interactive shell on the target over SSH.
async fn cmd_remote_ssh(config: Config, machine_id: String) -> Result<(), AppError> {
    let MachineConfig { machine, .. } = config.get_machine(&machine_id)?;
    let remote = machine
        .remote
        .as_ref()
        .ok_or_else(|| AppError::NoRemoteConfig {
            machine_id: machine_id.clone(),
        })?;
    validate_ssh_user(remote.user())?;

    let mut ssh = connect_remote(remote).await?;
    let _exit_code = ssh.terminal().await?;
    let _ = ssh.disconnect().await;

    Ok(())
}

// -- remote-apply helpers --------------------------------------------------

/// Connect to the remote SSH endpoint using the operator's private key.
async fn connect_remote(remote: &Remote) -> Result<Ssh, AppError> {
    let key_path = resolve_ssh_key_path(remote)?;
    let private_key = SshKeypair::load_private_key(&key_path)
        .await
        .map_err(|source| AppError::LoadOperatorKey {
            path: key_path.clone(),
            source,
        })?;
    let ssh = Ssh::connect(SshConnectOptions {
        private_key,
        addrs: (remote.host.clone(), remote.port()),
        username: remote.user().to_owned(),
        config: Arc::new(Default::default()),
        timeout: Duration::from_secs(10),
    })
    .await?;
    // Note(cc): host key verification is disabled — see
    // `lusid_ssh::session::NoCheckHandler`. Acceptable for v1; revisit when
    // remote apply needs to defend against an active MITM.
    Ok(ssh)
}

/// Resolve the operator's SSH private key path: `remote.ssh_key` (with `~`
/// expansion) or default to `${HOME}/.ssh/id_ed25519`. Errors when the path
/// would expand to a tilde-prefixed value (i.e. configured `~/...` but
/// `HOME` is unset or empty), or when no key is configured at all and HOME
/// is missing.
fn resolve_ssh_key_path(remote: &Remote) -> Result<PathBuf, AppError> {
    let home = env::var_os("HOME").filter(|h| !h.is_empty());
    let configured = remote.ssh_key.as_deref();
    let raw = match (configured, home.as_deref()) {
        (Some(path), home) => expand_tilde(path, home),
        (None, Some(home)) => PathBuf::from(home).join(".ssh/id_ed25519"),
        (None, None) => return Err(AppError::HomeUnset),
    };
    // Catch the "configured ~/foo but HOME unset" case: a literal tilde
    // would otherwise flow through to `load_private_key` as a relative path
    // and fail with a confusing not-found error.
    if raw.to_string_lossy().starts_with('~') {
        return Err(AppError::HomeUnset);
    }
    Ok(raw)
}

/// Manual tilde-expansion (no `shellexpand` in the workspace; not worth a
/// new dep). Treats both bare `~` and `~/...`. Pure — `home` is passed in
/// rather than read from the environment so the function is trivially
/// testable and parallel-safe. When `home` is `None`, returns the input
/// unchanged; the caller is responsible for surfacing a clear error.
fn expand_tilde(path: &Path, home: Option<&std::ffi::OsStr>) -> PathBuf {
    let Some(home) = home else {
        return path.to_path_buf();
    };
    let s = path.to_string_lossy();
    if s.as_ref() == "~" {
        PathBuf::from(home)
    } else if let Some(rest) = s.strip_prefix("~/") {
        PathBuf::from(home).join(rest)
    } else {
        path.to_path_buf()
    }
}

/// Validate `<ssh-user>` against shell-injection. Mirrors `useradd`'s
/// `NAME_REGEX`: must start with alnum/underscore (NOT dash — `chown -x` would
/// treat the value as a flag), then alnum/underscore/dash, total length 1..=32.
fn validate_ssh_user(user: &str) -> Result<(), AppError> {
    let mut chars = user.chars();
    let first_ok = matches!(chars.next(), Some(c) if c.is_ascii_alphanumeric() || c == '_');
    let rest_ok = chars.all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-');
    if !first_ok || !rest_ok || user.len() > 32 {
        return Err(AppError::InvalidSshUser {
            user: user.to_owned(),
        });
    }
    Ok(())
}

/// Run a remote command, return (exit_code, stdout, stderr). For short
/// **one-shot** commands where the caller wants to inspect the output
/// (bootstrap, install, cleanup). Drains both streams concurrently so a
/// stuck buffer can't deadlock waiting on the other.
///
/// Don't use this for long-running or self-daemonizing commands: it blocks
/// until both stdout and stderr hit EOF, which only happens when the remote
/// process closes its file descriptors. For streaming apply use the same
/// pattern as `cmd_remote_apply` (handle.stdout/stderr + a wait future).
async fn ssh_run(
    ssh: &mut Ssh,
    command: &str,
) -> Result<(Option<u32>, String, String), AppError> {
    let mut handle = ssh.command(command).await?;
    let mut stdout = String::new();
    let mut stderr = String::new();
    let stdout_fut = handle.stdout.read_to_string(&mut stdout);
    let stderr_fut = handle.stderr.read_to_string(&mut stderr);
    tokio::try_join!(stdout_fut, stderr_fut).map_err(AppError::ReadSshOutput)?;
    let exit = handle.channel.wait().await?;
    Ok((exit, stdout, stderr))
}

/// Create `/var/lib/lusid` and its `plan` / `secrets` subdirs on the target
/// and chown them to the SSH user. Idempotent. Only called when `user` is
/// non-root; root SFTP can mkdir directly via `sftp_mkdirs`.
///
/// `user` must already be validated by `validate_ssh_user` to contain only
/// shell-safe characters; safe to interpolate here.
async fn bootstrap_remote_dirs(
    ssh: &mut Ssh,
    user: &str,
    root: &str,
) -> Result<(), AppError> {
    let cmd = format!(
        "sudo -n mkdir -p {root} {root}/plan {root}/secrets \
         && sudo -n chown {user} {root} {root}/plan {root}/secrets",
        root = shell_words::quote(root),
        user = user,
    );
    let (exit, _stdout, stderr) = ssh_run(ssh, &cmd).await?;
    if exit != Some(0) {
        return Err(AppError::BootstrapRemoteDir { exit, stderr });
    }
    Ok(())
}

/// Best-effort: clear the secrets dir on the target and recreate it empty.
/// Idempotent. Errors are not propagated (caller uses `let _ = ...`).
async fn run_clear_secrets_dir(
    ssh: &mut Ssh,
    dir: &str,
    is_root: bool,
) -> Result<(), AppError> {
    let dir_q = shell_words::quote(dir).into_owned();
    let cmd = if is_root {
        format!("rm -rf {dir_q} && mkdir -p {dir_q}")
    } else {
        // `$(id -un)` runs in the remote login shell (NOT under sudo), so
        // it expands to the SSH user's name, restoring write access to the
        // recreated dir.
        format!(
            "sudo -n rm -rf {dir_q} && sudo -n mkdir -p {dir_q} && sudo -n chown $(id -un) {dir_q}"
        )
    };
    let (_exit, _stdout, _stderr) = ssh_run(ssh, &cmd).await?;
    Ok(())
}

/// Pick the `lusid-apply` binary for the target arch. Falls back to the
/// default name if unset; `which()` resolves it on PATH.
fn select_apply_binary(config: &Config, arch: &Arch) -> String {
    match arch {
        Arch::X86_64 => config.lusid_apply_linux_x86_64_path.clone(),
        Arch::Aarch64 => config.lusid_apply_linux_aarch64_path.clone(),
    }
}

// `dev apply`: boot a local QEMU VM matching the machine spec, upload the
// plan directory and a prebuilt `lusid-apply` binary over SFTP, then run
// apply remotely and stream its stdout/stderr through the TUI just like
// local apply. The VM's SSH keypair lives inside its instance dir (see
// `lusid_vm`).
//
// Secrets are forwarded via per-target re-encryption: when `identity_path`
// is set, the host decrypts every `*.age` with the operator identity,
// re-encrypts each plaintext to the VM's SSH keypair alone, ships the
// ciphertexts to `<dev_dir>/secrets/`, and points the guest's
// `lusid-apply` at `<dev_dir>/identity` (the same VM keypair in OpenSSH
// PEM form) via `--identity --guest-mode`. The operator identity never
// leaves the host.
async fn cmd_dev_apply(
    config: Config,
    machine_id: String,
    secrets_dir: PathBuf,
    identity_path: Option<PathBuf>,
) -> Result<(), AppError> {
    let MachineConfig {
        plan,
        machine,
        params,
    } = config.get_machine(&machine_id)?;

    let root = config.root();
    let mut ctx = Context::create(root).unwrap();

    let instance_id = &machine_id;
    let ports = vec![];
    let options = VmOptions {
        instance_id,
        machine: &machine,
        ports,
    };
    let vm = Vm::run(&mut ctx, options).await?;

    let vm_keypair = vm.ssh_keypair().await?;

    let mut ssh = Ssh::connect(SshConnectOptions {
        private_key: vm_keypair.private_key.clone(),
        addrs: (Ipv4Addr::LOCALHOST, vm.ssh_port),
        username: vm.user.clone(),
        config: Arc::new(Default::default()),
        timeout: Duration::from_secs(10),
    })
    .await?;

    let dev_dir = format!("/home/{}", vm.user);
    let plan_dir = plan.parent().unwrap();
    let plan_filename = plan.file_name().unwrap().to_string_lossy();
    let apply_bin = which(select_apply_binary(&config, &machine.arch))?;

    let mut volumes = vec![
        SshVolume::FilePath {
            local: apply_bin,
            remote: format!("{dev_dir}/lusid-apply"),
        },
        SshVolume::DirPath {
            local: plan_dir.to_path_buf(),
            remote: format!("{dev_dir}/plan"),
        },
    ];

    // Secrets forwarding mirrors `cmd_local_apply`'s gating on
    // `identity_path`: no identity → no secrets shipped, and the guest
    // will run without a secrets context (plans referencing
    // `@core/secret` will error loudly).
    let guest_identity_path = format!("{dev_dir}/identity");
    let guest_secrets_dir = format!("{dev_dir}/secrets");
    let forward_secrets = if let Some(identity_path) = identity_path.as_deref() {
        // The VM's auth keypair doubles as the age recipient/identity: it
        // already lives on both sides (instance dir on host, authorized_keys
        // on guest via cloud-init), is ephemeral per-VM, and re-using it
        // avoids a second keygen + a cloud-init host-key injection path.
        let machine_pubkey = vm_keypair.public_openssh()?;
        let reencrypted =
            reencrypt_for_machine(identity_path, &secrets_dir, &machine_pubkey).await?;

        let private_pem = vm_keypair.private_openssh()?;
        volumes.push(SshVolume::FileBytes {
            local: private_pem.into_bytes(),
            permissions: Some(0o600),
            remote: guest_identity_path.clone(),
        });
        for secret in reencrypted {
            volumes.push(SshVolume::FileBytes {
                local: secret.ciphertext,
                permissions: None,
                remote: format!("{guest_secrets_dir}/{}.age", secret.stem),
            });
        }
        true
    } else {
        false
    };

    let log = &config.log;
    let mut command = format!(
        "{dev_dir}/lusid-apply --root {} --plan {dev_dir}/plan/{plan_filename} --log {log}",
        root.display()
    );
    if forward_secrets {
        command.push_str(&format!(
            " --guest-mode --identity {guest_identity_path} --secrets-dir {guest_secrets_dir}"
        ));
    }
    if let Some(params) = params {
        let params_json = serde_json::to_string(&params)?;
        // `shell_words::quote` properly POSIX-escapes embedded single quotes,
        // which serde_json doesn't escape and the previous `'…'` quoting
        // mishandled.
        command.push_str(&format!(" --params {}", shell_words::quote(&params_json)));
    }

    for volume in volumes {
        ssh.sync(volume).await?;
    }

    let mut handle = ssh.command(&command).await?;
    let wait = Box::pin(async move {
        handle.channel.wait().await?;
        Ok::<_, SshError>(())
    });

    tui(&mut handle.stdout, &mut handle.stderr, wait).await?;

    ssh.disconnect().await?;

    Ok(())
}

// `dev ssh`: boot the VM (idempotent — reuses the instance if it already
// exists) and attach the local TTY to a remote interactive shell via
// `Ssh::terminal`. No TUI, no apply — just a shell inside the guest.
async fn cmd_dev_ssh(config: Config, machine_id: String) -> Result<(), AppError> {
    let MachineConfig {
        plan: _,
        machine,
        params: _,
    } = config.get_machine(&machine_id)?;

    let root = config.path.parent().unwrap();
    let mut ctx = Context::create(root).unwrap();

    let instance_id = &machine_id;
    let ports = vec![];
    let options = VmOptions {
        instance_id,
        machine: &machine,
        ports,
    };
    let vm = Vm::run(&mut ctx, options).await?;

    let mut ssh = Ssh::connect(SshConnectOptions {
        private_key: vm.ssh_keypair().await?.private_key,
        addrs: (Ipv4Addr::LOCALHOST, vm.ssh_port),
        username: vm.user,
        config: Arc::new(Default::default()),
        timeout: Duration::from_secs(10),
    })
    .await?;

    let _exit_code = ssh.terminal().await?;

    ssh.disconnect().await?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::ffi::OsStr;

    use super::*;

    #[test]
    fn expand_tilde_bare() {
        let home = OsStr::new("/home/alice");
        assert_eq!(
            expand_tilde(Path::new("~"), Some(home)),
            PathBuf::from("/home/alice")
        );
    }

    #[test]
    fn expand_tilde_with_subpath() {
        let home = OsStr::new("/home/alice");
        assert_eq!(
            expand_tilde(Path::new("~/.ssh/id_ed25519"), Some(home)),
            PathBuf::from("/home/alice/.ssh/id_ed25519")
        );
    }

    #[test]
    fn expand_tilde_absolute_passthrough() {
        let home = OsStr::new("/home/alice");
        assert_eq!(
            expand_tilde(Path::new("/etc/keys/k"), Some(home)),
            PathBuf::from("/etc/keys/k")
        );
    }

    #[test]
    fn expand_tilde_no_home_passthrough() {
        // Without home we deliberately return the input unchanged so the
        // call site can surface a clear "HOME unset" error rather than
        // producing a silent relative path.
        assert_eq!(
            expand_tilde(Path::new("~/foo"), None),
            PathBuf::from("~/foo")
        );
    }

    #[test]
    fn validate_ssh_user_accepts_normal() {
        validate_ssh_user("root").unwrap();
        validate_ssh_user("mikey").unwrap();
        validate_ssh_user("user_1").unwrap();
        validate_ssh_user("svc-account").unwrap();
        validate_ssh_user("a").unwrap();
    }

    #[test]
    fn validate_ssh_user_rejects_leading_dash() {
        let err = validate_ssh_user("-x").unwrap_err();
        assert!(matches!(err, AppError::InvalidSshUser { .. }));
    }

    #[test]
    fn validate_ssh_user_rejects_empty() {
        assert!(matches!(
            validate_ssh_user("").unwrap_err(),
            AppError::InvalidSshUser { .. }
        ));
    }

    #[test]
    fn validate_ssh_user_rejects_metachars() {
        for bad in &["mikey;rm", "mikey$x", "mikey ls", "mikey/x", "mikey`"] {
            assert!(
                validate_ssh_user(bad).is_err(),
                "should reject {bad:?}"
            );
        }
    }

    #[test]
    fn validate_ssh_user_rejects_too_long() {
        let too_long = "a".repeat(33);
        assert!(matches!(
            validate_ssh_user(&too_long).unwrap_err(),
            AppError::InvalidSshUser { .. }
        ));
    }

    #[test]
    fn expand_tilde_with_no_home_returns_input() {
        // Confirms the call site can detect the unexpanded `~` to surface a
        // HOME-unset error rather than passing it through silently.
        let result = expand_tilde(Path::new("~/foo"), None);
        assert!(result.to_string_lossy().starts_with('~'));
    }
}
