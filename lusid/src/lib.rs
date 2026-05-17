//! The `lusid` CLI. See the crate README for the subcommand surface.
//!
//! Architecture: the CLI doesn't run the apply pipeline in-process. It
//! spawns [`lusid-apply`](lusid_apply) (locally, in a dev VM, or over SSH
//! to a remote machine) and pipes its stdout JSON into the [`tui`] module
//! to render a live pipeline view. stderr is buffered into a separate pane.

mod config;
mod embedded;
mod tui;

use std::{
    borrow::Cow,
    env,
    net::Ipv4Addr,
    path::{Path, PathBuf},
    sync::Arc,
    time::Duration,
};

use clap::{Parser, Subcommand};
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_machine::Remote;
use lusid_secrets::cli::{CliEnv as SecretsCliEnv, CliError as SecretsCliError, SecretsCommand};
use lusid_secrets::{
    RecipientsError, ReencryptForTargetError, reencrypt_for_declared_machine, reencrypt_for_target,
};
use lusid_ssh::{
    HostKeyVerification, Ssh, SshConnectOptions, SshError, SshKeypairError, SshVolume,
    load_private_key,
};
use lusid_system::Arch;
use lusid_vm::{Vm, VmError, VmOptions};
use thiserror::Error;
use tokio::io::AsyncReadExt;

use crate::config::{Config, ConfigError, MachineConfig};
use crate::embedded::EmbeddedError;
use crate::tui::{TuiError, tui};

/// Parsed CLI. The `lusid-apply` worker is baked into this binary at build
/// time for each supported target arch (see [`crate::embedded`] /
/// [`build.rs`](../../build.rs)) - there is no runtime override.
#[derive(Parser, Debug)]
#[command(name = "lusid", version, about = "Lusid CLI")]
pub struct Cli {
    #[command(subcommand)]
    pub command: Cmd,

    #[arg(long = "config", env = "LUSID_CONFIG", global = true)]
    pub config_path: Option<PathBuf>,

    #[arg(long = "log", env = "LUSID_LOG", global = true)]
    pub log: Option<String>,

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
    #[doc = " Parse + validate the plan without probing or mutating state"]
    Parse,
}

#[derive(Subcommand, Debug)]
pub enum RemoteCmd {
    Apply {
        #[doc = " Machine identifier"]
        #[arg(long = "machine")]
        machine_id: String,
    },
    #[doc = " Parse + validate the plan on the target without probing or mutating state"]
    Parse {
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
    #[doc = " Parse + validate the plan in the dev VM without probing or mutating state"]
    Parse {
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
    Command(#[from] CommandError),

    #[error(transparent)]
    Vm(#[from] VmError),

    #[error(transparent)]
    Ssh(#[from] SshError),

    #[error("failed to convert params toml to json: {0}")]
    ParamsTomlToJson(#[from] serde_json::Error),

    #[error(transparent)]
    Tui(#[from] TuiError),

    #[error(transparent)]
    Secrets(#[from] SecretsCliError),

    #[error("failed to re-encrypt secrets for target: {0}")]
    ReencryptSecrets(#[from] ReencryptForTargetError),

    #[error("failed to serialize VM SSH keypair: {0}")]
    SshKeypair(#[from] SshKeypairError),

    #[error(transparent)]
    Embedded(#[from] EmbeddedError),

    #[error("failed to read output from remote SSH command: {0}")]
    ReadSshOutput(#[source] tokio::io::Error),

    #[error(
        "machine {machine_id:?} is not configured for remote management - \
         add a [machines.{machine_id}] `remote = {{ host = \"...\" }}` block"
    )]
    NoRemoteConfig { machine_id: String },

    #[error("HOME env var is unset and no `ssh_key` configured for remote machine")]
    HomeUnset,

    #[error("failed to ensure SSH config directory exists at {}: {source}", path.display())]
    EnsureSshDir {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error(
        "cannot expand leading `~` in {path}: only `~` and `~/...` are supported \
         (no `~user` syntax)",
        path = path.display()
    )]
    UnsupportedTildePath { path: PathBuf },

    #[error(
        "invalid `remote.user`: {user:?} (must be 1..=32 ASCII alphanumeric/underscore/dash; \
         first character must be alphanumeric or underscore, not a dash)"
    )]
    InvalidSshUser { user: String },

    #[error(
        "failed to load operator SSH private key from {}: {source}",
        path.display()
    )]
    LoadOperatorKey {
        path: PathBuf,
        #[source]
        source: SshKeypairError,
    },

    #[error(
        "operator SSH private key at {} is passphrase-protected, which is \
         unsupported in v1; decrypt with `ssh-keygen -p -f <path>` or use an \
         unencrypted key",
        path.display()
    )]
    LoadOperatorKeyEncrypted { path: PathBuf },

    #[error("failed to bootstrap /var/lib/lusid on target (sudo -n exit {exit:?}): {stderr}")]
    BootstrapRemoteDir { exit: Option<u32>, stderr: String },

    #[error("failed to install lusid-apply on target (sudo -n exit {exit:?}): {stderr}")]
    InstallApplyBinary { exit: Option<u32>, stderr: String },
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
            LocalCmd::Apply => cmd_local_apply(config, secrets_dir, identity_path, false).await,
            LocalCmd::Parse => cmd_local_apply(config, secrets_dir, identity_path, true).await,
        },
        Cmd::Remote { command } => match command {
            RemoteCmd::Apply { machine_id } => {
                cmd_remote_apply(config, machine_id, secrets_dir, identity_path, false).await
            }
            RemoteCmd::Parse { machine_id } => {
                cmd_remote_apply(config, machine_id, secrets_dir, identity_path, true).await
            }
            RemoteCmd::Ssh { machine_id } => cmd_remote_ssh(config, machine_id).await,
        },
        Cmd::Dev { command } => match command {
            DevCmd::Apply { machine_id } => {
                cmd_dev_apply(config, machine_id, secrets_dir, identity_path, false).await
            }
            DevCmd::Parse { machine_id } => {
                cmd_dev_apply(config, machine_id, secrets_dir, identity_path, true).await
            }
            DevCmd::Ssh { machine_id } => cmd_dev_ssh(config, machine_id).await,
        },
        Cmd::Secrets { command } => cmd_secrets(command, secrets_dir, identity_path).await,
    }
}

/// CLI flag wins over `<root>/secrets` default. No `lusid.toml` field for
/// this yet - add one only once a real project needs to override.
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

async fn cmd_local_apply(
    config: Config,
    secrets_dir: PathBuf,
    identity_path: Option<PathBuf>,
    parse_only: bool,
) -> Result<(), AppError> {
    let MachineConfig { plan, params, .. } = config.local_machine()?;

    let apply_bin = embedded::resolve_or_extract_for_arch(Arch::get()).await?;

    let mut command = Command::new(&apply_bin);
    command
        .args(["--root", &config.root().to_string_lossy()])
        .args(["--plan", &plan.to_string_lossy()])
        .args(["--log", &config.log])
        .args(["--secrets-dir", &secrets_dir.to_string_lossy()]);

    if let Some(identity_path) = identity_path.as_deref() {
        command.args(["--identity", &identity_path.to_string_lossy()]);
    }

    if parse_only {
        command.arg("--parse-only");
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
    parse_only: bool,
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
    //    Best-effort - never fail the apply over this. We log either way so a
    //    silent failure doesn't strand stale `.age` files alongside whatever
    //    we're about to upload; if we know we're about to forward secrets,
    //    escalate to `warn!` because we'd be writing into a dir whose state
    //    we couldn't confirm.
    if let Err(err) = clear_remote_secrets_dir(&mut ssh, &guest_secrets_dir, remote.is_root()).await
    {
        if identity_path.is_some() {
            tracing::warn!(
                ?err,
                "pre-apply secrets dir cleanup failed; new ciphertexts may \
                 land alongside leftovers from a previous run"
            );
        } else {
            tracing::debug!(?err, "pre-apply secrets dir cleanup failed");
        }
    }

    // 3. Re-encrypt secrets per-target if --identity supplied AND machine is
    //    listed in lusid-secrets.toml. Missing toml or absent [machines]
    //    entry both fall through to "no secrets forwarded" - operators can
    //    set --identity globally and only configure machines that need
    //    secrets. Plans that reference @core/secret will fail at apply time
    //    with a clear missing-secret error.
    let forward_secrets = if let Some(host_identity_path) = identity_path.as_deref() {
        match reencrypt_for_declared_machine(host_identity_path, &secrets_dir, &machine_id).await {
            Ok(reencrypted) => {
                for secret in reencrypted.iter() {
                    ssh.sync(SshVolume::FileBytes {
                        local: Cow::Owned(secret.ciphertext.clone()),
                        permissions: Some(0o600),
                        remote: format!("{guest_secrets_dir}/{}.age", secret.stem),
                    })
                    .await?;
                }
                !reencrypted.is_empty()
            }
            Err(ReencryptForTargetError::Recipients(RecipientsError::Missing { .. })) => false,
            Err(ReencryptForTargetError::UnknownMachine { machine_id }) => {
                tracing::warn!(
                    machine_id,
                    "machine not in [machines] in lusid-secrets.toml; \
                     proceeding without secrets (check --machine for typos)"
                );
                false
            }
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
    //    against between-SFTP-and-exec swaps. The embedded bytes are a
    //    `&'static [u8]` so `Cow::Borrowed` ships them to SFTP without a copy.
    let apply_bytes = Cow::Borrowed(embedded::embedded_lusid_apply(machine.arch)?);
    if remote.is_root() {
        ssh.sync(SshVolume::FileBytes {
            local: apply_bytes,
            permissions: Some(0o755),
            remote: guest_apply_path.clone(),
        })
        .await?;
    } else {
        let upload_path = format!("{REMOTE_ROOT}/lusid-apply.upload");
        ssh.sync(SshVolume::FileBytes {
            local: apply_bytes,
            permissions: Some(0o755),
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
    // TODO(cc): audit `--root` semantics for remote/dev apply. We pass the
    // operator's local path, but `lusid-apply` uses it on the guest to
    // anchor relative `host-path` resolution and the cache dir - and plans
    // typically anchor host-paths on the source span (the uploaded plan
    // file's location), making the operator-side path largely ineffective.
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
    if parse_only {
        command.push_str(" --parse-only");
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
    if forward_secrets
        && let Err(err) =
            clear_remote_secrets_dir(&mut ssh, &guest_secrets_dir, remote.is_root()).await
    {
        tracing::debug!(?err, "post-apply secrets dir cleanup failed");
    }
    if let Err(err) = ssh.disconnect().await {
        tracing::debug!(?err, "ssh disconnect failed");
    }

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
    if let Err(err) = ssh.disconnect().await {
        tracing::debug!(?err, "ssh disconnect failed");
    }

    Ok(())
}

// -- remote-apply helpers --------------------------------------------------

/// Connect to the remote SSH endpoint using the operator's private key.
async fn connect_remote(remote: &Remote) -> Result<Ssh, AppError> {
    let key_path = resolve_ssh_key_path(remote)?;
    // Classify a passphrase-protected key separately from generic load
    // failures. The hint about `ssh-keygen -p` is actionable only in that
    // specific case; surfacing it on file-not-found or garbage-bytes would
    // mislead the operator.
    let private_key = match load_private_key(&key_path).await {
        Ok(k) => k,
        Err(source) if source.is_encrypted() => {
            return Err(AppError::LoadOperatorKeyEncrypted { path: key_path });
        }
        Err(source) => {
            return Err(AppError::LoadOperatorKey {
                path: key_path,
                source,
            });
        }
    };
    let known_hosts_path = resolve_known_hosts_path()?;
    ensure_known_hosts_parent(&known_hosts_path)?;
    let host = remote.host.clone();
    let port = remote.port();
    let ssh = Ssh::connect(SshConnectOptions {
        private_key,
        addrs: (host.clone(), port),
        username: remote.user().to_owned(),
        config: Arc::new(Default::default()),
        timeout: Duration::from_secs(10),
        host_key_verification: HostKeyVerification::Tofu {
            host,
            port,
            known_hosts_path,
        },
    })
    .await?;
    Ok(ssh)
}

/// `~/.ssh/known_hosts` - OpenSSH's canonical location. No CLI override for
/// now; operators who need one can shadow `HOME` or symlink. Errors when
/// `HOME` is unset since we have nowhere sensible to default to.
fn resolve_known_hosts_path() -> Result<PathBuf, AppError> {
    let home = env::var_os("HOME")
        .filter(|h| !h.is_empty())
        .ok_or(AppError::HomeUnset)?;
    Ok(PathBuf::from(home).join(".ssh/known_hosts"))
}

/// Ensure `~/.ssh/` exists at mode `0700` so russh's `learn_known_hosts_path`
/// (which would otherwise `create_dir_all` at umask-default `0755`) doesn't
/// leave the operator with a too-permissive directory on first run. Idempotent:
/// `DirBuilder::create` with `recursive(true)` no-ops on an existing path
/// without mutating its mode, so an operator who has intentionally set a
/// different mode (e.g. `0750` for a shared system) keeps it.
fn ensure_known_hosts_parent(known_hosts_path: &Path) -> Result<(), AppError> {
    use std::os::unix::fs::DirBuilderExt;
    let Some(parent) = known_hosts_path.parent() else {
        return Ok(());
    };
    if parent.as_os_str().is_empty() {
        return Ok(());
    }
    std::fs::DirBuilder::new()
        .recursive(true)
        .mode(0o700)
        .create(parent)
        .map_err(|source| AppError::EnsureSshDir {
            path: parent.to_path_buf(),
            source,
        })
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
    // A literal `~` survived `expand_tilde` - either HOME was unset (only
    // bare `~`/`~/...` can need HOME) or the form is `~user/...` which we
    // don't expand. Distinguish the two so the error tells the operator
    // what to fix; otherwise the tilde would flow through to
    // `load_private_key` as a relative path and fail with a confusing
    // not-found.
    if raw.to_string_lossy().starts_with('~') {
        return Err(if home.is_some() {
            AppError::UnsupportedTildePath { path: raw }
        } else {
            AppError::HomeUnset
        });
    }
    Ok(raw)
}

/// Manual tilde-expansion (no `shellexpand` in the workspace; not worth a
/// new dep). Treats both bare `~` and `~/...`. Pure - `home` is passed in
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

/// Validate `<ssh-user>` against shell-injection. First character must be
/// ASCII alphanumeric or underscore (NOT dash - `chown -x` would treat the
/// value as a flag); subsequent characters add dash; total length 1..=32.
///
/// More permissive than shadow-utils' `useradd` regex (we allow uppercase and
/// leading digits) but strictly a subset of shell-safe characters: no
/// metacharacters can survive validation, so the validated string is safe to
/// interpolate raw into a shell command. Callers still apply
/// `shell_words::quote` as belt-and-suspenders defense in depth.
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
async fn ssh_run(ssh: &mut Ssh, command: &str) -> Result<(Option<u32>, String, String), AppError> {
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
/// `user` is expected to be validated by `validate_ssh_user`; we still apply
/// `shell_words::quote` as defense in depth so the shell command remains safe
/// if the validator is ever relaxed.
async fn bootstrap_remote_dirs(ssh: &mut Ssh, user: &str, root: &str) -> Result<(), AppError> {
    let cmd = format!(
        "sudo -n mkdir -p {root} {root}/plan {root}/secrets \
         && sudo -n chown {user} {root} {root}/plan {root}/secrets",
        root = shell_words::quote(root),
        user = shell_words::quote(user),
    );
    let (exit, _stdout, stderr) = ssh_run(ssh, &cmd).await?;
    if exit != Some(0) {
        return Err(AppError::BootstrapRemoteDir { exit, stderr });
    }
    Ok(())
}

/// Best-effort: clear the secrets dir on the target and recreate it empty.
/// Idempotent. Errors are not propagated (caller uses `let _ = ...`).
async fn clear_remote_secrets_dir(ssh: &mut Ssh, dir: &str, is_root: bool) -> Result<(), AppError> {
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
    parse_only: bool,
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
        // Dev VM: ephemeral host key regenerated each boot, no point pinning.
        host_key_verification: HostKeyVerification::Disabled,
    })
    .await?;

    let dev_dir = format!("/home/{}", vm.user);
    let plan_dir = plan.parent().unwrap();
    let plan_filename = plan.file_name().unwrap().to_string_lossy();

    let mut volumes = vec![
        SshVolume::FileBytes {
            local: Cow::Borrowed(embedded::embedded_lusid_apply(machine.arch)?),
            permissions: Some(0o755),
            remote: format!("{dev_dir}/lusid-apply"),
        },
        SshVolume::DirPath {
            local: plan_dir.to_path_buf(),
            remote: format!("{dev_dir}/plan"),
        },
    ];

    // Secrets forwarding. The dev VM SHADOWS the production target named
    // by `--machine`: it should see exactly the [files]-scoped subset
    // that `lusid remote apply --machine <id>` would ship, just under
    // the VM's ephemeral keypair. Same scoping as remote, different
    // cryptographic recipient (the VM's own pubkey).
    //
    // Gracefully fall through to "no secrets" when:
    //   - no --identity supplied,
    //   - lusid-secrets.toml absent (no project secrets at all).
    // Plans referencing `@resource/secret` will fail at apply time with a
    // clear missing-secret error.
    //
    // Unknown-machine is warn-logged and treated as no-secrets - a
    // typo'd --machine would otherwise silently produce a successful VM
    // boot with no secrets and an opaque plan failure later.
    let guest_identity_path = format!("{dev_dir}/identity");
    let guest_secrets_dir = format!("{dev_dir}/secrets");
    let forward_secrets = if let Some(identity_path) = identity_path.as_deref() {
        // The VM keypair: the encryption recipient (host side) AND the
        // guest's decryption identity (guest side). Re-using it avoids
        // a second keygen and a separate cloud-init injection path.
        let vm_pubkey = vm_keypair.public_openssh()?;
        match reencrypt_for_target(identity_path, &secrets_dir, &machine_id, &vm_pubkey).await {
            Ok(reencrypted) if !reencrypted.is_empty() => {
                let private_pem = vm_keypair.private_openssh()?;
                volumes.push(SshVolume::FileBytes {
                    local: Cow::Owned(private_pem.into_bytes()),
                    permissions: Some(0o600),
                    remote: guest_identity_path.clone(),
                });
                for secret in reencrypted {
                    volumes.push(SshVolume::FileBytes {
                        local: Cow::Owned(secret.ciphertext),
                        permissions: None,
                        remote: format!("{guest_secrets_dir}/{}.age", secret.stem),
                    });
                }
                true
            }
            // Machine declared but on no [files] entry - nothing to ship.
            Ok(_) => false,
            Err(ReencryptForTargetError::Recipients(RecipientsError::Missing { .. })) => false,
            Err(ReencryptForTargetError::UnknownMachine { machine_id }) => {
                tracing::warn!(
                    machine_id,
                    "machine not in [machines] in lusid-secrets.toml; \
                     proceeding without secrets (check --machine for typos)"
                );
                false
            }
            Err(other) => return Err(other.into()),
        }
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
    if parse_only {
        command.push_str(" --parse-only");
    }
    if let Some(params) = params {
        let params_json = serde_json::to_string(&params)?;
        // `shell_words::quote` correctly POSIX-escapes embedded single quotes;
        // serde_json doesn't.
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

    if let Err(err) = ssh.disconnect().await {
        tracing::debug!(?err, "ssh disconnect failed");
    }

    Ok(())
}

// `dev ssh`: boot the VM (idempotent - reuses the instance if it already
// exists) and attach the local TTY to a remote interactive shell via
// `Ssh::terminal`. No TUI, no apply - just a shell inside the guest.
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
        // Dev VM: ephemeral host key regenerated each boot, no point pinning.
        host_key_verification: HostKeyVerification::Disabled,
    })
    .await?;

    let _exit_code = ssh.terminal().await?;

    if let Err(err) = ssh.disconnect().await {
        tracing::debug!(?err, "ssh disconnect failed");
    }

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
            assert!(validate_ssh_user(bad).is_err(), "should reject {bad:?}");
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
    fn expand_tilde_user_form_passthrough() {
        // `~user/...` is not expanded; the call site detects the leading
        // tilde and surfaces `UnsupportedTildePath` so the operator sees a
        // specific error instead of a confusing not-found.
        let home = OsStr::new("/home/alice");
        assert_eq!(
            expand_tilde(Path::new("~bob/.ssh/k"), Some(home)),
            PathBuf::from("~bob/.ssh/k")
        );
    }
}
