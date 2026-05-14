//! The `lusid` CLI: user-facing front door for applying plans to local,
//! remote, and VM-dev targets.
//!
//! Architecture: the CLI doesn't run the apply pipeline in-process. It
//! spawns [`lusid-apply`](lusid_apply) (either locally for `local apply`,
//! or inside a dev VM over SSH for `dev apply`) and pipes its stdout JSON
//! into the [`tui`] module to render a live pipeline view. stderr is
//! buffered and shown on a separate pane.
//!
//! ## Subcommands
//!
//! - `machines list` — table of all machines in `lusid.toml`.
//! - `local apply` — apply the machine matching `$(hostname)` to this host.
//! - `remote apply`/`ssh` — **unimplemented**, `todo!()` today.
//! - `dev apply`/`ssh` — spin up a local QEMU VM (via [`lusid-vm`]), SFTP
//!   the plan + `lusid-apply` binary into it, and run apply over SSH (or
//!   open an interactive shell).

mod config;
mod embedded;
mod tui;

use std::{env, net::Ipv4Addr, path::PathBuf, sync::Arc, time::Duration};

use clap::{Parser, Subcommand};
use lusid_apply_stdio::AppViewError;
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_secrets::cli::{CliEnv as SecretsCliEnv, CliError as SecretsCliError, SecretsCommand};
use lusid_secrets::{RecipientsError, ReencryptForTargetError, reencrypt_for_target};
use lusid_ssh::{Ssh, SshConnectOptions, SshError, SshKeypairError, SshVolume};
use lusid_system::Arch;
use lusid_vm::{Vm, VmError, VmOptions};
use thiserror::Error;
use tracing::error;
use which::which;

use crate::config::{Config, ConfigError, MachineConfig};
use crate::embedded::EmbeddedError;
use crate::tui::{TuiError, tui};

/// Parsed CLI. `lusid_apply_linux_*_path` override the embedded `lusid-apply`
/// worker for a given target arch — useful when iterating on `lusid-apply`
/// itself without rebuilding `lusid` to re-embed. Each falls back to the
/// matching `lusid.toml` field, then to the binary baked in at build time
/// (see [`crate::embedded`]).
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

    #[arg(
        long = "lusid-apply-linux-x86-64",
        env = "LUSID_APPLY_LINUX_X86_64",
        global = true
    )]
    pub lusid_apply_linux_x86_64_path: Option<String>,

    #[arg(
        long = "lusid-apply-linux-aarch64",
        env = "LUSID_APPLY_LINUX_AARCH64",
        global = true
    )]
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
    ReencryptSecrets(#[from] ReencryptForTargetError),

    #[error("failed to serialize VM SSH keypair: {0}")]
    SshKeypair(#[from] SshKeypairError),

    #[error(transparent)]
    Embedded(#[from] EmbeddedError),
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
            RemoteCmd::Apply { machine_id } => cmd_remote_apply(config, machine_id).await,
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
    let MachineConfig { plan, params, .. } = config.local_machine()?;

    let apply_bin = resolve_local_apply_path(&config, Arch::get()).await?;

    let mut command = Command::new(&apply_bin);
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

// TODO(cc): implement remote apply/ssh. Expected shape: resolve the machine
// from config, connect to its hostname over SSH (using either agent auth or
// a configured key), upload the plan + lusid-apply binary, run apply, and
// pipe through the TUI — essentially `cmd_dev_*` without the VM bring-up.
//
// Secrets strategy: mirror `cmd_dev_apply`'s per-target re-encryption, with
// two substitutions:
//   - Recipient key is the target machine's entry in `lusid-secrets.toml`'s
//     `[machines]` table — looked up by `machine_id` — rather than an
//     ephemeral VM auth key. `lusid_secrets` will need a small helper to
//     expose that lookup; the CLI doesn't have `Recipients` access directly.
//   - Guest identity is the target's existing
//     `/etc/ssh/ssh_host_ed25519_key` on the machine itself — nothing is
//     SFTP'd for the identity; just pass `--identity=/etc/ssh/ssh_host_ed25519_key`
//     (plus `--guest-mode --secrets-dir=...`). Requires the guest
//     `lusid-apply` to run as root, which it typically does already.
async fn cmd_remote_apply(_config: Config, _machine_id: String) -> Result<(), AppError> {
    todo!()
}

async fn cmd_remote_ssh(_config: Config, _machine_id: String) -> Result<(), AppError> {
    todo!()
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
    let apply_volume =
        resolve_apply_volume(&config, machine.arch, format!("{dev_dir}/lusid-apply"))?;

    let mut volumes = vec![
        apply_volume,
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
    // Plans referencing `@core/secret` will fail at apply time with a
    // clear missing-secret error.
    //
    // Unknown-machine is warn-logged and treated as no-secrets — a
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
            }
            // Machine declared but on no [files] entry — nothing to ship.
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
    if let Some(params) = params {
        let params_json = serde_json::to_string(&params)?;
        command.push_str(&format!(" --params '{params_json}'"));
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

/// Pick the `lusid-apply` override path for `arch` from `Config`, if any.
///
/// Returning `None` means the caller should fall through to the embedded
/// binary for that arch.
fn override_apply_path(config: &Config, arch: Arch) -> Option<&str> {
    match arch {
        Arch::X86_64 => config.lusid_apply_linux_x86_64_path.as_deref(),
        Arch::Aarch64 => config.lusid_apply_linux_aarch64_path.as_deref(),
    }
}

/// Resolve a `lusid-apply` binary on disk for `local apply` to spawn as a
/// subprocess. Prefers the override (resolved through `which`, accepting
/// both literal paths and bare names on `$PATH`); otherwise extracts the
/// embedded binary into the user cache and returns that path.
async fn resolve_local_apply_path(config: &Config, arch: Arch) -> Result<PathBuf, AppError> {
    if let Some(p) = override_apply_path(config, arch) {
        return Ok(which(p)?);
    }
    Ok(embedded::resolve_or_extract_for_arch(arch).await?)
}

/// Pick the `SshVolume` that carries `lusid-apply` to a remote target of the
/// given `arch`. Override paths are uploaded by path (preserves dev
/// iteration); the embedded binary is uploaded straight from memory via
/// `SshVolume::FileBytes`.
fn resolve_apply_volume(
    config: &Config,
    arch: Arch,
    remote: String,
) -> Result<SshVolume, AppError> {
    if let Some(p) = override_apply_path(config, arch) {
        return Ok(SshVolume::FilePath {
            local: which(p)?,
            remote,
        });
    }
    let bytes = embedded::embedded_lusid_apply(arch)?.to_vec();
    Ok(SshVolume::FileBytes {
        local: bytes,
        permissions: Some(0o755),
        remote,
    })
}
