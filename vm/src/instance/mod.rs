mod paths;
mod setup;
mod start;

use self::paths::*;
use self::setup::*;
use self::start::*;

use lusid_ctx::Context as BaseContext;
use lusid_fs::{self as fs, FsError};
use lusid_machine::Machine;
use lusid_ssh::{SshKeypair, SshKeypairError};
use lusid_system::{Arch, CpuCount, DiskSize, Linux, MemorySize};
use nix::{
    sys::signal::{Signal, kill},
    unistd::Pid,
};
use serde::{Deserialize, Serialize};
use std::num::ParseIntError;
use std::time::Duration;
use std::{fmt::Display, net::Ipv4Addr, path::PathBuf, str::FromStr};
use thiserror::Error;
use tokio::time::sleep;

use crate::{
    context::{Context, ContextError},
    utils::is_tcp_port_open,
};

/// Inputs for [`Vm::run`]: which instance to (re)use, what [`Machine`] image
/// to run, and any additional guest ports to forward beyond SSH.
pub struct VmOptions<'a> {
    pub instance_id: &'a str,
    pub machine: &'a Machine,
    pub ports: Vec<VmPort>,
}

#[derive(Error, Debug)]
pub enum VmError {
    #[error(transparent)]
    Context(#[from] ContextError),

    #[error(transparent)]
    Setup(#[from] VmSetupError),

    #[error(transparent)]
    Start(#[from] VmStartError),

    #[error("failed to load ssh keypair")]
    LoadSshKeypair(#[source] SshKeypairError),

    #[error("failed to check whether instance dir exists")]
    DirExists(#[source] fs::FsError),

    #[error("failed to serialize or deserialize state")]
    StateSerde(#[source] serde_json::Error),

    #[error("failed to read state")]
    StateRead(#[source] fs::FsError),

    #[error("failed to write state")]
    StateWrite(#[source] fs::FsError),

    #[error("failed to remove instance dir")]
    RemoveDir(#[source] fs::FsError),

    #[error("failed to read pid")]
    ReadPid(#[source] FsError),

    #[error("failed to parse pid")]
    ParsePid(#[source] ParseIntError),

    #[error("failed to kill pid")]
    KillPid(#[source] nix::errno::Errno),
}

/// A configured (and usually running) VM instance. Serialized to
/// `<instance_dir>/state.json` so that re-running with the same `instance_id`
/// picks up the same overlay, keypair, and forwarded SSH port.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Vm {
    pub id: String,
    pub dir: PathBuf,
    pub arch: Arch,
    pub linux: Linux,
    /// Default SSH login user (e.g. `debian`); image-dependent.
    pub user: String,
    /// Host-side TCP port forwarded to the guest's port 22. Chosen once at
    /// setup time via [`get_free_tcp_port`](crate::utils::get_free_tcp_port)
    /// and persisted so reruns stay reachable at the same address.
    pub ssh_port: u16,
    pub memory_size: Option<MemorySize>,
    pub cpu_count: Option<CpuCount>,
    /// Virtual size of the overlay qcow2, applied at first-create time by
    /// `setup_overlay`. `None` falls back to the crate default.
    pub disk_size: Option<DiskSize>,
    pub ports: Vec<VmPort>,
    pub graphics: Option<bool>,
    pub kvm: Option<bool>,
    /// Image-supplied shell snippet to run over SSH after the guest's port 22
    /// opens, before the caller drives the VM. Lets per-image defaults drain
    /// first-boot races (cloud-init `packages:`, mirror refreshers) that
    /// hold OS-level package locks. Idempotent on re-runs.
    pub ready_check: Option<String>,
}

impl Vm {
    /// Ensure a VM is set up, running, and accepting SSH.
    ///
    /// Idempotent: if `<instance_dir>/state.json` already exists it is loaded
    /// instead of rebuilt, and qemu is only spawned if no `qemu.pid` is
    /// present. Blocks until `127.0.0.1:<ssh_port>` answers.
    pub async fn run(ctx: &mut BaseContext, options: VmOptions<'_>) -> Result<Vm, VmError> {
        let mut ctx = Context::create(ctx)?;

        let VmOptions {
            instance_id,
            machine,
            ports,
        } = options;

        let instance = if Vm::exists(&mut ctx, instance_id).await? {
            Vm::load(&mut ctx, instance_id).await?
        } else {
            let setup_options = VmSetupOptions {
                instance_id,
                machine,
                ports,
            };
            let inst = Vm::setup(&mut ctx, setup_options).await?;
            inst.save().await?;
            inst
        };

        if !instance.is_qemu_running().await? {
            instance.start(&mut ctx).await?;

            loop {
                if instance.is_ssh_open() {
                    break;
                }

                sleep(Duration::from_millis(100)).await;
            }
        }

        Ok(instance)
    }

    fn paths(&self) -> VmPaths<'_> {
        VmPaths::new(&self.dir)
    }

    async fn setup(ctx: &mut Context, options: VmSetupOptions<'_>) -> Result<Self, VmError> {
        Ok(setup_instance(ctx, options).await?)
    }

    async fn exists(ctx: &mut Context, instance_id: &str) -> Result<bool, VmError> {
        let instance_dir = ctx.paths().instance_dir(instance_id);
        let exists = fs::path_exists(instance_dir)
            .await
            .map_err(VmError::DirExists)?;
        Ok(exists)
    }

    async fn load(ctx: &mut Context, instance_id: &str) -> Result<Self, VmError> {
        let instance_dir = ctx.paths().instance_dir(instance_id);
        let paths = VmPaths::new(&instance_dir);
        let state_path = paths.state();
        let state_str = fs::read_file_to_string(state_path)
            .await
            .map_err(VmError::StateRead)?;
        let instance = serde_json::from_str(&state_str).map_err(VmError::StateSerde)?;
        Ok(instance)
    }

    async fn save(&self) -> Result<(), VmError> {
        let state_path = self.paths().state();
        let state = serde_json::to_string_pretty(self).map_err(VmError::StateSerde)?;
        fs::write_file(state_path, state.as_bytes())
            .await
            .map_err(VmError::StateWrite)?;
        Ok(())
    }

    /// Delete the instance directory (overlay, OVMF vars, cloud-init, keypair,
    /// state). The caller is responsible for [`stop`](Self::stop)ping qemu
    /// first; removing a running instance's dir is undefined behaviour.
    pub async fn remove(self) -> Result<(), VmError> {
        fs::remove_dir(self.dir).await.map_err(VmError::RemoveDir)?;
        Ok(())
    }

    async fn start(&self, ctx: &mut Context) -> Result<(), VmError> {
        Ok(instance_start(ctx.executables(), self).await?)
    }

    async fn is_qemu_running(&self) -> Result<bool, VmError> {
        let pid_exists = fs::path_exists(&self.paths().qemu_pid_path())
            .await
            .map_err(VmError::ReadPid)?;
        Ok(pid_exists)
    }

    fn is_ssh_open(&self) -> bool {
        is_tcp_port_open(self.ssh_port)
    }

    /// Kill the qemu process via `SIGKILL` on the pid stored in `qemu.pid`.
    ///
    /// Note(cc): this is an unconditional hard kill - no `SIGTERM` grace
    /// period, no QMP `system_powerdown`. Fine for the dev workflow (overlay
    /// disk absorbs the damage), but worth revisiting if guests ever carry
    /// state worth flushing.
    pub async fn stop(&self) -> Result<(), VmError> {
        let pid_str = fs::read_file_to_string(&self.paths().qemu_pid_path())
            .await
            .map_err(VmError::ReadPid)?;
        let pid_int: i32 = FromStr::from_str(&pid_str).map_err(VmError::ParsePid)?;
        let pid = Pid::from_raw(pid_int);
        kill(pid, Some(Signal::SIGKILL)).map_err(VmError::KillPid)?;
        Ok(())
    }

    /// Load (or create on first call) the instance's ed25519 SSH keypair from
    /// `<dir>/id_ed25519[.pub]`. Matches the public key seeded into the guest
    /// by cloud-init, so SSH connections authenticate out of the box.
    pub async fn ssh_keypair(&self) -> Result<SshKeypair, VmError> {
        SshKeypair::load_or_create(&self.dir)
            .await
            .map_err(VmError::LoadSshKeypair)
    }
}

/// A host→guest TCP forward translated into a QEMU `hostfwd` rule. An omitted
/// `host_ip` binds `0.0.0.0`; an omitted `host_port` reuses `vm_port`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VmPort {
    pub host_ip: Option<Ipv4Addr>,
    pub host_port: Option<u16>,
    pub vm_port: u16,
}

impl Display for VmPort {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut wrote_left = false;
        if let Some(ip) = self.host_ip {
            write!(f, "{}", ip)?;
            wrote_left = true;
        }
        if let Some(port) = self.host_port {
            if wrote_left {
                write!(f, ":")?;
            }
            write!(f, "{}", port)?;
            wrote_left = true;
        }
        if wrote_left {
            write!(f, "->")?;
        }
        write!(f, "{}/tcp", self.vm_port)
    }
}
