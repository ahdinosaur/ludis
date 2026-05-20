//! Per-instance file layout.
//!
//! Everything a VM needs at runtime lives side-by-side under
//! `<instance_dir>/` so `rm -rf` (via [`Vm::remove`](super::Vm::remove)) is
//! sufficient to discard the instance:
//!
//! ```text
//! <instance_dir>/
//!   state.json              - serialized `Vm` (see `super::Vm`)
//!   overlay.qcow2           - writeable overlay, backed by the cached image
//!   OVMF_VARS.4m.fd.qcow2   - per-VM UEFI NVRAM (qcow2 for snapshotability)
//!   cloud-init-{meta,user}-data, cloud-init.iso - seed ISO for first boot
//!   id_ed25519[.pub]        - SSH keypair (written by lusid_ssh::SshKeypair)
//!   qemu.pid                - pid of the daemonized qemu process
//!   qmp.sock                - QMP control socket (currently unused by lusid)
//! ```
//!
//! `ovmf_*_system_path` point at the read-only firmware files shipped by the
//! host distro (see crate README for package names).

use std::path::{Path, PathBuf};
use std::sync::LazyLock;

/// Resolver for files under a specific instance directory. Cheap to build
/// (just borrows the path); produced ad-hoc by [`Vm::paths`](super::Vm::paths).
pub struct VmPaths<'a> {
    instance_dir: &'a Path,
}

impl<'a> VmPaths<'a> {
    pub fn new(instance_dir: &'a Path) -> Self {
        Self { instance_dir }
    }

    pub fn state(&self) -> PathBuf {
        self.instance_dir.join("state.json")
    }

    pub fn overlay_image_path(&self) -> PathBuf {
        self.instance_dir.join("overlay.qcow2")
    }

    // TODO(cc): x86-64-only. aarch64 wants `AAVMF_{CODE,VARS}_4M.fd` (Debian
    // `qemu-efi-aarch64` package). Pick by `Vm::arch` instead of hardcoding.
    pub fn ovmf_vars_system_path(&self) -> &Path {
        static OVMF_VARS_SYSTEM_FILE: LazyLock<PathBuf> =
            LazyLock::new(|| PathBuf::from("/usr/share/OVMF/OVMF_VARS_4M.fd"));

        OVMF_VARS_SYSTEM_FILE.as_path()
    }

    pub fn ovmf_vars_path(&self) -> PathBuf {
        self.instance_dir.join("OVMF_VARS.4m.fd.qcow2")
    }

    pub fn ovmf_code_system_path(&self) -> &Path {
        static OVMF_CODE_SYSTEM_FILE: LazyLock<PathBuf> =
            LazyLock::new(|| PathBuf::from("/usr/share/OVMF/OVMF_CODE_4M.fd"));

        OVMF_CODE_SYSTEM_FILE.as_path()
    }

    pub fn cloud_init_meta_data_path(&self) -> PathBuf {
        self.instance_dir.join("cloud-init-meta-data")
    }

    pub fn cloud_init_user_data_path(&self) -> PathBuf {
        self.instance_dir.join("cloud-init-user-data")
    }

    pub fn cloud_init_image_path(&self) -> PathBuf {
        self.instance_dir.join("cloud-init.iso")
    }

    pub fn qemu_pid_path(&self) -> PathBuf {
        self.instance_dir.join("qemu.pid")
    }

    pub fn qemu_qmp_socket_path(&self) -> PathBuf {
        self.instance_dir.join("qmp.sock")
    }
}
