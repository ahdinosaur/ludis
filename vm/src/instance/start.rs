//! Spawn qemu for a prepared [`Vm`]. Assembles the argv via [`Qemu`] and
//! launches it daemonized; the pid ends up in `<instance_dir>/qemu.pid` where
//! [`Vm::stop`](super::Vm::stop) can find it later.
//!
//! OVMF firmware boots the overlay's ESP via the UEFI removable-media fallback
//! path (`\EFI\BOOT\BOOTX64.EFI`), so the guest's own bootloader picks up the
//! kernel and initramfs from its `/boot`. This survives in-guest kernel
//! upgrades, which is the whole point of doing it this way.
//!
//! Defaults when the [`Vm`]'s optional fields are `None`: 8 GiB memory, 2
//! CPUs, graphics on, KVM on. The SSH forward is always prepended to the
//! caller-supplied `ports` so it survives any reordering.

use lusid_system::{CpuCount, MemorySize};
use std::net::Ipv4Addr;
use thiserror::Error;

use crate::{
    instance::{Vm, VmPort},
    paths::ExecutablePaths,
    qemu::{Qemu, QemuError},
};

#[derive(Error, Debug)]
pub enum VmStartError {
    #[error(transparent)]
    Qemu(#[from] QemuError),
}

pub(super) async fn instance_start(
    executables: &ExecutablePaths,
    instance: &Vm,
) -> Result<(), VmStartError> {
    let Vm {
        id: _instance_id,
        dir: _instance_dir,
        arch,
        linux: _,
        user: _,
        ssh_port,
        memory_size,
        cpu_count,
        disk_size: _,
        ports,
        graphics,
        kvm,
        ready_check: _,
    } = instance;
    let paths = instance.paths();

    let other_ports = ports.clone();
    let mut ports = vec![VmPort {
        host_ip: Some(Ipv4Addr::LOCALHOST),
        host_port: Some(*ssh_port),
        vm_port: 22,
    }];
    ports.extend(other_ports);

    let memory_size = memory_size.unwrap_or_else(|| MemorySize::new(8 * 1024 * 1024 * 1024));
    let memory_size_in_gb: u64 = u64::from(memory_size) / 1024 / 1024 / 1024;
    let cpu_count = cpu_count.unwrap_or_else(|| CpuCount::new(2));
    let graphics = graphics.unwrap_or(true);
    let kvm = kvm.unwrap_or(true);

    let qemu_executable = match arch {
        lusid_system::Arch::X86_64 => executables.qemu_x86_64(),
        lusid_system::Arch::Aarch64 => executables.qemu_aarch64(),
    };
    let mut qemu = Qemu::new(qemu_executable);

    qemu.easy()
        .cpu_count(cpu_count.to_string())
        .memory(memory_size_in_gb)
        .plash_drives(paths.ovmf_code_system_path(), &paths.ovmf_vars_path());

    qemu.qmp_socket(&paths.qemu_qmp_socket_path())
        .kvm(kvm)
        .pid_file(paths.qemu_pid_path())
        .graphics(graphics)
        .ports(&ports);

    // Overlay (bootable) and cloud-init seed (data-only). `bootindex=0`
    // tells OVMF to scan the overlay's ESP first; cloud-init.iso has no EFI
    // bootloader so it'd be skipped anyway, but being explicit avoids future
    // surprises if the ISO ever grows an El-Torito EFI entry or the drive
    // order changes.
    qemu.virtio_drive(
        "overlay-disk",
        "qcow2",
        &paths.overlay_image_path(),
        Some(0),
    )
    .virtio_drive(
        "cloud-init",
        "raw",
        &paths.cloud_init_image_path(),
        None,
    );

    tracing::debug!(cmd = ?qemu, "spawning QEMU");

    let _child = qemu.spawn().await?;

    tracing::info!(
        arch=?arch,
        memory_gb=memory_size_in_gb,
        cpus=%cpu_count,
        ssh_port=%ssh_port,
        graphics=graphics,
        kvm=kvm,
        "VM process started"
    );

    Ok(())
}
