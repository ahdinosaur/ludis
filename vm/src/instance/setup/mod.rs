//! One-time per-instance setup: download the guest image, build the overlay,
//! extract the kernel, make UEFI vars writeable, mint SSH keys, seed
//! cloud-init. Each sub-step is idempotent (skips work if the output file
//! already exists), so partial runs can be resumed by re-invoking.

mod cloud_init;
mod kernel;
mod overlay;
mod ovmf;

use lusid_fs::{self as fs, FsError};
use lusid_machine::{Machine, MachineVmOptions};
use lusid_ssh::{SshKeypair, SshKeypairError};
use thiserror::Error;

use crate::utils::get_free_tcp_port;
use crate::{
    context::Context,
    image::{VmImage, VmImageError, get_image},
    instance::{
        Vm, VmPaths, VmPort,
        setup::{
            cloud_init::{CloudInitError, setup_cloud_init},
            kernel::{ExtractKernelError, VmKernelDetails, setup_kernel},
            overlay::{CreateOverlayImageError, setup_overlay},
            ovmf::{ConvertOvmfVarsError, setup_ovmf_uefi_variables},
        },
    },
};

pub struct VmSetupOptions<'a> {
    pub instance_id: &'a str,
    pub machine: &'a Machine,
    pub ports: Vec<VmPort>,
}

#[derive(Error, Debug)]
pub enum VmSetupError {
    #[error(transparent)]
    Image(#[from] VmImageError),

    #[error(transparent)]
    ConvertOvmfVars(#[from] ConvertOvmfVarsError),

    #[error(transparent)]
    ExtractKernel(#[from] ExtractKernelError),

    #[error(transparent)]
    CreateOverlayImage(#[from] CreateOverlayImageError),

    #[error(transparent)]
    CloudInit(#[from] CloudInitError),

    #[error(transparent)]
    Fs(#[from] FsError),

    #[error(transparent)]
    SshKeypair(#[from] SshKeypairError),

    #[error("no open ports available")]
    NoOpenPortsAvailable,
}

/// Do everything needed to take a fresh instance id to a bootable (but not yet
/// running) [`Vm`]. Allocates the host-side SSH forwarding port with
/// [`get_free_tcp_port`] - there is an inherent TOCTOU here (another process
/// could grab the port between release and qemu binding it), but in practice
/// picking at spawn time and binding immediately is close enough.
pub async fn setup_instance(
    ctx: &mut Context,
    options: VmSetupOptions<'_>,
) -> Result<Vm, VmSetupError> {
    let VmSetupOptions {
        instance_id,
        machine,
        ports,
    } = options;

    let source_image = get_image(ctx, machine).await?;
    let MachineVmOptions {
        memory_size,
        cpu_count,
        disk_size,
        graphics,
    } = machine.vm.clone().unwrap_or_default();

    let VmImage {
        arch,
        linux,
        image_path: source_image_path,
        kernel_root,
        user,
    } = source_image;

    let instance_dir = ctx.paths().instance_dir(instance_id);
    fs::setup_directory_access(&instance_dir).await?;

    let executables = ctx.executables();
    let instance_paths = VmPaths::new(&instance_dir);

    setup_overlay(&instance_paths, &source_image_path, disk_size).await?;
    setup_ovmf_uefi_variables(executables, &instance_paths).await?;

    let VmKernelDetails { has_initrd } =
        setup_kernel(executables, &instance_paths, &source_image_path).await?;

    let ssh_keypair = SshKeypair::load_or_create(&instance_dir).await?;
    let ssh_port = get_free_tcp_port().ok_or(VmSetupError::NoOpenPortsAvailable)?;

    setup_cloud_init(
        executables,
        &instance_paths,
        instance_id,
        &machine.hostname,
        &ssh_keypair.public_key,
    )
    .await?;

    Ok(Vm {
        id: instance_id.to_owned(),
        dir: instance_dir,
        arch,
        linux,
        kernel_root,
        user,
        has_initrd,
        ssh_port,
        memory_size,
        cpu_count,
        disk_size,
        ports,
        graphics,
        // TODO(cc): plumb through global lusid config so KVM can be disabled
        // on hosts without `/dev/kvm` access. `instance_start` currently
        // defaults this to `true`, which will fail on such hosts.
        kvm: None,
    })
}
