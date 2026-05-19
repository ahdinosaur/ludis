use lusid_ctx::Paths as BasePaths;
use std::path::{Path, PathBuf};
use thiserror::Error;
use which::which_global;

/// Base paths specialised for this crate. Cacheable downloads (guest images +
/// sums) live under `cache_dir/vm/images`; per-VM mutable state (overlays,
/// OVMF vars, kernel, cloud-init, keypair, pid) lives under
/// `data_dir/vm/instances/<id>`.
#[derive(Debug, Clone)]
pub struct Paths {
    base: BasePaths,
}

impl Paths {
    pub fn new(base: BasePaths) -> Self {
        Self { base }
    }

    pub fn images_dir(&self) -> PathBuf {
        self.base.cache_dir().join("vm/images")
    }

    pub fn image_file(&self, image_file_name: &str) -> PathBuf {
        self.images_dir().join(image_file_name)
    }

    pub fn instances_dir(&self) -> PathBuf {
        self.base.data_dir().join("vm/instances")
    }

    pub fn instance_dir(&self, instance_id: &str) -> PathBuf {
        self.instances_dir().join(instance_id)
    }
}

#[derive(Error, Debug)]
#[error(transparent)]
pub struct ExecutablePathsError(#[from] which::Error);

/// Absolute paths to the external binaries the VM pipeline shells out to.
/// Resolved once via `$PATH` at [`Context`](crate::context::Context)
/// construction; [`ExecutablePaths::new`] fails fast if any are missing so
/// the user gets a clear install-missing-deps error rather than a runtime
/// ENOENT deep inside setup.
#[derive(Debug, Clone)]
pub struct ExecutablePaths {
    qemu_x86_64: PathBuf,
    qemu_aarch64: PathBuf,
    qemu_img: PathBuf,
    mkisofs: PathBuf,
}

impl ExecutablePaths {
    pub fn new() -> Result<ExecutablePaths, ExecutablePathsError> {
        let qemu_x86_64 = which_global("qemu-system-x86_64")?;
        let qemu_aarch64 = which_global("qemu-system-aarch64")?;
        let qemu_img = which_global("qemu-img")?;
        let mkisofs = which_global("mkisofs")?;

        Ok(ExecutablePaths {
            qemu_x86_64,
            qemu_aarch64,
            qemu_img,
            mkisofs,
        })
    }

    pub fn qemu_x86_64(&self) -> &Path {
        &self.qemu_x86_64
    }

    pub fn qemu_aarch64(&self) -> &Path {
        &self.qemu_aarch64
    }

    pub fn qemu_img(&self) -> &Path {
        &self.qemu_img
    }

    pub fn mkisofs(&self) -> &Path {
        &self.mkisofs
    }
}
