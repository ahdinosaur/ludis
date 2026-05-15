use lusid_cmd::{Command, CommandError};
use lusid_fs::{self as fs, FsError};
use lusid_system::DiskSize;
use std::path::Path;
use thiserror::Error;

use crate::instance::VmPaths;

/// Default virtual size of the guest's root disk when the machine config
/// doesn't override it. Cloud images ship with a ~2 GB partition which fills
/// up quickly under real workloads (e.g. installing a desktop environment is
/// ~500 MB on its own). qcow2 is sparse and the backing image is shared
/// across instances, so the host only allocates blocks the guest actually
/// writes - making this effectively free until used. Cloud-init's `growpart`
/// and `resize_rootfs` modules expand the root partition and filesystem to
/// fill the disk on first boot.
const DEFAULT_OVERLAY_VIRTUAL_SIZE_BYTES: u64 = 20 * 1024 * 1024 * 1024;

#[derive(Error, Debug)]
pub enum CreateOverlayImageError {
    #[error(transparent)]
    Fs(#[from] FsError),

    #[error(transparent)]
    Command(#[from] CommandError),
}
/// Create an overlay image based on a source image
pub(super) async fn setup_overlay(
    paths: &VmPaths<'_>,
    source_image_path: &Path,
    disk_size: Option<DiskSize>,
) -> Result<(), CreateOverlayImageError> {
    let overlay_image_path = paths.overlay_image_path();

    if !fs::path_exists(&overlay_image_path).await? {
        let backing_file = format!(
            "backing_file={},backing_fmt=qcow2,nocow=on",
            source_image_path.display()
        );
        // qemu-img accepts a bare byte count as the size argument, so passing
        // raw bytes avoids any unit-suffix parsing surprises.
        let size_bytes = disk_size.map_or(DEFAULT_OVERLAY_VIRTUAL_SIZE_BYTES, u64::from);
        let size_arg = size_bytes.to_string();

        Command::new("qemu-img")
            .arg("create")
            .args(["-o", &backing_file])
            .args(["-f", "qcow2"])
            .arg(&overlay_image_path)
            .arg(&size_arg)
            .run()
            .await?;
    }

    Ok(())
}
