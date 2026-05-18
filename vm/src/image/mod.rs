//! Guest image catalogue, download, and hash verification.
//!
//! The catalogue is a static `images.toml` (compiled in via `include_str!`)
//! mapping `(arch, os)` pairs to a download URL and a checksum URL (SHA-256
//! or SHA-512 SUMS file). [`get_image`] picks the row matching the requested
//! [`Machine`], downloads both files into `cache_dir/vm/images/` if absent,
//! verifies the hash, and returns a [`VmImage`] pointing at the cached file.

use lusid_fs::{self as fs, FsError};
use lusid_http::HttpError;
use lusid_machine::Machine;
use lusid_system::{Arch, Linux, Os};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use thiserror::Error;
use tracing::info;

mod hash;
mod index;

use crate::{
    context::Context,
    image::{
        hash::{VmImageHash, VmImageHashError},
        index::{VmImageIndex, VmImagesList},
    },
    paths::Paths,
};

#[derive(Error, Debug)]
pub enum VmImageError {
    #[error("Failed to load image cache: {0}")]
    CacheLoad(#[from] toml::de::Error),

    #[error(transparent)]
    Hash(#[from] VmImageHashError),

    #[error(transparent)]
    Http(#[from] HttpError),

    #[error(transparent)]
    Fs(#[from] FsError),
}

pub async fn get_images_list() -> Result<VmImagesList, VmImageError> {
    let images_str = include_str!("../../images.toml");
    let images_list: VmImagesList = toml::from_str(images_str)?;
    Ok(images_list)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VmImage {
    pub arch: Arch,
    pub linux: Linux,
    pub image_path: PathBuf,
    pub kernel_root: String,
    pub user: String,
    pub ready_check: Option<String>,
}

impl VmImage {
    // Note(cc): VMs are Linux-only today. The `Os::Linux(_)` narrowing is
    // deliberate - the `_ => unimplemented!()` arm would fire if `images.toml`
    // ever lists a non-Linux `os:` value, which isn't a supported state. If
    // FreeBSD/etc. guests are ever added, this needs a real error path.
    pub fn new(paths: &Paths, image_index: VmImageIndex) -> Self {
        let image_path = paths.image_file(&image_index.to_image_file_name());
        let VmImageIndex {
            arch,
            os,
            image: _,
            hash: _,
            kernel_root,
            user,
            ready_check,
        } = image_index;
        match os {
            Os::Linux(linux) => VmImage {
                arch,
                linux,
                image_path,
                kernel_root,
                user,
                ready_check,
            },
            _ => {
                unimplemented!()
            }
        }
    }
}

pub async fn get_image(ctx: &mut Context, machine: &Machine) -> Result<VmImage, VmImageError> {
    let image_index = find_image_index_for_machine(machine).await?;

    // TODO(cc): turn this into a proper `VmImageError::NoMatchingImage { arch,
    // os }` variant. As-is, an unsupported (arch, os) pair from user config
    // crashes the process with a generic panic message that doesn't even name
    // the offending machine.
    let Some(image_index) = image_index else {
        panic!("Unable to find matching image for machine");
    };

    info!("image: {:?}", image_index);

    info!("fetching...");

    fetch_image(ctx, &image_index).await?;

    info!("fetched.");

    let image = get_image_from_index(ctx, image_index);

    Ok(image)
}

async fn find_image_index_for_machine(
    machine: &Machine,
) -> Result<Option<VmImageIndex>, VmImageError> {
    let images_list = get_images_list().await?;
    let image_index = images_list
        .into_values()
        .find(|image_index| image_index.os == machine.os && image_index.arch == machine.arch);
    Ok(image_index)
}

async fn fetch_image(ctx: &mut Context, image_index: &VmImageIndex) -> Result<(), VmImageError> {
    let image_path = ctx.paths().image_file(&image_index.to_image_file_name());

    fs::setup_directory_access(ctx.paths().images_dir()).await?;

    ctx.http_client()
        .download_file(image_index.image.to_url(), &image_path)
        .await?;

    let hash_path = ctx.paths().image_file(&image_index.to_hash_file_name());

    ctx.http_client()
        .download_file(image_index.hash.to_url(), &hash_path)
        .await?;

    let hash = VmImageHash::new(&image_index.hash, &hash_path);
    hash.validate(image_index, &image_path).await?;

    Ok(())
}

fn get_image_from_index(ctx: &mut Context, image_index: VmImageIndex) -> VmImage {
    VmImage::new(ctx.paths(), image_index)
}
