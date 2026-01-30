use lusid_system::{Arch, Os};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VmImageIndex {
    pub arch: Arch,
    pub os: Os,
    pub image: VmImageRef,
    pub hash: VmImageHashRef,
    pub kernel_root: String,
    pub user: String,
}

impl VmImageIndex {
    pub fn to_image_file_name(&self) -> String {
        let arch = &self.arch;
        let os = &self.os;
        let ext = self.image.to_extension();
        format!("{arch}_{os}.{ext}")
    }
    pub fn to_hash_file_name(&self) -> String {
        let arch = &self.arch;
        let os = &self.os;
        let ext = self.hash.to_extension();
        format!("{arch}_{os}.{ext}")
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum VmImageRef {
    #[serde(rename = "qcow2")]
    Qcow2 { url: String },
}

impl VmImageRef {
    pub fn to_url(&self) -> &str {
        match self {
            VmImageRef::Qcow2 { url } => url,
        }
    }
    fn to_extension(&self) -> &str {
        match self {
            VmImageRef::Qcow2 { url: _ } => "qcow2",
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum VmImageHashRef {
    #[serde(rename = "sha512sums")]
    Sha512Sums { url: String },
    #[serde(rename = "sha256sums")]
    Sha256Sums { url: String },
}

impl VmImageHashRef {
    pub fn to_url(&self) -> &str {
        match self {
            VmImageHashRef::Sha512Sums { url } => url,
            VmImageHashRef::Sha256Sums { url } => url,
        }
    }
    fn to_extension(&self) -> &str {
        match self {
            VmImageHashRef::Sha512Sums { url: _ } => "sha512sums",
            VmImageHashRef::Sha256Sums { url: _ } => "sha256sums",
        }
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct VmImagesList(HashMap<String, VmImageIndex>);

impl VmImagesList {
    pub fn into_values(self) -> impl Iterator<Item = VmImageIndex> {
        self.0.into_values()
    }
}
