//! Simple hardware primitives (CPU count, memory size, disk size) used by the
//! `vm` crate. Kept deliberately minimal - just typed newtypes around `u16` /
//! `u64`.

use std::fmt::Display;

use serde::{Deserialize, Serialize};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct CpuCount(u16);

impl CpuCount {
    pub fn new(count: u16) -> Self {
        Self(count)
    }
}

impl Display for CpuCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct MemorySize(u64); // In bytes

impl MemorySize {
    pub fn new(size: u64) -> Self {
        Self(size)
    }
}

impl Display for MemorySize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl From<MemorySize> for u64 {
    fn from(value: MemorySize) -> Self {
        value.0
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct DiskSize(u64); // In bytes

impl DiskSize {
    pub fn new(size: u64) -> Self {
        Self(size)
    }
}

impl Display for DiskSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl From<DiskSize> for u64 {
    fn from(value: DiskSize) -> Self {
        value.0
    }
}
