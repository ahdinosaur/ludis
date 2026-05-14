//! CPU architecture detection via Rust's `cfg(target_arch)`.
//!
//! The [`Bitness`] type is currently only `64-bit`; kept around so future 32-bit or
//! other width categorizations slot in without changing consumers.
// Note(cc): `Bitness` is defined here but not referenced anywhere else in the
// workspace. Delete if no use materializes soon.

use std::fmt::Display;

use serde::{Deserialize, Serialize};

/// Adding a new variant: also update `lusid/build.rs::ARCHES` (the build-time
/// embed list — separate from this enum because `build.rs` can't depend on
/// `lusid-system` without a slow build-dep), and the match in
/// `lusid/src/embedded.rs::embedded_lusid_apply` (exhaustive over `Arch`, so
/// the compiler flags missing arms).
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Arch {
    #[serde(rename = "x86-64")]
    X86_64,
    #[serde(rename = "aarch64")]
    Aarch64,
}

impl Arch {
    /// All supported architectures, in canonical order. Use for iteration in
    /// docs, CLI help, and tests that need to round-trip every variant.
    pub const fn all() -> &'static [Arch] {
        &[Arch::X86_64, Arch::Aarch64]
    }

    /// Suffix used in cfg names and env vars: underscore form matching Rust's
    /// own `target_arch` convention (e.g. `x86_64`, `aarch64`). Distinct from
    /// [`Display`] (which uses the dashed form `x86-64` for filenames).
    pub const fn cfg_suffix(&self) -> &'static str {
        match self {
            Arch::X86_64 => "x86_64",
            Arch::Aarch64 => "aarch64",
        }
    }

    #[cfg(target_arch = "x86_64")]
    pub fn get() -> Self {
        Arch::X86_64
    }

    #[cfg(target_arch = "aarch64")]
    pub fn get() -> Self {
        Arch::Aarch64
    }
}

impl Display for Arch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Arch::X86_64 => write!(f, "x86-64"),
            Arch::Aarch64 => write!(f, "aarch64"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Bitness {
    #[serde(rename = "64-bit")]
    X64,
}

impl Display for Bitness {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Bitness::X64 => write!(f, "64-bit"),
        }
    }
}

impl From<Arch> for Bitness {
    fn from(value: Arch) -> Self {
        use Bitness::*;
        match value {
            Arch::X86_64 => X64,
            Arch::Aarch64 => X64,
        }
    }
}
