//! Embed `lusid-apply` binaries for each supported worker arch into the
//! `lusid` binary at build time.
//!
//! When `LUSID_APPLY_BINARIES_DIR` is set, this script copies the binaries it
//! finds inside into `OUT_DIR` and exports two things for each arch where a
//! binary was resolved:
//!
//! - `cargo:rustc-env=LUSID_APPLY_OUT_<ARCH>=<absolute-path>` — pointed at by
//!   `include_bytes!` in `src/embedded.rs`.
//! - `cargo:rustc-cfg=embedded_apply_<arch>` — gates the `include_bytes!`
//!   call so dev builds with no env vars set compile without needing
//!   placeholder files.
//!
//! Per-arch overrides (`LUSID_APPLY_LINUX_X86_64` / `LUSID_APPLY_LINUX_AARCH64`)
//! take precedence over the directory layout, mirroring the runtime CLI flags
//! of the same names — so a developer iterating on `lusid-apply` can point at
//! `./target/<triple>/release/lusid-apply` without staging a directory.
//!
//! When neither source resolves for an arch, this script emits a
//! `cargo:warning` and skips that arch. The runtime resolver reports
//! `NotEmbedded` if the user then asks for that arch.

use std::env;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process;

/// (`Arch::Display` value, env-var stem matching the existing CLI / `lusid.toml`
/// fields, cfg-name suffix using underscores so `target_arch`-style cfgs stay
/// consistent.)
const ARCHES: &[(&str, &str, &str)] = &[
    ("x86-64", "LUSID_APPLY_LINUX_X86_64", "x86_64"),
    ("aarch64", "LUSID_APPLY_LINUX_AARCH64", "aarch64"),
];

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-env-changed=LUSID_APPLY_BINARIES_DIR");

    // Declare the cfgs we conditionally emit so `cargo` doesn't warn about
    // unknown cfg names in `src/embedded.rs`.
    for (_, _, cfg_suffix) in ARCHES {
        println!("cargo:rustc-check-cfg=cfg(embedded_apply_{cfg_suffix})");
    }

    let out_dir = PathBuf::from(env::var_os("OUT_DIR").expect("OUT_DIR not set"));
    let binaries_dir = env::var_os("LUSID_APPLY_BINARIES_DIR").map(PathBuf::from);

    // Only warn about a missing per-arch binary when the user clearly *meant*
    // to embed (set the dir or the specific env var). Default dev `cargo
    // build` should be silent.
    let any_intent = binaries_dir.is_some()
        || ARCHES
            .iter()
            .any(|(_, env_var, _)| env::var_os(env_var).is_some());

    for (arch_display, env_var, cfg_suffix) in ARCHES {
        println!("cargo:rerun-if-env-changed={env_var}");

        let source = resolve_source(env_var, binaries_dir.as_deref(), arch_display);
        match source {
            Some(path) => {
                let dest = out_dir.join(format!("lusid-apply-{cfg_suffix}.bin"));
                if let Err(err) = atomic_copy(&path, &dest) {
                    eprintln!(
                        "error copying {} to {}: {err}",
                        path.display(),
                        dest.display()
                    );
                    process::exit(1);
                }
                println!("cargo:rerun-if-changed={}", path.display());
                println!(
                    "cargo:rustc-env=LUSID_APPLY_OUT_{}={}",
                    cfg_suffix.to_uppercase(),
                    dest.display()
                );
                println!("cargo:rustc-cfg=embedded_apply_{cfg_suffix}");
            }
            None if any_intent => {
                println!(
                    "cargo:warning=lusid-apply for {arch_display} not embedded \
                     (set LUSID_APPLY_BINARIES_DIR/{arch_display} or {env_var}); \
                     runtime resolution for that arch will require an override path"
                );
            }
            None => {}
        }
    }
}

/// Resolve a binary source for a given arch, preferring the per-arch override
/// (the same env var the runtime CLI reads) over the directory layout.
fn resolve_source(
    override_env_var: &str,
    binaries_dir: Option<&Path>,
    arch_display: &str,
) -> Option<PathBuf> {
    if let Some(val) = env::var_os(override_env_var) {
        let path = PathBuf::from(val);
        if path.is_file() {
            return Some(path);
        }
        eprintln!(
            "warning: {override_env_var} set to {} but file does not exist; \
             falling back to LUSID_APPLY_BINARIES_DIR",
            path.display()
        );
    }
    let dir = binaries_dir?;
    let candidate = dir.join(format!("lusid-apply-{arch_display}"));
    candidate.is_file().then_some(candidate)
}

/// Copy `src` to `dst` via a `.tmp` sibling + atomic rename, to keep
/// incremental rebuilds from seeing partial writes.
fn atomic_copy(src: &Path, dst: &Path) -> io::Result<()> {
    let parent = dst.parent().expect("dst has no parent");
    fs::create_dir_all(parent)?;
    let tmp = parent.join(format!(
        ".{}.tmp.{}",
        dst.file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_default(),
        process::id()
    ));
    fs::copy(src, &tmp)?;
    fs::rename(&tmp, dst)
}
