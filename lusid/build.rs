//! Embed `lusid-apply` binaries for each supported worker arch into the
//! `lusid` binary at build time.
//!
//! Looks for `lusid-apply-<arch_display>` files in this directory order:
//!
//! 1. `$LUSID_APPLY_BINARIES_DIR` if set (used by CI).
//! 2. `<repo-root>/embed/` (the layout `just build-lusid-apply` stages).
//!
//! For each binary found, copies it into `OUT_DIR` and exports:
//!
//! - `cargo:rustc-env=LUSID_APPLY_OUT_<ARCH>=<absolute-path>` — pointed at by
//!   `include_bytes!` in `src/embedded.rs`.
//! - `cargo:rustc-cfg=embedded_apply_<arch>` — gates the `include_bytes!`
//!   call so a dev build with no binaries staged compiles without needing a
//!   placeholder file.
//!
//! When neither location has a binary for an arch, `cargo:warning=…` points
//! the user at `just build-lusid-apply`. The resulting `lusid` errors with
//! `EmbeddedError::NotEmbedded` at runtime if asked to apply for that arch.

use std::env;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process;

/// (`Arch::Display` value used for the source filename, cfg-name suffix
/// using underscores so `target_arch`-style cfgs stay consistent.)
const ARCHES: &[(&str, &str)] = &[("x86-64", "x86_64"), ("aarch64", "aarch64")];

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-env-changed=LUSID_APPLY_BINARIES_DIR");

    // Declare the cfgs we conditionally emit so `cargo` doesn't warn about
    // unknown cfg names in `src/embedded.rs`.
    for (_, cfg_suffix) in ARCHES {
        println!("cargo:rustc-check-cfg=cfg(embedded_apply_{cfg_suffix})");
    }

    let out_dir = PathBuf::from(env::var_os("OUT_DIR").expect("OUT_DIR not set"));
    let binaries_dir = env::var_os("LUSID_APPLY_BINARIES_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            // Default to `<repo-root>/embed/`, which is where the justfile
            // (and contributors following the README) stage binaries.
            // `CARGO_MANIFEST_DIR` here is the `lusid` crate dir; its parent
            // is the workspace / repo root.
            PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .expect("CARGO_MANIFEST_DIR has no parent")
                .join("embed")
        });

    for (arch_display, cfg_suffix) in ARCHES {
        let source = binaries_dir.join(format!("lusid-apply-{arch_display}"));
        if !source.is_file() {
            println!(
                "cargo:warning=lusid-apply for {arch_display} not embedded: \
                 {} is missing — run `just build-lusid-apply`",
                source.display()
            );
            continue;
        }
        let dest = out_dir.join(format!("lusid-apply-{cfg_suffix}.bin"));
        if let Err(err) = atomic_copy(&source, &dest) {
            eprintln!(
                "error copying {} to {}: {err}",
                source.display(),
                dest.display()
            );
            process::exit(1);
        }
        println!("cargo:rerun-if-changed={}", source.display());
        println!(
            "cargo:rustc-env=LUSID_APPLY_OUT_{}={}",
            cfg_suffix.to_uppercase(),
            dest.display()
        );
        println!("cargo:rustc-cfg=embedded_apply_{cfg_suffix}");
    }
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
