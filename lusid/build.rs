//! Embed `lusid-apply` binaries for each supported worker arch into the
//! `lusid` binary at build time.
//!
//! Looks for `lusid-apply-<arch_display>` files in this directory order:
//!
//! 1. `$LUSID_APPLY_BINARIES_DIR` if set (used by CI). **Strict mode**:
//!    missing arch fails the build, so a release never silently ships a
//!    binary with an arch unembedded.
//! 2. `<repo-root>/embed/` (the layout `just build-lusid-apply` stages).
//!    **Lenient mode**: missing arch emits `cargo:warning=…` and proceeds, so
//!    a fresh checkout compiles before the worker is built. Runtime then
//!    errors with `EmbeddedError::NotEmbedded` for that arch.
//!
//! For each binary found, copies it into `OUT_DIR` and exports:
//!
//! - `cargo:rustc-env=LUSID_APPLY_OUT_<ARCH>=<absolute-path>` — pointed at by
//!   `include_bytes!` in `src/embedded.rs`.
//! - `cargo:rustc-cfg=embedded_apply_<arch>` — gates the `include_bytes!`
//!   call so a lenient-mode build with no binaries staged still compiles.

use std::env;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process;

/// `(Arch::Display value used for the source filename, Arch::cfg_suffix()
/// used for cfg names so `target_arch`-style cfgs stay consistent)`.
///
/// Source of truth for the variant set lives in `lusid_system::Arch` (see
/// `Arch::all()` / `Arch::cfg_suffix()`); duplicated here because `build.rs`
/// can't depend on `lusid-system` without a slow build-dep. Keep these two
/// strings per variant aligned with the corresponding `Arch` value.
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
    // Strict mode is gated on the env var being set, not on the dir being
    // present. CI explicitly opts in by exporting `LUSID_APPLY_BINARIES_DIR`;
    // local dev fall-back to `<repo-root>/embed/` stays lenient even if a
    // contributor happens to have created that dir.
    let strict = env::var_os("LUSID_APPLY_BINARIES_DIR").is_some();
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

    // Watch the staging dir itself so a previously-empty `embed/` getting
    // populated by `just build-lusid-apply` triggers a rerun. Without this,
    // the per-file `rerun-if-changed` below is only emitted when a binary is
    // *already* present, so additions to an empty dir would otherwise need a
    // `cargo clean` or `touch build.rs` to be picked up. Cargo accepts a
    // nonexistent path here and reruns when it appears.
    println!("cargo:rerun-if-changed={}", binaries_dir.display());

    for (arch_display, cfg_suffix) in ARCHES {
        let source = binaries_dir.join(format!("lusid-apply-{arch_display}"));
        if !source.is_file() {
            if strict {
                eprintln!(
                    "error: lusid-apply for {arch_display} not embedded: {} is missing \
                     (LUSID_APPLY_BINARIES_DIR is set, so this is a strict-mode build)",
                    source.display()
                );
                process::exit(1);
            }
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
