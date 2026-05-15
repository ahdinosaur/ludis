# Directory the `lusid` build script reads to embed `lusid-apply` blobs at
# compile time. See `lusid/build.rs` and `lusid/src/embedded.rs`.
export LUSID_APPLY_BINARIES_DIR := justfile_directory() / "embed"

# Show available recipes.
default:
  @just --list

# Build `lusid-apply` for each supported worker arch and stage it under
# `./embed/` ready for the next `cargo build -p lusid` to embed. Clears
# any previously-staged binaries first so toggling which arches are
# enabled below doesn't leave stale files in the embed dir.
#
# Cross-compiling aarch64 from an x86-64 host requires:
#   Debian/Ubuntu:  gcc-aarch64-linux-gnu  libc6-dev-arm64-cross
#   Arch:           aarch64-linux-gnu-gcc  aarch64-linux-gnu-glibc
# plus `rustup target add aarch64-unknown-linux-gnu`. The linker is
# wired up in `.cargo/config.toml`. (The reverse - aarch64 → x86-64 -
# needs the mirror set of packages and works the same way.) CI builds
# each arch on a native runner instead; see `.github/workflows/release.yml`.
build-lusid-apply:
  rm -f {{ LUSID_APPLY_BINARIES_DIR }}/lusid-apply-*
  mkdir -p {{ LUSID_APPLY_BINARIES_DIR }}
  cargo build -p lusid-apply --target x86_64-unknown-linux-gnu --release
  cp ./target/x86_64-unknown-linux-gnu/release/lusid-apply {{ LUSID_APPLY_BINARIES_DIR }}/lusid-apply-x86-64
  cargo build -p lusid-apply --target aarch64-unknown-linux-gnu --release
  cp ./target/aarch64-unknown-linux-gnu/release/lusid-apply {{ LUSID_APPLY_BINARIES_DIR }}/lusid-apply-aarch64

# -----------------------------------------------------------------------------
# Example: examples/nginx-cluster
#
# Two Debian 13 x86-64 servers, each running nginx with a per-machine greeting.

# List the machines defined in the nginx-cluster example.
nginx-cluster-list:
  cargo run -p lusid --release -- --config ./examples/nginx-cluster/lusid.toml machines list

# Boot the web-a VM (if not already running) and apply the plan to it.
nginx-cluster-apply-a: build-lusid-apply
  cargo run -p lusid --release -- --config ./examples/nginx-cluster/lusid.toml dev apply --machine web-a

# Boot the web-b VM (if not already running) and apply the plan to it.
nginx-cluster-apply-b: build-lusid-apply
  cargo run -p lusid --release -- --config ./examples/nginx-cluster/lusid.toml dev apply --machine web-b

# Open an SSH session to the web-a dev VM (e.g. to `curl localhost`).
nginx-cluster-ssh-a:
  cargo run -p lusid --release -- --config ./examples/nginx-cluster/lusid.toml dev ssh --machine web-a

# Open an SSH session to the web-b dev VM.
nginx-cluster-ssh-b:
  cargo run -p lusid --release -- --config ./examples/nginx-cluster/lusid.toml dev ssh --machine web-b

# -----------------------------------------------------------------------------
# Example: examples/arch-desktop
#
# One Arch Linux x86-64 machine with a minimal XFCE desktop + LightDM.

# List the machines defined in the arch-desktop example.
arch-desktop-list:
  cargo run -p lusid --release -- --config ./examples/arch-desktop/lusid.toml machines list

# Boot the desktop VM, apply the plan, and watch LightDM appear in the QEMU window.
arch-desktop-apply: build-lusid-apply
  cargo run -p lusid --release -- --config ./examples/arch-desktop/lusid.toml dev apply --machine desktop

# Open an SSH session to the desktop dev VM.
arch-desktop-ssh:
  cargo run -p lusid --release -- --config ./examples/arch-desktop/lusid.toml dev ssh --machine desktop
