# Installation

No binary releases yet - build from source.

## Prerequisites

- **Rust** (stable toolchain).
- **[`just`](https://github.com/casey/just)** - runs the build recipes.
- **aarch64 cross-compile toolchain.** The `lusid-apply` worker builds for both x86-64 and aarch64 (so a host can drive a guest VM of either arch) - you need the cross-tools even if you only care about your host arch.

### Cross-compile toolchain

- **Debian / Ubuntu**

  ```sh
  sudo apt install gcc-aarch64-linux-gnu libc6-dev-arm64-cross
  ```

- **Arch**

  ```sh
  sudo pacman -S aarch64-linux-gnu-gcc aarch64-linux-gnu-glibc
  ```

- **All distros**

  ```sh
  rustup target add x86_64-unknown-linux-gnu aarch64-unknown-linux-gnu
  ```

The aarch64 linker is wired up in `.cargo/config.toml`.

### For `dev apply` (local QEMU VMs)

If you want to apply plans inside a local VM, install QEMU and the image-building tools:

- **Debian**

  ```sh
  sudo apt install qemu-system-x86 qemu-utils libguestfs-tools genisoimage
  ```

- **Arch**

  ```sh
  sudo pacman -S qemu-full libguestfs cdrtools
  ```

You don't need these for `lusid local apply` on a real machine you already own.

## Build

```sh
git clone https://github.com/ahdinosaur/lusid
cd lusid
just build-lusid-apply           # builds the apply worker, stages under ./embed/
cargo build -p lusid --release   # builds the CLI; picks up ./embed/ by default
```

You get one binary at `./target/release/lusid`.

The `lusid-apply` worker is **embedded** in the CLI binary at build time. On first `local apply` it's extracted to `~/.cache/lusid/lusid-apply/<version>/<arch>/`. For `dev apply` / `remote apply` it's streamed to the target over SFTP. No path to configure at runtime.

The example recipes (e.g. `just nginx-cluster-apply-a`) chain both steps for you.

## Verify

```sh
./target/release/lusid --help
```

## Next

- [Quickstart](./quickstart.md) - write and apply your first plan.
