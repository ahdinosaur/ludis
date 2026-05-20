# Installation

Two paths: download a pre-built release, or build from source.

## Download a release

Releases ship `lusid` for Linux on x86-64 and aarch64. The `lusid-apply` worker is embedded for both architectures, so one binary drives same-arch and cross-arch targets - there's no second binary to download or path to configure.

1. Pick the archive for your machine from the [latest release](https://github.com/ahdinosaur/lusid/releases):

    - `lusid-<version>-x86_64-unknown-linux-gnu.tar.gz` - x86-64 Linux.
    - `lusid-<version>-aarch64-unknown-linux-gnu.tar.gz` - aarch64 Linux.

    Each archive ships with a matching `.sha256` file.

2. Verify and extract (substitute `<version>` and `<target>`):

    ```sh
    sha256sum -c lusid-<version>-<target>.tar.gz.sha256
    tar -xzf lusid-<version>-<target>.tar.gz
    ```

3. Install onto your `PATH`:

    ```sh
    sudo install -m 0755 lusid /usr/local/bin/lusid
    ```

Skip ahead to [Verify](#verify), or read on to build from source.

## Build from source

Use this path on an arch we don't ship, or to track `main`.

### Prerequisites

- **Rust** (stable toolchain).
- **[`just`](https://github.com/casey/just)** - runs the build recipes.
- **aarch64 cross-compile toolchain.** The `lusid-apply` worker builds for both x86-64 and aarch64 (so a host can drive a guest VM of either arch) - you need the cross-tools even if you only care about your host arch.

#### Cross-compile toolchain

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

### Build

```sh
git clone https://github.com/ahdinosaur/lusid
cd lusid
just build-lusid-apply           # builds the apply worker, stages under ./embed/
cargo build -p lusid --release   # builds the CLI; picks up ./embed/ by default
```

You get one binary at `./target/release/lusid`.

The `lusid-apply` worker is **embedded** in the CLI binary at build time. On first `local apply` it's extracted to `~/.cache/lusid/lusid-apply/<version>/<arch>/`. For `dev apply` / `remote apply` it's streamed to the target over SFTP. No path to configure at runtime.

The example recipes (e.g. `just nginx-cluster-apply-a`) chain both steps for you.

## For `dev apply` (local QEMU VMs)

If you want to apply plans inside a local VM, install QEMU and the image-building tools:

- **Debian**

  ```sh
  sudo apt install qemu-system-x86 qemu-utils genisoimage
  ```

- **Arch**

  ```sh
  sudo pacman -S qemu-full cdrtools
  ```

You don't need these for `lusid local apply` on a real machine you already own.

## Verify

```sh
lusid --help
```

## Next

- [Quickstart](./quickstart.md) - write and apply your first plan.
