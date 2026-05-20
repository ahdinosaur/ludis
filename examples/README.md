# lusid examples

Runnable, end-to-end examples of configuring real machines with lusid.

Each example is a self-contained directory with its own `lusid.toml`, one or more `.lusid` plan files, and a README walking through what the plan does and how to try it.

## Prerequisites

- **Rust toolchain** (stable) to build lusid.
- **[just](https://github.com/casey/just)** to run the top-level `justfile` recipes. Recipes wrap `cargo run`; you can run those commands directly if you prefer.
- **QEMU + mkisofs** for the `dev apply` / `dev ssh` flow (local VMs):
  - Debian: `sudo apt install qemu-system-x86 qemu-utils genisoimage`
  - Arch: `sudo pacman -S qemu-full cdrtools`

Only Rust is required for `lusid local apply` on a real machine.

## Examples

| Example | What it is | OS |
| --- | --- | --- |
| [`nginx-cluster/`](./nginx-cluster/) | Two Debian servers, each running nginx with a per-machine greeting page. Shows multi-machine configs, per-machine `params`, and dependency ordering. | Debian 13 |
| [`arch-desktop/`](./arch-desktop/) | One Arch Linux machine running a minimal XFCE desktop with LightDM. Shows installing a group of packages and enabling a display-manager service. | Arch Linux |
| [`dotfiles/`](./dotfiles/) | Symlinks a config file and a config directory into `$HOME` via `state: "linked"`. Demonstrates `sourced` (copy) vs `linked` (symlink) for `@resource/file` and `@resource/directory`. | any Linux |

Each example follows the same shape:

```
<example-name>/
├── README.md        # what it demonstrates + how to run it
├── lusid.toml       # machines + plans + params
└── <plan>.lusid     # Rimu plan file(s)
```

See the top-level [README](../README.md#concepts) for the concept reference (Plan, Resource, Operation, Epoch).
