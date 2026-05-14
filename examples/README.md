# lusid examples

Runnable, end-to-end examples of configuring real machines with lusid.

Each example is a self-contained directory with its own `lusid.toml`, one or more `.lusid` plan files, and a README that walks through what the plan does and how to try it.

## Prerequisites

Before running any example, you'll need:

- **Rust toolchain** (stable) to build lusid itself.
- **[just](https://github.com/casey/just)** to run the recipes in the top-level `justfile`. The recipes are just wrappers around `cargo run` with the right arguments — you can run them by hand instead if you prefer.
- **QEMU + libguestfs + mkisofs** if you want to use the `dev apply` / `dev ssh` flow (local VMs).
  On Debian:
  ```sh
  sudo apt install qemu-system-x86 qemu-utils libguestfs-tools genisoimage
  ```
  On Arch:
  ```sh
  sudo pacman -S qemu-full libguestfs cdrtools
  ```

None of these are needed if you're only applying plans to machines you already have (via `lusid local apply`).

## Examples

| Example | What it is | OS |
| --- | --- | --- |
| [`nginx-cluster/`](./nginx-cluster/) | Two Debian servers, each running nginx with a per-machine greeting page. Shows multi-machine configs, per-machine `params`, and dependency ordering between resources. | Debian 13 |
| [`arch-desktop/`](./arch-desktop/) | One Arch Linux machine running a minimal XFCE desktop with LightDM. Shows installing a group of packages and enabling a display-manager service. | Arch Linux |
| [`dotfiles/`](./dotfiles/) | A dotfiles-style plan that symlinks a config file and a config directory into `$HOME` via `state: "linked"`. Demonstrates the `sourced` (copy) vs `linked` (symlink) distinction for `@resource/file` and `@resource/directory`. | any Linux |

Every example follows the same structure:

```
<example-name>/
├── README.md        # what the example demonstrates + how to run it
├── lusid.toml       # which machines exist, what plan + params each uses
└── <plan>.lusid     # the Rimu plan file(s)
```

`lusid.toml` is the top-level config the `lusid` CLI reads. It lists the machines you want to manage and pairs each with a plan. A plan (written in [Rimu](https://rimu.dev)) exports a `setup(params, system) => [...]` function that returns the list of resources to apply.

See the top-level [README](../README.md#concepts) for the full concept reference (Plan, Resource, Operation, Epoch).
