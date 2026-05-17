# lusid

_STATUS: MAD SCIENCE 🧪_

![frankenstein](https://github.com/user-attachments/assets/53b049ef-a256-4b41-9e01-240660fb0153)

> Use declarative code to configure your living computer.

## About

Lusid configures your computers with the exact setup you describe.

- Like .dotfiles on steroids: less ideological than NixOS, friendlier than Ansible or Salt for personal setups
- Works on workstations (desktops, laptops) and servers (homelab, cloud)

## Get Started

Check out the [examples](./examples/):

- [`examples/nginx-cluster`](./examples/nginx-cluster/) - two Debian servers each running nginx with a per-machine greeting.
- [`examples/arch-desktop`](./examples/arch-desktop/) - one Arch machine running a minimal XFCE desktop.

### Install

Download `lusid` from [latest releases](https://github.com/ahdinosaur/lusid/releases).

The `lusid-apply` worker, which will execute the plan (on your local, virtual, or remote machine) is **embedded** into the `lusid` program at build-time.

For `dev apply` / `dev ssh` you also need QEMU and image-building tools - see the [examples prerequisites](./examples/README.md#prerequisites).

### Create a plan

A lusid project is just a directory with two files:

- `lusid.toml` - lists the machines you want to manage and pairs each with a plan file.
- `*.lusid` - one or more plan files written in [Rimu](https://rimu.dev), each exporting a `setup(params, system)` function that returns a list of resources.

The smallest useful project is a single machine applying a single plan:

```toml
# lusid.toml
[machines.my-server]
hostname = "my-server"
arch = "x86-64"
os = { type = "linux", linux = "debian", debian = 13 }
plan = "./server.lusid"
```

```yaml
# server.lusid
name: "server"
version: "0.1.0"

setup: (params, system) =>
  - module: "@resource/apt"
    params:
      packages: ["curl", "git", "htop"]
```

See the [examples](./examples/) for configs that use `params`, dependency ordering, and the `system` object (hostname, OS, current user).

> ⓘ Use [`@resource/secret`](./secrets/README.md) for sensitive material.

### Apply a plan

There are three ways to run a plan, depending on where the target machine is:

**Local** - apply to the host you're sitting at. lusid picks the machine config whose `hostname` matches `$(hostname)`.

```sh
lusid --config ./lusid.toml local apply
```

**Dev VM** - boot a local QEMU VM matching the machine's spec (OS, arch) and apply inside it. Great for iterating on a plan without touching your real machine:

```sh
lusid --config ./lusid.toml dev apply --machine my-server
lusid --config ./lusid.toml dev ssh   --machine my-server   # shell inside the VM
```

**Remote** - apply to a real machine you reach over SSH. Add a `remote = { host = "..." }` block to the machine entry, then:

```sh
lusid --config ./lusid.toml remote apply --machine my-server
lusid --config ./lusid.toml remote ssh   --machine my-server   # shell on the target
```

`remote` accepts `host` (required), `port` (default `22`), `user` (default `"root"`), and `ssh_key` (default `~/.ssh/id_ed25519`). When `user` is not `root`, lusid wraps the remote `lusid-apply` invocation in `sudo -n …`, so the SSH user must have passwordless sudo configured.

> ⓘ Host-key verification is trust-on-first-use against `~/.ssh/known_hosts`: the first connection pins whatever key the server presents (matching OpenSSH's `StrictHostKeyChecking=accept-new`), and subsequent connections refuse mismatches. **The first apply assumes the network path between you and the target is clean** - if there's any doubt (apply across the public internet, captive Wi-Fi, fresh cloud VM), verify the target's `/etc/ssh/ssh_host_ed25519_key.pub` out-of-band and seed `~/.ssh/known_hosts` yourself before the first run.

Each form has a sibling `parse` subcommand that validates the plan and shows the resolved resource tree without probing target state or running any operation:

```sh
lusid --config ./lusid.toml local parse
lusid --config ./lusid.toml dev    parse --machine my-server
lusid --config ./lusid.toml remote parse --machine my-server
```

**Per-epoch confirm.** Apply pauses between resource epochs and shows a footer prompt summarising what's about to run; press `↵` to apply, `n`/`Esc` to abort. Pass `-y` / `--yes` to skip every prompt and run straight through. The confirm prompt is always interactive: lusid refuses to start an apply that would block on a prompt it cannot show, so any non-interactive invocation (CI, pipes, redirects, `--no-tui`) must pass `-y`.

**Plain-log mode.** Pass `--no-tui` to skip the ratatui TUI and emit a line-buffered digest to stderr instead. lusid switches to plain-log automatically whenever stdout is not a terminal. This only affects rendering; the confirm requirement above is independent.

Applying the same plan twice is always safe: lusid reads the current state of every resource and only runs the operations needed to close the gap. A no-op apply after a successful apply prints "no changes" and exits.

## Concepts

### Plan

A plan describes a modular set of resources you want to be applied to the machine.

Plans are written in [the Rimu language](https://rimu.dev):

```yaml
name: "example-git-setup"
version: "0.1.0"

params:
  whatever:
    type: "boolean"

setup: (params, system) =>
  - module: "@resource/file"
    params:
      state: "sourced"
      source: "./gitconfig"
      path: system.user.home + "/.gitconfig"

  - module: "@resource/apt"
    id: "install-curl"
    params:
      package: "curl"

  - module: "@resource/command"
    params:
      status: "install"
      install: "curl -LO 'https://github.com/BurntSushi/ripgrep/releases/download/15.1.0/ripgrep_15.1.0-1_amd64.deb' && sudo dpkg -i ripgrep_15.1.0-1_amd64.deb && rm ripgrep_15.1.0-1_amd64.deb"
      is_installed: "which rg"
    requires:
      - "install-curl"
```

A plan has metadata (`name`, `version`), declared `params`, and a `setup` function returning a list of items.

- Each item is either another plan (called recursively) or a core resource (`@resource/*`).
- Items can declare `requires` / `required_by` to order them.
- Items can declare `on_change` to specify an operation that should run when the item is changed.

On apply, lusid expands plans into atomic resources, diffs each against the live system, computes a dependency-ordered set of operations, merges duplicates, and runs them in epochs.

### Resource

A resource is the intended state of a thing on your computer - a package, a file, a service. Each resource type defines its user-facing params, how to observe current state, how to compute a change, and how to lower that change into operations.

### Operation

An operation is an action that actually runs on your computer - installing a package, writing a file, reloading a service. Each operation type defines how to merge same-typed operations within an epoch, and how to apply one.

### Built-in types

| Type | Resource | Operation |
| --- | --- | --- |
| `apt` | [resource](./resource/src/resources/apt.rs) | [operation](./operation/src/operations/apt.rs) |
| `apt-repo` | [resource](./resource/src/resources/apt_repo.rs) | [operation](./operation/src/operations/apt_repo.rs) |
| `aur` | [resource](./resource/src/resources/aur.rs) | [operation](./operation/src/operations/aur.rs) |
| `command` | [resource](./resource/src/resources/command.rs) | [operation](./operation/src/operations/command.rs) |
| `directory` | [resource](./resource/src/resources/directory.rs) | [operation](./operation/src/operations/directory.rs) |
| `file` | [resource](./resource/src/resources/file.rs) | [operation](./operation/src/operations/file.rs) |
| `flatpak` | [resource](./resource/src/resources/flatpak.rs.rs) | [operation](./operation/src/operations/flatpak.rs) |
| `flatpak-remote` | [resource](./resource/src/resources/flatpak_remote.rs.rs) | [operation](./operation/src/operations/flatpak_remote.rs) |
| `git` | [resource](./resource/src/resources/git.rs) | [operation](./operation/src/operations/git.rs) |
| `group` | [resource](./resource/src/resources/group.rs) | [operation](./operation/src/operations/group.rs) |
| `pacman` | [resource](./resource/src/resources/pacman.rs) | [operation](./operation/src/operations/pacman.rs) |
| `podman` | [resource](./resource/src/resources/podman.rs) | [operation](./operation/src/operations/podman.rs) |
| `secret` | [resource](./resource/src/resources/secret.rs) | [operation](./operation/src/operations/secret.rs) |
| `systemd` | [resource](./resource/src/resources/systemd.rs) | [operation](./operation/src/operations/systemd.rs) |
| `user` | [resource](./resource/src/resources/user.rs) | [operation](./operation/src/operations/user.rs) |

## Glossary

- **Rimu**: embedded language used for `.lusid` plans.
- **Plan**: a `.lusid` file declaring metadata, params, and a `setup` function.
- **Resource**: the desired state of one thing on a machine.
- **Operation**: a concrete action that runs on a machine.
- **Change**: the delta from observed state to desired state.
- **Epoch**: a layer of operations with no remaining dependencies, run together.

## Roadmap

- [ ] Implement my complete personal "SnugOS" config
- [ ] Add Nix-like immutable package builder: https://github.com/ahdinosaur/lusid/issues/1
- [ ] Add unit testing framework for plans: https://github.com/ahdinosaur/lusid/issues/11

## Development

**Prerequisites.** Rust stable, [`just`](https://github.com/casey/just), and the aarch64 cross-compile toolchain (the worker builds for both x86-64 and aarch64 even on a single-arch host):

- Debian/Ubuntu: `sudo apt install gcc-aarch64-linux-gnu libc6-dev-arm64-cross`
- Arch: `sudo pacman -S aarch64-linux-gnu-gcc aarch64-linux-gnu-glibc`
- All: `rustup target add x86_64-unknown-linux-gnu aarch64-unknown-linux-gnu`

The aarch64 linker is wired up in `.cargo/config.toml`. CI builds each arch on a native runner, so this isn't needed in `release.yml`.

```sh
git clone https://github.com/ahdinosaur/lusid
cd lusid
just build-lusid-apply           # builds lusid-apply, stages under ./embed/
cargo build -p lusid --release   # picks up ./embed/ by default
```

The example recipes (e.g. `just nginx-cluster-apply-a`) chain both steps.

You get one binary at `./target/release/lusid`. The worker is extracted to `~/.cache/lusid/lusid-apply/<version>/<arch>/` on first `local apply`, and streamed over SFTP for `dev apply` / `remote apply`.

## Related projects

- [comtrya](https://github.com/comtrya/comtrya)
- (legacy) [boxen](https://github.com/boxen/boxen)
