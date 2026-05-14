# lusid

_STATUS: MAD SCIENCE 🧪_

![frankenstein](https://github.com/user-attachments/assets/53b049ef-a256-4b41-9e01-240660fb0153)

> Use declarative code to configure your living computer.

## About

Lusid helps you configure your computers with the exact setup you describe.

Like .dotfiles on steroids, but less "pure" (ideological) than NixOS. Like Ansible or Salt Stack, but more friendly and functional for personal setups.

Lusid can be used for your workstations (desktops or laptops) or your servers (homelab or cloud).

## Get Started

Check out the [examples](./examples/):

- [`examples/nginx-cluster`](./examples/nginx-cluster/) — two Debian servers each running nginx with a per-machine greeting.
- [`examples/arch-desktop`](./examples/arch-desktop/) — one Arch machine running a minimal XFCE desktop.

### Install

Until lusid has binary releases, build it from source:

```sh
git clone https://github.com/ahdinosaur/lusid
cd lusid
cargo build --release
```

This produces two binaries under `./target/release/`:

- `lusid` — the CLI you run: `lusid machines list`, `lusid local apply`, `lusid dev apply`, ….
- `lusid-apply` — the worker that actually evaluates + applies a plan. `lusid` spawns this either locally or inside a dev VM over SSH.

The `just` recipes in the repo root handle the cross-compile dance. If you have [just](https://github.com/casey/just) installed:

```sh
just build-lusid-apply
```

For running the `dev apply` / `dev ssh` flow you also need QEMU and a couple of image-building tools — see the [examples prerequisites](./examples/README.md#prerequisites) for the exact packages.

### Create a plan

A lusid project is just a directory with two files:

- `lusid.toml` — lists the machines you want to manage and pairs each with a plan file.
- `*.lusid` — one or more plan files written in [Rimu](https://rimu.dev), each exporting a `setup(params, system)` function that returns a list of resources.

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

> ⚠️ Don't put secret values in `lusid.toml`'s `params`. The CLI forwards them to `lusid-apply --params <json>`, where they land in `argv[]` and are visible via `ps` / `/proc/<pid>/cmdline` to any UID on the target. Use [`@resource/secret`](./secrets/README.md) for sensitive material.

### Apply a plan

There are three ways to run a plan, depending on where the target machine is:

**Local** — apply to the host you're sitting at. lusid picks the machine config whose `hostname` matches `$(hostname)`.

```sh
lusid --config ./lusid.toml local apply
```

**Dev VM** — boot a local QEMU VM matching the machine's spec (OS, arch) and apply inside it. Great for iterating on a plan without touching your real machine:

```sh
lusid --config ./lusid.toml dev apply --machine my-server
lusid --config ./lusid.toml dev ssh   --machine my-server   # shell inside the VM
```

**Remote** — apply to a machine you reach over SSH. Not implemented yet; tracked on the roadmap.

Applying the same plan twice is always safe: lusid reads the current state of every resource and only runs the operations needed to close the gap. A no-op apply after a successful apply prints "no changes" and exits.

## Concepts

### Plan

A plan describes a modular set of resources you want to be applied to the machine.

Plans are written in the [the Rimu language](https://rimu.dev):

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

A plan:

- Defines basic metadata like name and version (e.g. think `package.json` or `Cargo.toml`)
- Defines parameters that it expects to receive
- Defines a `setup` function, which return a list of items to apply.
  - An item can refer to another plan defined by the user, in which case they are called.
  - Or, an item can a core states, these are defined in Rust and called like any other plan.
- Items can be dependent: there is a way to say this _requires_ or is _required_by_ another item.

When a plan is applied:

- Given the inputs, the outputs should construct a tree.
  - The branches are user modules, the leaves are core states.
- The core states are evaluated from user-facing params into a sub-tree of atomic resources.
- For each resource, find the current state of the resource on your computer, then compare with the desired state to determine a resource change.
- Convert each resource change into a sub-tree of operations.
- From the causality tree, find a minimal list of ordered epochs, where each epoch is a list of operations that can be applied together.
- Merge all operations of the same type in the same epoch.
- Iterate through each epoch in order, applying the operations.

### Resource

A resource represents the intended state of a thing on your computer, e.g. a package or a file or a service.

Resource types:

- [x] [Apt](./resource/src/resources/apt.rs)
- [x] [AptRepo](./resource/src/resources/apt_repo.rs)
- [x] [Command](./resource/src/resources/command.rs)
- [x] [Directory](./resource/src/resources/directory.rs)
- [x] [File](./resource/src/resources/file.rs)
- [x] [Git](./resource/src/resources/git.rs)
- [x] [Group](./resource/src/resources/group.rs)
- [x] [Pacman](./resource/src/resources/pacman.rs)
- [x] [Podman](./resource/src/resources/podman.rs)
- [x] [Systemd](./resource/src/resources/systemd.rs)
- [x] [User](./resource/src/resources/user.rs)
- [ ] FlatPak ([TODO](https://github.com/ahdinosaur/lusid/issues/32))

Each resource type defines:

- The user-facing parameters to describe such resources
- How to evaluate user-facing params into atomic resources: each atomic resource representing one thing on your computer.
- How to find the current state of the resource on your computer.
- Given the current state and the desired state, what change should be applied?
- How to apply the change as a set of operations.

### Operation

An operation is an action you can apply to your computer, e.g. installing a package, writing a file, or reloading a service.

Operation types:

- [x] [Apt](./operation/src/operations/apt.rs)
- [x] [AptRepo](./operation/src/operations/apt_repo.rs)
- [x] [Command](./operation/src/operations/command.rs)
- [x] [Directory](./operation/src/operations/directory.rs)
- [x] [File](./operation/src/operations/file.rs)
- [x] [Git](./operation/src/operations/git.rs)
- [x] [Group](./operation/src/operations/group.rs)
- [x] [Pacman](./operation/src/operations/pacman.rs)
- [x] [Podman](./operation/src/operations/podman.rs)
- [x] [Systemd](./operation/src/operations/systemd.rs)
- [x] [User](./operation/src/operations/user.rs)
- [ ] FlatPak ([TODO](https://github.com/ahdinosaur/lusid/issues/32))

Each operation type defines:

- How to merge multiple operations of the same type
- How to apply an operation

## Glossary

- **Rimu**: embedded language used for `.lusid` plans.
- **Spanned**: value annotated with source span for diagnostics.
- **Plan**: parsed/evaluated Rimu object containing `setup`.
- **PlanItem**: an entry returned by setup, either resource module or nested plan.
- **ResourceParams**: typed configuration definition (user-facing).
- **Resource**: atomized resource node(s) derived from params.
- **State**: observed current system state for a resource.
- **Change**: computed delta from state to desired.
- **Operation**: executable action(s) derived from change.
- **Epoch**: dependency layer computed from causality constraints.

## Roadmap

- [ ] Implement my complete personal "SnugOS" config
- [ ] Add system (i.e. Salt Stack "grains"): https://github.com/ahdinosaur/lusid/issues/9
- [ ] Add secrets management: https://github.com/ahdinosaur/lusid/issues/7
- [ ] Add Nix-like immutable package builder: https://github.com/ahdinosaur/lusid/issues/1
- [ ] Add unit testing framework for plans: https://github.com/ahdinosaur/lusid/issues/11
- [ ] Add install hooks: https://github.com/ahdinosaur/lusid/issues/31

## Related projects

- [comtrya](https://github.com/comtrya/comtrya)
- (legacy) [boxen](https://github.com/boxen/boxen)
