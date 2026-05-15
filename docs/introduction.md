# Introduction

**lusid** lets you describe a computer's configuration in code and converge the real machine to match.

You write a plan: a list of resources you want on the machine (packages, files, users, services). lusid reads the current state of each, computes the smallest set of changes needed, runs them in the right order, and shows you what's happening as it goes.

## What it's good for

- **Personal workstations and servers** - laptops, desktops, homelab boxes, SBCs, cloud VMs.
- **Iterating on machine configs** - every plan can be tried inside a QEMU VM before touching real hardware.

## How it compares

- **vs. Ansible / Salt** - more friendly and functional for personal setups. Plans are written in [Rimu](https://rimu.dev), a small expression-oriented language, instead of templated YAML. Dependencies between resources are declared by id, not by ordering within a play.
- **vs. NixOS** - less ideological. You don't have to commit to an immutable distro, a custom package set, or the Nix expression language. lusid runs on top of `apt` / `pacman` and whatever services your distro ships.
- **vs. dotfiles** - same ergonomics for personal config (`state: "linked"` is a symlink-style mode), plus dependency ordering, idempotent re-apply, and the same machinery for system configs.

## Design themes

- **Idempotent**. Re-applying a plan after a successful apply is a no-op.
- **Diagnosable**. Errors point at the offending line in your plan source.
- **Streaming**. Apply progress is shown live; long-running operations stream their output.
- **Composable**. Plans can include other plans, pass params, and depend on each other.

## What it isn't (yet)

- **Not a package builder.** lusid orchestrates existing package managers; it doesn't build Nix-style immutable closures. (See [issue #1](https://github.com/ahdinosaur/lusid/issues/1).)
- **Not a fleet manager.** Each `apply` targets one machine. Multi-machine flows are sequential, not coordinated.
- **Not stable.** APIs, plan syntax, and CLI shape will change. Pin to a commit if you're depending on it.

## Next

- Build it: [Installation](./installation.md)
- Try it: [Quickstart](./quickstart.md)
