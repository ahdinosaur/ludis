# Resources

A **resource** is the intended state of one thing on your computer - a package, a file, a service, a user. Resources are declarative: you say what you want, lusid figures out how to make it true.

Resources live in the `@resource/<id>` namespace.

```yaml
- module: "@resource/apt"
  params:
    package: "nginx"
```

This says "nginx should be installed via apt". If nginx is already installed, this resource is a no-op. If it isn't, lusid runs `apt install nginx`.

## How a resource is processed

For every resource in a plan, lusid runs the same five-step pipeline:

1. **Params** - what you wrote in the plan.
2. **Resource atoms** - the params expand into one or more indivisible pieces. `apt { packages: [a, b] }` becomes two atoms, one per package.
3. **State** - lusid probes the machine to find each atom's current state (`Installed` / `NotInstalled`, file mode bits, service active/enabled).
4. **Change** - the diff from observed state to desired state. `None` means "already correct".
5. **Operations** - concrete actions that close the diff (`apt install`, write file, `systemctl enable`).

The five-step shape is uniform across every resource type. Different resources probe different things and emit different operations, but the flow is the same.

## Built-in resource types

| Type | Purpose |
| --- | --- |
| `@resource/apt` | Install Debian/Ubuntu packages. |
| `@resource/apt-repo` | Manage `/etc/apt/sources.list.d/` repository definitions and signing keys. |
| `@resource/aur` | Install AUR packages on Arch (via an AUR helper). |
| `@resource/command` | Idempotent shell commands with an `is_installed` probe. |
| `@resource/directory` | Create / source / link directories on the target. |
| `@resource/file` | Create / source / link / remove files on the target. |
| `@resource/flatpak` | Install Flatpak apps (experimental). |
| `@resource/flatpak-remote` | Manage Flatpak remotes (experimental). |
| `@resource/git` | Clone / update a git working tree. |
| `@resource/group` | Manage Unix groups. |
| `@resource/pacman` | Install Arch Linux packages. |
| `@resource/podman` | Manage podman containers. |
| `@resource/podman-compose` | Manage podman-compose projects. |
| `@resource/secret` | Materialise an [age-encrypted secret](../guides/secrets.md) as a file. |
| `@resource/systemd` | Enable / start / stop systemd units. |
| `@resource/user` | Manage Unix users. |

The full param schema for each lives in the [resource reference](../reference/resources.md).

## Resources are idempotent

Every resource type observes current state before doing anything. A re-apply after a successful apply is a no-op for that resource - it sees no diff, emits no operations.

This is the core difference from the imperative side: resources are *what should be true*; [operations](./operations.md) are *what to do right now*.

## Atoms and intra-resource ordering

A single resource declaration can expand into multiple atoms with their own internal ordering. For example, `@resource/file` with `state: "sourced"` and a `mode` field expands to:

- write the file's bytes, then
- set the mode.

These are linked by a small internal dependency tree so the mode is applied after the write, never before. As a plan author you don't see this - you just write one `@resource/file` item.

## See also

- [Operations](./operations.md) - the imperative counterpart.
- [Dependencies](./dependencies.md) - ordering across resources, not within.
- [Reference: built-in resource params](../reference/resources.md).
