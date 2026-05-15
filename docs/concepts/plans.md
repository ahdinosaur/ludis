# Plans

A **plan** is a `.lusid` file. It's the unit of configuration lusid loads and applies.

## Shape

A plan has three top-level fields:

```yaml
name: "my-plan"
version: "0.1.0"

params:           # optional — declares what params the plan accepts
  greeting:
    type: "string"

setup: (params, system) =>
  - module: "@resource/file"
    params:
      state: "sourced"
      source: "./hello.txt"
      path: "/etc/hello.txt"
```

- **`name`** and **`version`** — identifying metadata, like a Cargo or npm manifest.
- **`params`** — schema for the parameters the plan accepts. Optional; omit if the plan takes none.
- **`setup`** — a function that takes `(params, system)` and returns a list of plan items.

## The `setup` function

`setup` is the only logic in a plan. It returns a list of items, each being either:

- A **resource** — `module: "@resource/<id>"` (apt, file, systemd, …).
- A **nested plan** — `module: "./other.lusid"` (a sibling file path; recursively planned).

Rimu's expression language lets `setup` build the list dynamically — conditional items, lists derived from params, computed strings. See the [plan syntax reference](../reference/plan-syntax.md).

## Params

When declared, params are typed and validated before `setup` runs. A bad param value points at the offending line in `lusid.toml` (or in `--params` JSON).

```yaml
params:
  greeting:
    type: "string"
  install_extra:
    type: "boolean"
    optional: true
  packages:
    type: "list"
    item:
      type: "string"
```

See the [plan syntax reference](../reference/plan-syntax.md#param-schema) for the full type vocabulary.

## The `system` object

`system` is the second argument to `setup`. It's populated by lusid from runtime detection:

```text
system.hostname        # "my-laptop"
system.arch            # "x86-64" or "aarch64"
system.os.type         # "linux"
system.os.linux        # "debian" / "ubuntu" / "arch"
system.os.debian       # 13 (number) — only when linux = "debian"
system.os.ubuntu       # "22.04" (string) — only when linux = "ubuntu"
system.user.name       # current user running apply
system.user.home       # current user's $HOME
```

The version field is named after the distro and only exists on that distro (Arch is rolling, so no version field).

Use `system` to make a plan portable — write a dotfile to `system.user.home + "/.zshrc"` instead of hard-coding a path. Full schema: [the system object](../reference/system.md).

## Nested plans

A `module:` that isn't a `@resource/*` or `@operation/*` is resolved as a file path relative to the current plan. The named file is planned recursively, its `setup` invoked with whatever `params:` you pass.

```yaml
setup: (params, system) =>
  - module: "./web-stack.lusid"
    params:
      domain: "example.com"

  - module: "./database.lusid"
```

This is the composition mechanism — split a large machine config into per-role plans, share plans across machines.

## Plan items: optional fields

Every item in the `setup` list can carry these fields:

```yaml
- module: "@resource/apt"
  id: "install-nginx"           # label this item; others can `requires: ["install-nginx"]`
  params:
    package: "nginx"
  requires:                     # depend on other items by id
    - "configure-firewall"
  required_by:                  # invert: declare what depends on this
    - "start-nginx"
  on_change:                    # operations to run when this resource changes
    - module: "@operation/systemd"
      params: { name: "nginx", action: "reload" }
```

`requires` / `required_by` shape the [dependency graph](./dependencies.md). `on_change` declares [hooks](./dependencies.md#on_change-hooks).

## Idempotence

Re-applying is always safe. See [Resources](./resources.md#resources-are-idempotent) for how.
