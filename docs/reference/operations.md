# Operations reference

Operations are imperative actions invoked from a `@resource/*` plan item's `on_change` block. They're not idempotent on their own - they just run whenever the parent resource has a non-empty state diff.

```yaml
- module: "@resource/file"
  params:
    path: "/etc/nginx/nginx.conf"
    source: "./nginx.conf"
    state: "sourced"
  on_change:
    - module: "@operation/systemd"
      params: { name: "nginx", action: "reload" }
```

Available operation modules are listed in `plan/src/operation.rs::AVAILABLE_OPERATION_MODULES`. Today:

- [`@operation/command`](#operationcommand) - shell out to an arbitrary command.
- [`@operation/systemd`](#operationsystemd) - reload / restart / start / stop a unit.
- [`@operation/podman`](#operationpodman) - container and compose actions.

## `@operation/command`

See the resource counterpart's docs for the shape; the operation form mirrors it.

```yaml
- module: "@operation/command"
  params:
    command: "systemctl reload nginx"
    executor: "shell"   # optional; "shell" (default) or "direct"
    sudo: true          # optional
```

## `@operation/systemd`

```yaml
- module: "@operation/systemd"
  params:
    name: "nginx"
    action: "reload"    # "reload" | "restart" | "start" | "stop"
    user: false         # optional; --user if true
```

## `@operation/podman`

Discriminator: `action`. Single source of truth for valid values is `operation/src/operations/podman.rs::PodmanOperation::parse_params`.

### Container actions

```yaml
- module: "@operation/podman"
  params:
    action: "start"
    name: "redis"
    sudo: false
```

| `action`   | Required fields | Optional fields |
| ---------- | --------------- | --------------- |
| `start`    | `name`          | `sudo`          |
| `stop`     | `name`          | `sudo`          |
| `remove`   | `name`          | `sudo`          |

`create` is intentionally not exposed: it writes a `lusid.config-hash` label that the resource layer owns. Use `@resource/podman state: "present"` for declarative container creation.

### Compose actions

```yaml
- module: "@operation/podman"
  params:
    action: "compose_up"
    project: "my_app"
    files: ["./compose.yaml"]
    working_dir: "./services"     # optional; default = first file's parent
    env_file: "./.env"            # optional
    sudo: false
```

| `action`         | Required fields              | Optional fields                            | What                                                            |
| ---------------- | ---------------------------- | ------------------------------------------ | --------------------------------------------------------------- |
| `compose_up`     | `project`, `files`           | `working_dir`, `env_file`, `sudo`          | `podman-compose -p <project> -f f1 ... up -d`.                  |
| `compose_down`   | `project`                    | `sudo`                                     | Remove containers + networks bearing the project label.         |
| `compose_pull`   | `project`, `files`           | `working_dir`, `env_file`, `sudo`          | Refresh images without recreating. Pair with `compose_up` to roll. |

The author-facing `compose_up` does **not** install the lusid marker network: hash-based drift detection is owned by `@resource/podman state: "compose_present"`. Operators reaching for the operation form are choosing imperative control; mixing the two interfaces for the same project will cause every apply to detect drift and recreate.

`project` is validated against `^[a-z0-9][a-z0-9_-]{0,62}$`.
