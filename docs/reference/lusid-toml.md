# `lusid.toml` reference

The project config file. Lists every managed machine, where its plan lives, and any params to forward.

## Minimal example

```toml
[machines.my-laptop]
hostname = "laptop"
arch = "x86-64"
os = { type = "linux", linux = "debian", debian = 13 }
plan = "./plans/laptop.lusid"
```

## Full example

```toml
log = "info"   # default log level; overridden by --log / LUSID_LOG

[machines.web-a]
hostname = "web-a"
arch = "x86-64"
os = { type = "linux", linux = "debian", debian = 13 }
plan = "./web-server.lusid"
params = { greeting = "Hello from web-a!" }
remote = { host = "web-a.example.com", user = "deploy", port = 22 }

[machines.workstation]
hostname = "tower"
arch = "x86-64"
os = { type = "linux", linux = "arch" }
plan = "./workstation.lusid"
vm = { cpu_count = 4, memory_size = 4294967296, graphics = true }
```

## Top-level keys

| Key | Type | Default | What |
| --- | --- | --- | --- |
| `log` | string | `"error"` | Log level for `lusid` and `lusid-apply`. |
| `[machines.<id>]` | table | required | One entry per managed machine. The `<id>` is the name passed to `--machine`. |

## `[machines.<id>]` keys

| Key | Type | Required | What |
| --- | --- | --- | --- |
| `hostname` | string | yes | The target's `hostname`. Used by `local apply` to match the machine. |
| `arch` | string | yes | `"x86-64"` or `"aarch64"`. |
| `os` | table | yes | See [`os`](#os) below. |
| `plan` | string (path) | yes | Path to the `.lusid` plan file, relative to `lusid.toml`. |
| `params` | table | optional | Params forwarded to the plan's `setup` function. Schema validated against the plan's `params:`. |
| `remote` | table | optional | SSH connection details for `remote apply` / `remote ssh`. See [`remote`](#remote). |
| `vm` | table | optional | QEMU options for `dev apply` / `dev ssh`. See [`vm`](#vm). |

### `os`

```toml
os = { type = "linux", linux = "debian", debian = 13 }
os = { type = "linux", linux = "arch" }
os = { type = "linux", linux = "ubuntu", ubuntu = "24.04" }
```

| Key | Values | Notes |
| --- | --- | --- |
| `type` | `"linux"` | macOS / Windows not currently supported as targets. |
| `linux` | `"debian"`, `"ubuntu"`, `"arch"` | The distro. |
| `debian` | integer | Major version (e.g. 12, 13). Required when `linux = "debian"`. |
| `ubuntu` | string `"YY.MM"` | e.g. `"22.04"`, `"24.04"`. Required when `linux = "ubuntu"`. |

Arch is rolling, so no version field.

### `remote`

```toml
remote = { host = "web-a.example.com" }
remote = { host = "10.0.0.5", user = "deploy", port = 2222, ssh_key = "~/.ssh/work_ed25519" }
```

| Key | Type | Default | What |
| --- | --- | --- | --- |
| `host` | string | required | Hostname or IP of the SSH target. |
| `port` | integer | `22` | SSH port. |
| `user` | string | `"root"` | SSH user. When not root, lusid wraps `lusid-apply` in `sudo -n` - user must have passwordless sudo. |
| `ssh_key` | string (path) | `~/.ssh/id_ed25519` | Private key to authenticate with. |

### `vm`

```toml
vm = { cpu_count = 4, memory_size = 4294967296, graphics = true }
```

| Key | Type | Default | What |
| --- | --- | --- | --- |
| `cpu_count` | integer | backend default | Number of vCPUs. |
| `memory_size` | integer (bytes) | backend default | RAM in bytes (e.g. `4294967296` = 4 GiB). |
| `disk_size` | integer (bytes) | backend default | Virtual size of the guest root disk overlay. Raise this when the plan installs a lot of software - cloud images ship with a small partition. |
| `graphics` | bool | `false` | Open a QEMU display window. Set true for desktop-environment plans. |

## ⚠️ Don't put secrets in `params`

`params` values are forwarded to `lusid-apply --params <json>`, which puts them in the process's `argv[]`. Visible via `ps` and `/proc/<pid>/cmdline` to any UID on the target.

Use [`@resource/secret`](../guides/secrets.md) for sensitive values.

## See also

- [Apply modes guide](../guides/apply-modes.md) - when `remote`, `vm`, and `params` matter.
- [`lusid` crate README](../../lusid/README.md) - the CLI's view of the config.
