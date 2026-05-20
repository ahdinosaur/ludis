# Resources reference

Every built-in `@resource/<id>` module and its parameters.

Param types use the [plan-syntax vocabulary](./plan-syntax.md#param-schema): `string`, `number`, `boolean`, `list`, `host-path`, `target-path`.

---

## `@resource/apt`

Install Debian/Ubuntu packages.

```yaml
- module: "@resource/apt"
  params:
    package: "nginx"
# or
- module: "@resource/apt"
  params:
    packages: ["nginx", "curl", "git"]
```

| Field | Type | Required | What |
| --- | --- | --- | --- |
| `package` | string | one-of | A single package name. |
| `packages` | list of string | one-of | Multiple packages - expands to one atom per package. |

Same-epoch atoms merge into a single `apt-get install ...` invocation.

---

## `@resource/apt-repo`

Manage a third-party APT repository.

```yaml
- module: "@resource/apt-repo"
  params:
    name: "nodesource"
    uris: ["https://deb.nodesource.com/node_20.x"]
    suites: ["nodistro"]
    components: ["main"]
    key_url: "https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key"
```

| Field | Type | Required | What |
| --- | --- | --- | --- |
| `name` | string | yes | Filesystem-safe basename for `/etc/apt/sources.list.d/<name>.sources` and `/etc/apt/keyrings/<name>.asc`. |
| `uris` | list of string | yes | Repository URIs. |
| `suites` | list of string | yes | Distribution suites (e.g. `["bookworm"]`). |
| `components` | list of string | yes | Components (e.g. `["main", "contrib"]`). |
| `key_url` | string | yes | URL of the signing key. |
| `types` | list of string | optional | Defaults to `["deb"]`. |
| `architectures` | list of string | optional | e.g. `["amd64"]`. |
| `enabled` | boolean | optional | Defaults to `true`. |

---

## `@resource/aur`

Install Arch User Repository packages (requires an AUR helper on the target).

```yaml
- module: "@resource/aur"
  params:
    packages: ["yay", "paru"]
```

| Field | Type | Required | What |
| --- | --- | --- | --- |
| `package` | string | one-of | A single AUR package name. |
| `packages` | list of string | one-of | Multiple packages. |

---

## `@resource/command`

Run a shell command idempotently - gated by an `is_installed` probe.

```yaml
- module: "@resource/command"
  params:
    status: "install"
    install: "curl -sSL https://example.com/install.sh | sh"
    is_installed: "which my-tool"
```

| Field | Type | Required | What |
| --- | --- | --- | --- |
| `status` | `"install"` or `"uninstall"` | yes | Discriminator. |
| `install` | string | when `status = "install"` | Shell command to run when not installed. |
| `is_installed` | string | optional | Shell command; non-zero exit means "not installed, run `install`". |
| `uninstall` | string | when `status = "uninstall"` | Shell command to run when installed. |

Without `is_installed`, the `install` command runs on every apply - use it only for genuinely idempotent operations.

---

## `@resource/directory`

Create / source / link / remove a directory.

```yaml
# Just ensure it exists
- module: "@resource/directory"
  params:
    state: "present"
    path: "/etc/myapp"
    mode: 0o755
    user: "root"
    group: "root"

# Recursive copy from a source
- module: "@resource/directory"
  params:
    state: "sourced"
    source: "./configs/myapp"
    path: "/etc/myapp"

# Symlink to a source
- module: "@resource/directory"
  params:
    state: "linked"
    source: "./dotfiles/helix"
    path: system.user.home + "/.config/helix"

# Remove it
- module: "@resource/directory"
  params:
    state: "absent"
    path: "/etc/old-app"
```

| State | Extra fields |
| --- | --- |
| `"present"` | `path` (target-path, required), `mode`/`user`/`group` (optional), `sudo` (optional bool, default `false`). |
| `"sourced"` | `source` (host-path, required), `path` (target-path, required), `mode`/`user`/`group` (optional), `sudo` (optional bool, default `false`). |
| `"linked"` | `source` (host-path, required), `path` (target-path, required), `sudo` (optional bool, default `false`). No `mode`/`user`/`group`. |
| `"absent"` | `path` (target-path, required), `sudo` (optional bool, default `false`). |

`sudo: true` runs the create/copy/symlink/remove and any follow-up chmod/chown
under `sudo -n` so a `local apply` (which runs as the calling user) can land
target paths under `/etc/`, `/var/`, etc. Requires passwordless sudo. The state
probe stays as the calling user, so the parent directory must still be
readable to you - this works for the common case (root-owned `0755` parents)
and not for restricted ones (e.g. `/root/`).

See the [files-and-directories guide](../guides/files-and-directories.md) for `sourced` vs `linked`.

---

## `@resource/file`

Create / source / link / remove a file. Same state vocabulary as `@resource/directory`.

```yaml
- module: "@resource/file"
  params:
    state: "sourced"
    source: "./config/nginx.conf"
    path: "/etc/nginx/nginx.conf"
    mode: 0o644
    user: "root"
    group: "root"
    sudo: true
```

| State | Extra fields |
| --- | --- |
| `"present"` | `path` (target-path, required), `mode`/`user`/`group` (optional), `sudo` (optional bool, default `false`). |
| `"sourced"` | `source` (host-path, required, must be a regular file), `path`, `mode`/`user`/`group` (optional), `sudo` (optional bool, default `false`). |
| `"linked"` | `source` (host-path, required, must be a regular file), `path`, `sudo` (optional bool, default `false`). No `mode`/`user`/`group`. |
| `"absent"` | `path` (target-path, required), `sudo` (optional bool, default `false`). |

`sudo: true` semantics match `@resource/directory`: writes via stage-and-`sudo
install` (mode pinned to `0644` by the install; any explicit `mode:` then
applies through a follow-up `chmod`), symlinks via stage-temp-then-atomic-`mv`,
removes via `sudo rm -f`. Probes stay user-mode; `path` must still be readable
to you.

---

## `@resource/flatpak` (experimental)

Install a Flatpak app.

```yaml
- module: "@resource/flatpak"
  params:
    state: "present"
    name: "org.signal.Signal"
    remote: "flathub"   # optional; defaults to "flathub"
    user: false         # optional; --system if false, --user if true
```

| State | Extra fields |
| --- | --- |
| `"present"` | `name` (string, required), `remote` (string, optional, default `"flathub"`), `user` (bool, optional). |
| `"absent"` | `name` (string, required), `user` (bool, optional), `delete_data` (bool, optional). |

Apps only. Cross-scope dupes (declaring user-scope when system-scope exists, or vice versa) don't trigger a re-install.

---

## `@resource/flatpak-remote` (experimental)

Manage a Flatpak remote.

```yaml
- module: "@resource/flatpak-remote"
  params:
    state: "present"
    name: "flathub"
    url: "https://flathub.org/repo/flathub.flatpakrepo"
```

| State | Extra fields |
| --- | --- |
| `"present"` | `name` (string, required), `url` (string, required), `user` (bool, optional). |
| `"absent"` | `name` (string, required), `user` (bool, optional). |

---

## `@resource/git`

Clone or update a git working tree.

```yaml
- module: "@resource/git"
  params:
    repo: "https://github.com/user/dotfiles"
    path: "/home/me/repos/dotfiles"
    version: "main"
    update: true
```

| Field | Type | Required | What |
| --- | --- | --- | --- |
| `repo` | string | yes | Git URL. |
| `path` | target-path | yes | Where to clone. |
| `version` | string | optional | Branch / tag / SHA to check out. |
| `update` | boolean | optional | Pull on re-apply when the working tree exists. |
| `force` | boolean | optional | Allow destructive updates (overwrites local changes). |

---

## `@resource/group`

Manage a Unix group.

```yaml
- module: "@resource/group"
  params:
    state: "present"
    name: "deploy"
    gid: 1500
    system: false
    append_users: ["alice", "bob"]
```

| State | Fields |
| --- | --- |
| `"present"` | `name` (required), `gid` (optional), `system` (bool, optional), `append_users` (list of string, optional). |
| `"absent"` | `name` (required). |

---

## `@resource/pacman`

Install Arch Linux packages.

```yaml
- module: "@resource/pacman"
  params:
    packages: ["xorg-server", "xfce4"]
```

| Field | Type | Required | What |
| --- | --- | --- | --- |
| `package` | string | one-of | A single package name. |
| `packages` | list of string | one-of | Multiple packages. |

Same-epoch atoms merge into a single `pacman -S ...` invocation.

---

## `@resource/podman`

Manage a podman container.

```yaml
- module: "@resource/podman"
  params:
    state: "present"
    name: "redis"
    image: "docker.io/redis:7"
    ports: ["6379:6379"]
    volumes: ["/data/redis:/data"]
    restart_policy: "always"
    running: true
```

| State | Fields |
| --- | --- |
| `"present"` | `name` (required), `image` (required), `command` (list, optional), `env` (list, optional), `ports` (list, optional), `volumes` (list, optional), `restart_policy` (string, optional), `running` (bool, optional). |
| `"absent"` | `name` (required). |

---

## `@resource/secret`

Materialise an age-encrypted secret as a file on the target.

```yaml
- module: "@resource/secret"
  params:
    name: "api_token"           # → secrets/api_token.age
    path: "/etc/myapp/token"
    mode: 0o600                 # optional; default 0o600
    user: "myapp"               # optional
    group: "myapp"              # optional
```

| Field | Type | Required | What |
| --- | --- | --- | --- |
| `name` | string | yes | Secret name; resolves to `secrets/<name>.age`. |
| `path` | target-path | yes | Where the plaintext lands. |
| `mode` | number | optional | Default `0o600`. |
| `user` | string | optional | Target file owner. |
| `group` | string | optional | Target file group. |

See the [secrets guide](../guides/secrets.md) for the full flow.

---

## `@resource/systemd`

Enable / start / stop a systemd unit.

```yaml
- module: "@resource/systemd"
  params:
    name: "nginx"
    enabled: true
    active: true
    user: false         # optional; --user if true
```

| Field | Type | Required | What |
| --- | --- | --- | --- |
| `name` | string | yes | Unit name (`.service` may be omitted). |
| `enabled` | boolean | optional | Default `true`. `true` enables on boot; `false` disables. |
| `active` | boolean | optional | Default `true`. `true` starts now; `false` stops. |
| `user` | boolean | optional | Default `false`. `true` operates on `systemctl --user`. |

---

## `@resource/user`

Manage a Unix user.

```yaml
- module: "@resource/user"
  params:
    state: "present"
    name: "deploy"
    group: "deploy"
    append_groups: ["wheel", "docker"]
    home: "/home/deploy"
    shell: "/bin/bash"
    comment: "Deploy user"
```

| State | Fields |
| --- | --- |
| `"present"` | `name` (required), `uid` (optional), `group` (string, optional), `append_groups` (list of string, optional), `comment` (string, optional), `home` (target-path, optional), `shell` (string, optional), `system` (bool, optional), `create_home` (bool, optional). |
| `"absent"` | `name` (required), `remove_home` (bool, optional). |

`append_groups` is append-only: missing groups are added, existing memberships are left alone, and groups not listed here are **not** removed.

The plan does not set passwords. Combine with `@resource/command` shelling out to `chpasswd` for that - see the [arch-desktop example](../../examples/arch-desktop/).
