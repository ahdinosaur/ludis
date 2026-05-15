# CLI reference

```
lusid [--config <path>] <subcommand>
```

## Global flags

| Flag | Env | Default | What |
| --- | --- | --- | --- |
| `--config <path>` | `LUSID_CONFIG` | `./lusid.toml` | Path to the project config. |
| `--log <level>` | `LUSID_LOG` | `error` | Log level: `error`, `warn`, `info`, `debug`, `trace`. |
| `--secrets-dir <path>` | `LUSID_SECRETS_DIR` | `<root>/secrets` | Override the secrets directory. |
| `--identity <path>` | `LUSID_IDENTITY` | (none) | Age identity file. Required by `local apply`, `secrets cat`, `secrets edit`, `secrets rekey`; ignored by `secrets ls`, `secrets check`, `secrets keygen`. |

The TOML `log = "..."` in `lusid.toml` is overridden by `--log` / `LUSID_LOG`.

## `machines list`

Print every machine declared in `lusid.toml` as a table.

```sh
lusid --config ./lusid.toml machines list
```

## `local apply`

Apply a plan to the local host. lusid looks up the entry whose `hostname` matches `$(hostname)`.

```sh
lusid --config ./lusid.toml local apply
```

Errors out if no machine matches the host's `hostname`.

## `dev apply --machine <id>`

Boot a local QEMU VM matching the machine spec, copy the plan in, apply over SSH.

```sh
lusid --config ./lusid.toml dev apply --machine my-server
```

The VM persists between runs - re-applying with the same `--machine` reuses the same VM. Cloud images are cached under `~/.cache/lusid/vm/images/`.

## `dev ssh --machine <id>`

Same VM bring-up as `dev apply`, then drop into an interactive shell.

```sh
lusid --config ./lusid.toml dev ssh --machine my-server
```

## `remote apply --machine <id>`

Apply over SSH to a real machine. Requires a `remote = { host = "..." }` block on the machine.

```sh
lusid --config ./lusid.toml remote apply --machine my-server
```

When the SSH `user` isn't root, lusid wraps the remote `lusid-apply` invocation in `sudo -n`. The user must have passwordless sudo configured.

Host-key verification is trust-on-first-use against `~/.ssh/known_hosts`. See [apply modes](../guides/apply-modes.md#host-key-verification).

## `remote ssh --machine <id>`

Open an interactive shell on a remote machine.

```sh
lusid --config ./lusid.toml remote ssh --machine my-server
```

## `secrets <subcommand>`

Manage age-encrypted secrets in the project's `secrets/` directory. See the [secrets guide](../guides/secrets.md) for a walkthrough.

| Subcommand | Action |
| --- | --- |
| `lusid secrets ls` | List `*.age` files and their effective recipients. |
| `lusid secrets cat <name>` | Decrypt to stdout. |
| `lusid secrets edit <name>` | Decrypt → `$EDITOR` → re-encrypt on save. |
| `lusid secrets rekey [<name>]` | Re-encrypt to the current recipient list. Bulk-rekeys without `<name>`. |
| `lusid secrets keygen [-o <path>]` | Generate an x25519 operator identity. |
| `lusid secrets check` | Audit `secrets/` against `lusid-secrets.toml`. Non-zero exit on any finding. |

## Exit codes

| Code | When |
| --- | --- |
| `0` | Apply succeeded (or no-op). |
| non-zero | Any error: bad config, plan load error, apply failure, lookup miss, etc. |
