# lusid

User-facing CLI. Reads `lusid.toml`, spawns the [`lusid-apply`](../lusid-apply) worker (embedded in this binary at build time), and renders its progress as a live [ratatui](https://docs.rs/ratatui) TUI.

## Subcommands

- `machines list` — table of machines from `lusid.toml`.
- `local apply` — apply locally; matches the entry whose `hostname` is `$(hostname)`.
- `dev {apply,ssh} --machine <id>` — bring up a QEMU VM via [`lusid-vm`](../vm), then apply or shell into it.
- `remote {apply,ssh} --machine <id>` — over SSH to a machine with a `remote = { host = "..." }` block.

## Architecture

```
lusid CLI ──spawn──> lusid-apply ──stdout: AppUpdate JSON──> TUI (ratatui)
                                 ──stderr: text lines ─────> stderr pane
```

The TUI doesn't know lusid's domain types — only [`AppView`](../apply-stdio) / [`FlatViewTree`](../apply-stdio). `lusid-apply` renders everything to [`lusid-view`](../view) values before they hit the wire.

## `lusid.toml`

```toml
log = "info"

[machines.my-laptop]
hostname = "laptop"
arch = "x86-64"
os = { type = "linux", linux = "debian", debian = 13 }
plan = "./plans/laptop.lusid"
params = { extra_pkgs = ["ripgrep"] }
```

CLI flags and env vars (`LUSID_CONFIG`, `LUSID_LOG`) override the corresponding TOML keys.

The `lusid-apply` worker is baked into the binary at build time (see [`build.rs`](./build.rs) and [`src/embedded.rs`](./src/embedded.rs)) — no runtime path to configure.
