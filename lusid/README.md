# lusid

User-facing CLI. Reads `lusid.toml`, spawns the
[`lusid-apply`](../lusid-apply) worker baked into this binary at build
time, and renders its progress as a live [ratatui](https://docs.rs/ratatui)
TUI.

## Subcommands

- `lusid machines list` — print all configured machines as a table.
- `lusid local apply` — apply the plan for the machine whose `hostname`
  matches `$(hostname)` on the local host. Spawns `lusid-apply` as a
  subprocess.
- `lusid dev apply --machine <id>` — boot a QEMU VM matching the machine
  spec (via [`lusid-vm`](../vm)), SFTP the plan directory + a prebuilt
  `lusid-apply` binary into it, run apply over SSH, pipe the stream into
  the TUI. Reuses the VM if it already exists.
- `lusid dev ssh --machine <id>` — same VM bring-up, then drop into an
  interactive shell.
- `lusid remote apply` / `lusid remote ssh` — reserved; `todo!()` today.

## Architecture

```
lusid CLI ──spawn──> lusid-apply ──stdout: AppUpdate JSON──> TUI (ratatui)
                                 ──stderr: text lines ─────> stderr pane
```

The TUI doesn't know about lusid's domain types — it only knows
[`AppView`](../apply-stdio) / [`FlatViewTree`](../apply-stdio). Everything
renderable has already been turned into [`lusid-view`](../view) values by
`lusid-apply` before it hits the wire.

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

CLI flags + env vars (`LUSID_CONFIG`, `LUSID_LOG`) override the
corresponding TOML keys.

The `lusid-apply` worker is baked into the `lusid` binary at build time
(see [`lusid/build.rs`](./build.rs) and [`lusid/src/embedded.rs`](./src/embedded.rs)) —
no runtime path to configure.
