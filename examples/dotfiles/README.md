# lusid dotfiles example

A minimal dotfiles-style setup demonstrating `state: "linked"` for
`@resource/file` and `@resource/directory`.

## What it does

Run `lusid local apply` and the plan creates two symlinks under `$HOME`:

- `~/.zshrc` → `examples/dotfiles/files/zshrc`
- `~/.config/helix/` → `examples/dotfiles/files/helix/`

Edits to the source files in this repo show up immediately at the symlink
targets — no re-apply needed.

## `sourced` vs `linked`

`@resource/file` and `@resource/directory` both offer two ways to materialise a host-path source on the target:

| State | What it does | Use when |
| --- | --- | --- |
| `state: "sourced"` | Copies bytes (file) or the tree (directory) into `path`. Accepts `mode`/`user`/`group`. | The bytes need to live on the target independently — system configs, deployable artifacts, `dev apply` / `remote apply`. |
| `state: "linked"` | Symlinks `path` to `source`. No `mode`/`user`/`group`. | Editing config files in place — changes take effect without re-applying. The dotfiles ergonomic this example uses. |

## Running

```sh
lusid --config examples/dotfiles/lusid.toml local apply
```

The plan reads `system.user.home` (the `$HOME` of whoever runs apply), so the symlinks always land under the invoking user's home — no config edits needed.

Back up any existing `~/.zshrc` or `~/.config/helix/` first — `linked` atomically replaces files and stale symlinks at `path`.

Why use lusid for dotfiles? You get dependency ordering, idempotent re-apply, and the same machinery as your system configs.
