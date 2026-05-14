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

`@resource/file` and `@resource/directory` both offer two ways to materialise a
host-path source on the target:

| State | What it does | Use when |
| --- | --- | --- |
| `state: "sourced"` | Copies the bytes (file) or recursively copies the tree (directory) into `path`. Accepts `mode`/`user`/`group`. | The bytes need to live independently on the target — e.g. system configs, deployable artifacts, or anything you'd run via `dev apply`/`remote apply` where the operator's filesystem isn't reachable from the target. |
| `state: "linked"` | Materialises `path` as a symlink to `source`. No `mode`/`user`/`group`. | You're editing config files in place and want changes to take effect without re-running apply — the dotfiles ergonomic this example uses. |

Both forms validate at plan-load time that `source` exists and has the
expected type (regular file for `@resource/file`, directory for `@resource/directory`).

## Running

```sh
lusid --config examples/dotfiles/lusid.toml local apply
```

The plan reads `system.user.home` (the `$HOME` of whoever runs apply), so
the symlinks always land under the invoking user's home — no config edits
needed. Back up any existing `~/.zshrc` or `~/.config/helix` first if you
don't want them clobbered: the `linked` state replaces a regular file or
stale symlink at `path` atomically.
