# Files and directories

`@resource/file` and `@resource/directory` both materialise something from a source on your machine onto the target. They support two states for *how* that materialisation happens — `sourced` and `linked` — plus `present` and `absent` for the simple cases.

## `sourced` — copy the bytes

```yaml
- module: "@resource/file"
  params:
    state: "sourced"
    source: "./config/nginx.conf"
    path: "/etc/nginx/nginx.conf"
    mode: 0o644
    user: "root"
    group: "root"
```

- **What happens.** lusid reads `source` from your machine and writes the bytes to `path` on the target, atomically.
- **Drift.** Edits to `source` only propagate on the next apply.
- **Mode / owner.** Optional `mode` / `user` / `group` fields.
- **Use when.** The bytes need to live on the target independently of your filesystem — system configs, deployable artifacts, anything you'd run via `dev apply` or `remote apply` where the operator's machine isn't reachable from the target.

For directories, `sourced` recursively copies the tree.

## `linked` — symlink to the source

```yaml
- module: "@resource/file"
  params:
    state: "linked"
    source: "./dotfiles/zshrc"
    path: system.user.home + "/.zshrc"
```

- **What happens.** lusid creates a symlink at `path` pointing to `source`.
- **Drift.** Edits to `source` show up immediately — no re-apply needed.
- **Mode / owner.** Refused. Linux symlinks don't have meaningful permissions of their own, and chmod / chown via the link would silently mutate the source file in your repo.
- **Use when.** You're editing config files in place and want changes to take effect without re-applying. The classic dotfiles ergonomic.

The same applies to `@resource/directory` with `state: "linked"`.

## Source validation

Both `sourced` and `linked` validate at plan-load time (before any resource probes the target) that:

- `source` exists on the operator's machine.
- It has the expected type — regular file for `@resource/file`, directory for `@resource/directory`.

This catches typos and stale paths up front, with a diagnostic pointing at the offending line in your plan source.

## `present` and `absent`

For files/directories you don't need to source:

```yaml
- module: "@resource/file"
  params:
    state: "present"
    path: "/etc/myapp/config"
    mode: 0o600
    user: "myapp"
    group: "myapp"
```

`present` ensures the path exists with the given attributes. `absent` removes it.

## Sourced vs. linked: when to use what

| Situation | State |
| --- | --- |
| Dotfile you edit daily | `linked` |
| System config you commit to a repo | `sourced` |
| Anything applied via `dev apply` or `remote apply` | `sourced` |
| Config for a binary running with stricter perms than your editor | `sourced` |

If you're not sure, `sourced` is the safer default — it works in every apply mode and the file on the target is a regular file with whatever mode you set.

`linked` is the right call when you want the dotfiles ergonomic: change a file in your repo, the change is live immediately.

## See also

- [The `dotfiles` example](../../examples/dotfiles/) — a minimal `linked` setup for `~/.zshrc` and `~/.config/helix/`.
- [Resource reference](../reference/resources.md) — full param schemas for `@resource/file` and `@resource/directory`.
