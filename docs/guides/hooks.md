# `on_change` hooks

When a resource changes, you often want something else to happen - reload a service, regenerate a derived file, signal a process. `on_change` is how a plan declares those follow-up actions.

## The pattern

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

Read it as: "ensure the config file matches my source; when the config actually changes, reload nginx."

If you re-apply and the file already matches, the hook doesn't fire. Hooks run on real diffs, not on every apply.

## What fires a hook

Any non-empty change to the resource - new bytes, different mode, different owner, anything that makes the resource emit at least one operation. There's no per-field granularity in v1; it's "the resource changed" vs. "the resource didn't change".

## What can go inside `on_change`

Inline `@operation/*` items only:

| Type | Use |
| --- | --- |
| `@operation/systemd` | `start` / `stop` / `restart` / `reload` a unit. |
| `@operation/command` | Run a shell command. |

`on_change` items can't carry an `id`, `requires`, or `required_by`. They inherit ordering from the resource they hang off.

## Ordering

Hooks run in a strictly-later epoch than the resource's own operations. If the resource writes a file and chmods it, both happen before any hook runs.

If another plan item `requires: ["resource-id"]`, it waits for both the resource *and* its hooks. Dependents see the hook's effect, not just the resource's state.

## Identical hooks coalesce

If ten resources in the same epoch each declare `on_change: reload nginx`, all ten hooks land in the next epoch and merge into **one** reload. This is the killer feature: you don't have to manually deduplicate handlers across a fleet of config edits.

```yaml
- module: "@resource/file"
  params: { path: "/etc/nginx/conf.d/site-a.conf", source: "./a.conf", state: "sourced" }
  on_change:
    - module: "@operation/systemd"
      params: { name: "nginx", action: "reload" }

- module: "@resource/file"
  params: { path: "/etc/nginx/conf.d/site-b.conf", source: "./b.conf", state: "sourced" }
  on_change:
    - module: "@operation/systemd"
      params: { name: "nginx", action: "reload" }
```

Two file edits, one reload.

## Common shapes

### Reload after edit

```yaml
- module: "@resource/file"
  params: { path: "/etc/cron.d/backup", source: "./backup.cron", state: "sourced" }
  on_change:
    - module: "@operation/command"
      params: { command: "sudo systemctl reload cron" }
```

### Run a command after install

```yaml
- module: "@resource/apt"
  params:
    package: "postfix"
  on_change:
    - module: "@operation/command"
      params: { command: "sudo newaliases" }
```

## Gotchas (v1)

- **Cross-epoch coalescing isn't handled.** If resource A reloads nginx, B also reloads nginx, and B `requires: ["A"]` (different epoch), nginx reloads **twice**. Workaround: pull the reload into a single downstream `@resource/command`, or accept the duplicate (most reloads are idempotent).
- **Hook failure leaves you stuck.** If a hook fails, apply aborts. The resource is now in its target state, so a re-apply won't re-trigger the hook. Recovery:
  - Run the operation manually (`sudo systemctl reload nginx`), then re-apply, or
  - Briefly toggle a field on the resource (e.g. change `mode`) and re-apply, then revert.

## See also

- [Concepts: dependencies and hooks](../concepts/dependencies.md).
- [Concepts: operations](../concepts/operations.md).
- The [`nginx-cluster` example](../../examples/nginx-cluster/) is a working multi-machine plan with config-driven service control.
