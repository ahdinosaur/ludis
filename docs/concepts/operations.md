# Operations

An **operation** is an imperative action that runs on the target - install this package, reload this service, run this command.

Operations live in the `@operation/<id>` namespace.

```yaml
- module: "@operation/systemd"
  params:
    name: "nginx"
    action: "reload"
```

Operations are *not* idempotent in the resource sense. They have no notion of "current state" - they just run when triggered.

## Where operations live

Operations can only appear inside an [`on_change` block](./dependencies.md#on_change-hooks) on a `@resource/*` plan item. They are the actions a resource fires when it changes.

```yaml
- module: "@resource/file"
  params:
    path: "/etc/nginx/nginx.conf"
    source: "./nginx.conf"
    state: "sourced"
  on_change:
    - module: "@operation/systemd"      # only valid here, never at top level
      params: { name: "nginx", action: "reload" }
```

Operations *cannot* be top-level items in `setup`. Putting one there is a hard error. The principle: top-level plan items should be idempotent declarations; imperative actions only run when prompted by a state change.

If you want an idempotent imperative action at the top level - e.g. a shell command that's a no-op once a marker file exists - use `@resource/command` instead.

## Built-in operation types

Two operation modules are currently exposed:

| Type | Purpose |
| --- | --- |
| `@operation/command` | Run a shell command. |
| `@operation/systemd` | `start` / `stop` / `restart` / `reload` a systemd unit. |

`@operation/command` is intentionally broad - it covers logrotate signals, cron reloads, cache invalidation, anything you'd reach for `sh -c` to do.

More operation modules exist internally (the apt, file, git, etc. families) but they're produced *by resources*, not authored directly by plans.

## Why have operations at all?

A common pattern is "edit a config file, then reload the service that uses it". The edit is declarative (config should look like X); the reload is imperative (run `systemctl reload` once, after the edit). Resources express the former; operations express the latter. [`on_change` hooks](./dependencies.md#on_change-hooks) connect them.

## See also

- [Resources](./resources.md) - the declarative counterpart.
- [Dependencies and hooks](./dependencies.md).
- The [nginx-cluster example](../../examples/nginx-cluster/) shows operations triggered from `on_change`.
