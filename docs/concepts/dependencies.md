# Dependencies

lusid runs operations in dependency order, not in source order. Two resources that don't depend on each other can apply concurrently; one that needs the other waits.

## `requires` / `required_by`

Every plan item can carry an `id`, plus `requires` and `required_by` lists referencing other ids:

```yaml
setup: (params, system) =>
  - module: "@resource/apt"
    id: "install-nginx"
    params:
      package: "nginx"

  - module: "@resource/file"
    id: "write-config"
    params:
      path: "/etc/nginx/nginx.conf"
      source: "./nginx.conf"
      state: "sourced"
    requires:
      - "install-nginx"

  - module: "@resource/systemd"
    params:
      name: "nginx"
      enabled: true
      active: true
    requires:
      - "write-config"
```

This says: install nginx first, then write the config, then enable + start the service.

`requires: [X]` and `required_by: [X]` are two ways to express the same edge. Use whichever reads more naturally where you are in the plan.

## Epochs

When lusid schedules a plan, it groups resources into **epochs** - layers of the dependency graph. Within an epoch every resource is independent; across epochs there's a "must happen before" relationship.

```text
Epoch 1: apt install nginx
Epoch 2: write /etc/nginx/nginx.conf
Epoch 3: systemctl enable nginx; systemctl start nginx
```

For each epoch in turn, lusid probes the current state of every resource in that epoch, computes the diff against the desired state, and emits operations to close the diff. Then it runs those operations before moving on. **State probing happens per-epoch**, so a resource's diff reflects what's true on the machine after every prior epoch has been applied - not what was true at the start of the apply.

Within one epoch, operations are coalesced where possible. Ten `apt install` ops in one epoch collapse into a single `apt install <pkg1> <pkg2> …` call (same for `pacman`, `aur`, `flatpak`). Side-effecting families (file writes, git pulls) don't merge - order matters. Coalescing only happens *within* an epoch; the same operation appearing in two different epochs runs twice (because the second occurrence might legitimately need to re-do the work after the first epoch's changes).

A plan with no `requires` edges runs as one big epoch (modulo intra-resource ordering).

## Depending on a nested plan

When a `requires:` references the id of a nested plan (a `module: "./other.lusid"` branch), it depends on every resource inside that plan - the whole subtree must complete before the dependent runs.

```yaml
- module: "./web-stack.lusid"
  id: "web"

- module: "@resource/systemd"
  params: { name: "monitor", enabled: true, active: true }
  requires: ["web"]    # waits for every resource in web-stack.lusid
```

## `on_change` hooks

A resource can declare operations to run when it changes. The classic case: edit a config file, reload the service.

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

Hooks fire when the resource has *any* non-empty change - new contents, different mode, different owner. They run in a strictly-later epoch than the resource's own operations.

### Identical hooks coalesce

If ten resources in the same epoch each `on_change: reload nginx`, the hooks land in a single later epoch and merge into **one** reload - not ten.

### Hooks register under the resource's id

A `requires: [resource-id]` waits for both the resource's own operations *and* its hooks. Dependents see the hook's effect, not just the resource's state.

```yaml
- module: "@resource/file"
  id: "nginx-config"
  params: { path: "/etc/nginx/nginx.conf", source: "./nginx.conf", state: "sourced" }
  on_change:
    - module: "@operation/systemd"
      params: { name: "nginx", action: "reload" }

- module: "@resource/command"
  params:
    status: "install"
    install: "curl http://localhost/ > /tmp/check"
    is_installed: "test -f /tmp/check"
  requires: ["nginx-config"]   # waits for the reload too
```

### Limitations (v1)

- **Hooks are inline only** - you can't reference a named handler defined elsewhere.
- **Inline operations can't declare `id`, `requires`, or `required_by`** - they inherit ordering from the resource that owns them.
- **Triggered on any change** - no add/modify/remove distinction.
- **Cross-epoch coalescing isn't handled.** If resource A reloads nginx, B also reloads nginx, and B `requires: ["A"]` (so they're in different epochs), nginx reloads twice. Workaround: pull the reload into a single downstream `@resource/command`, or accept the duplicate (most reloads are idempotent).
- **Hook failure leaves you stuck.** If a hook fails, apply aborts. The resource is now in its target state, so a plain re-apply won't re-trigger the hook. Recovery: run the operation manually (`sudo systemctl reload nginx`), or briefly toggle a field on the resource (e.g. change `mode`) and re-apply, then revert.

## Causality IDs must be unique

Every `id` must be unique across the whole plan tree (including nested plans). Duplicates are a hard error at planning time.

If you find yourself wanting the same id in two places, you probably want a single shared `@resource/*` upstream both depend on.
