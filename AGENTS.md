# AGENTS.md

**lusid** is a Rust project for declarative machine configuration via "plans".

## What this project is

Lusid takes a `.lusid` plan (written in the **Rimu** language), optionally a parameters object, and:

1. Loads + evaluates the plan's `setup(params, system)` function → returns a list of **PlanItem**s.
2. Converts PlanItems into either:
   - **Resource modules** (`@resource/*`) → typed `ResourceParams` (apt/file/pacman/…).
   - Or nested plans (module path) → recursively planned.
3. Validates parameter schemas and values (with span/source error reporting).
4. Expands ResourceParams into a tree of resource **atoms**, then `inject_handlers` grafts `on_change` operations into the tree as conditionally-firing `Handler` leaves.
5. Computes dependency **epochs** over the augmented atom tree (topological layers via Kahn's algorithm).
6. For each epoch in order: probe state for atoms in that epoch, compute changes, expand operations, merge same-family ops within the epoch, apply. Streams structured updates as JSON to stdout.
7. The `lusid` CLI renders a TUI from those updates.

## Principles

- Premature optimization is the root of all evil.
- Do not second guess or make assumptions.
- Prefer robustness over performance.
- Achieve performance with simple fit-for-purpose abstractions, not clever hacks.
- Before adding non-trivial code: confirm the approach is solid, check for simpler alternatives, look at industry tools and existing crates. Complexity is fine when warranted; the point is to be deliberate.

## Working style

- Plain, direct, human-friendly language.
- Value the attention of the reader, be clear and concise, do not be verbose and over-explain.
- No em dashes (—) in prose. Use hyphens, commas, or shorter sentences.
- Don't co-author commits as Claude. Drop the `Co-Authored-By: Claude ...` trailer.
- Comments should age well. Describe intent or a non-obvious constraint, not the change you just made. Avoid "added X for Y", "previously this was Z", or "fixes the case from #123". That belongs in PR descriptions and rots fast.

## Reading order

To understand the runtime behavior, read in this order:
1. `lusid-apply/src/lib.rs` (full pipeline)
2. `plan/src/lib.rs` (planning recursion + resource modules)
3. `params/src/lib.rs` (schema/value validation)
4. `causality/src/epoch.rs` (dependency scheduling)
5. `lusid/src/tui.rs` (how updates are rendered)

## Gotchas / invariants to preserve

### Spans and diagnostics are important
Many errors are `Spanned<...>` or embed `Span` to point to plan source locations. When adding new parsing/validation logic:
- Preserve spans where possible.
- Prefer returning `Spanned<Error>` variants when the error is attributable to a specific value.

### ParamType HostPath vs TargetPath
In `params`:
- `HostPath` expects a **relative** string; it is resolved relative to the source file directory using span source info.
- `TargetPath` expects an **absolute** path string.

If you add new path-like types, follow this pattern and be explicit about absolute/relative requirements.

### `state: "sourced"` vs `state: "linked"`

`@resource/file` and `@resource/directory` both expose two ways to materialise a host-path source on the target:

- **`state: "sourced"`** - byte-copy of the file, or recursive `cp -r` of the directory tree, into `path`. Accepts optional `mode`/`user`/`group`. Edits to `source` only propagate on the next apply. Use this when the bytes need to live on the target independently of the operator's filesystem (system configs, deployed artifacts, dev/remote apply).
- **`state: "linked"`** - atomic symlink at `path` pointing to `source`. Refuses `mode`/`user`/`group` at the parser level (Linux symlinks have no meaningful mode of their own, and chmod/chown via the link silently mutates the target file in the operator's repo - declined). Edits to `source` show up at `path` immediately. Use this for dotfiles-style ergonomics.

Both states validate at plan-load time (post-`plan()`, pre-resources expansion) that `source` exists and has the expected type - regular file for `@resource/file`, directory for `@resource/directory`. See `ResourceParams::validate_host_paths` in `resource/src/lib.rs`.

Implementation notes:
- The Linked state probe is *lexical*: `readlink(2)` against the source string. We deliberately don't canonicalise; otherwise drift between a plan declaring `./foo` and an existing link declaring something else is invisible.
- The Sourced directory state probe is intentionally weak (`path` exists as a directory ⇒ `Sourced`). Content drift in `source` after first apply is not detected; declare `state: "absent"` and re-apply to force a refresh. A content-aware recursive diff is a future direction (cf. Salt's `file.recurse`).

### Causality IDs must be unique
`compute_epochs` fails on duplicate IDs across leaves/branches. Any new code generating ids should avoid collisions (or scope them like `map_plan_subitems()` does by minting a `scope_id`).

### Streaming output protocol
`lusid-apply` emits **newline-delimited JSON** `AppUpdate` messages to stdout.
The `lusid` TUI expects this exact protocol. Avoid printing human text to stdout from `lusid-apply`; use tracing/logging to stderr.

### Resources, operations, and `on_change` hooks

Plans declare two kinds of items, in two namespaces:

- A **resource** (`@resource/<id>`) describes *desired state* - "nginx should be enabled and active". Lusid probes current state, computes a diff, and converges. Idempotent across re-applies.
- An **operation** (`@operation/<id>`) describes an *imperative action* - "reload nginx", "run this command". Operations are not state-checked; they run when triggered.

Resources live at the top level of `setup`. Operations live only inside an `on_change` block.

#### `on_change` hooks

A resource may declare a list of operations to run when it changes. Hooks fire on any change (new contents, different mode, owner change, etc.) and run in a strictly-later resource epoch than the resource's own atoms. Identical hooks landing in the same internal-operation epoch coalesce - ten resources each declaring `on_change: reload nginx`, when their anchors all live in the same resource epoch, collapse to one reload.

```rimu
- module: "@resource/file"
  params: { path: "/etc/nginx/nginx.conf", source: "./nginx.conf", state: "sourced" }
  on_change:
    - module: "@operation/systemd"
      params: { name: "nginx", action: "reload" }
```

A plan item's `id` registers its hooks too: a `requires: [<id>]` dependent waits for both the resource and its hooks. See the `inject_handlers` post-pass below for the mechanism.

#### v1 limitations

- Hooks are inline only - no by-reference (`on_change: ["handler-id"]`).
- Inline operations cannot declare `id`, `requires`, or `required_by`.
- Triggered on any change - no add/modify/remove distinction.
- **Cross-epoch coalescing not handled.** If resource A reloads nginx, resource B also reloads nginx, and B `requires: ["A"]` (so they're in different resource epochs), nginx reloads twice. `Operation::merge` only coalesces within a single internal-operation epoch, which only contains ops emitted from one resource epoch. Workaround: factor the reload into a single dedicated `@resource/command` downstream, or accept the duplicate (nginx reload is idempotent).
- **Hook failure leaves you stuck.** If a hook fails, apply aborts. The resource is now in its target state, so re-applying will NOT re-trigger the hook. Recovery: either run the operation manually (e.g. `sudo systemctl reload nginx`), or briefly toggle a field on the resource (e.g. change `mode` on a `@resource/file`, or `enabled` on a `@resource/systemd`) and re-apply, then revert.
- **`@operation/command` covers a lot.** Although only `command` and `systemd` are exposed as operations in v1, `@operation/command` shells out - logrotate signals, cron reloads, cache invalidation, etc. all fit under it.

#### Implementation: the `inject_handlers` post-pass

Handlers are parsed alongside the rest of a plan item and stashed in `PlanMeta::handlers`. After ResourceParams expansion produces the atom tree (`PlanTree<Resource>`), `inject_handlers` (in `lusid-plan`) walks it branch-by-branch and wraps any branch whose `meta.handlers` is non-empty in this shape:

```
Branch (outer, plan-item id retained, handlers cleared) {
  Branch (anchor, id = SubItem(fresh_scope, "@@handler-anchor")) {
    <original Resource children, recursively transformed>
  },
  Leaf (AtomNode::Handler { operation, anchor_id }, requires = [anchor_id]),
  ... (one leaf per handler op)
}
```

The wrap is **unconditional** - we don't yet know which atoms will resolve to a change. The Resource leaves inside the anchor are converted to `AtomNode::Resource { resource, anchor_ids }`, where `anchor_ids` is the stack of all anchors the leaf lives under (so a Resource inside two nested anchors gets both ids).

Per causality's branch-as-group semantics, the anchor branch's id covering its leaves means each handler leaf (which `requires` the anchor id) lands in a resource epoch strictly later than every Resource atom under the anchor. The outer branch's plan-item id covers both the anchor and the handler leaves, so any plan item with `requires: [<plan-item-id>]` correctly waits for both.

**Conditional firing** happens in `lusid-apply::apply`: it maintains a `HashSet<PlanNodeId>` of anchors that fired during this run. As Resource atoms are probed and changed per-epoch, the set is updated for every anchor in their `anchor_ids` list. When a Handler atom's epoch arrives, the apply loop checks the set and emits the handler operation iff the anchor is present.


## Build / run / test (agent checklist)

### Typical commands
- Build workspace:
  - `cargo build`
- Run CLI:
  - `cargo run -p lusid -- --help`
- Run apply binary (manual):
  - `cargo run -p lusid-apply -- --root <root> --plan <path/to/plan.lusid> --log info --params '{"k":"v"}'`
- Run tests:
  - `cargo test`
- Lint:
  - `cargo clippy --workspace -- -D warnings`
- Format:
  - `cargo fmt --all`

## Coding style expectations (match existing code)

- Error handling uses `thiserror` + rich enums; avoid `anyhow`-style catchalls.
- Many crates use `displaydoc::Display` for error messages; follow that pattern.
- Use a blank line between each error enum variant.
- Prefer small pure functions
- Keep public APIs conservative: prefer adding new types/functions instead of changing signatures.
- Maintain `Clone` friendliness when types are used in trees/flat arenas.
- Import order: std, external crates, internal crates (`lusid_*`), within crate (`crate::`/`self::`/`super::`), with a blank line between each group.

## Safety and operational concerns

This project runs privileged operations (`sudo apt-get`, `sudo pacman`, filesystem ownership changes). When adding new operations:
- Ensure commands are non-interactive.
- Avoid leaking secrets in logs/structured UI updates.
- Keep stdout/stderr streaming for long-running commands.

## Reviews

- Think about long-term maintenance.
- Verify algorithms against relevant specs.
- Check `unsafe` usage is correct and documented with `SAFETY` comments.
- Look for simpler ways to do (or say) the same thing.
- Compare current abstractions against alternatives.
- Add `debug_assert!` to validate assumptions.
- For observations that don't lead to a change now: `Note(cc): xxx` for future readers, `TODO(cc): xxx` if we should change it later.

## Testing

- Don't assume the current code is correct.
- Never change a test just to make it pass - fix the cause.
- Add tests for specific edge cases, not for the sake of count.
- Remove redundant tests.

## Tracing

- Use `tracing::instrument` or manual spans where they add context.
- Use all levels deliberately: `error!` for breakage, `warn!` for degraded-but-recoverable, `info!` for lifecycle events, `debug!` for operational detail, `trace!` for per-frame/hot-path detail.
- Prefer structured fields (`info!(plan_id = %id, "planning complete")`) over string interpolation.
- Write log messages as if you will read them at 3 AM debugging a production issue two years from now.

## Before submitting changes (AI agent self-check)

- Does the change preserve span-aware errors where applicable?
- Does it maintain the stdout JSON protocol from `lusid-apply`?
- Did you avoid printing non-JSON to stdout in apply?
- Are causality IDs still unique and dependencies valid?
- Are new operations safe/non-interactive and appropriately `sudo()`-wrapped?
- Did you add/adjust tests for logic-heavy changes?
