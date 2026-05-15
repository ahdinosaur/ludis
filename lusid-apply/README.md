# lusid-apply

Pipeline orchestrator. Loads a plan, builds the atom tree, schedules atoms into dependency epochs, and processes each epoch in order - probing state, computing changes, expanding operations, and executing them - streaming progress as newline-delimited JSON [`AppUpdate`](../apply-stdio)s on stdout for the [`lusid`](../lusid) TUI.

Shipped as both a library (`lusid_apply::apply`) and a binary (`lusid-apply`). Entry point: `src/lib.rs::apply()`.

## Pipeline

1. **Plan** - [`lusid_plan::plan`] evaluates Rimu, produces `PlanTree<ResourceParams>`.
2. **Resources** - each plan node expands into 1+ typed resource atoms ([`map_plan_subitems`] scopes intra-resource ids).
3. **Inject handlers** - [`lusid_plan::inject_handlers`] grafts each plan-item branch's `on_change` operations into the atom tree as `Handler` leaves under a synthetic anchor branch. Resource atoms gain an `anchor_ids` list recording which anchor(s) cover them.
4. **Epoch scheduling** - [`lusid_causality::compute_epochs`] orders atoms into topological layers.
5. **Per-epoch processing** - for each layer in order:
   - probe state for `Resource` atoms in parallel,
   - compute change per atom; record any change against every anchor covering that atom,
   - decide each `Handler` atom by checking whether its anchor was marked changed (handlers are always in epochs strictly later than every atom under their anchor),
   - combine per-atom op subtrees + emitted handler ops, compute INTERNAL operation epochs, and apply each with same-family merging.

State is probed *per epoch*, not upfront. By the time atoms in epoch N are probed, every atom in epochs 0..N has already been applied, so probes see fresh-from-disk state. This matters when a prior epoch creates the file or installs the package being probed.

Within each epoch, [`Operation::merge`] still coalesces like-typed operations (e.g. multiple `apt install` → one multi-package call). Coalescing does NOT cross epochs; identical operations in two different epochs run twice.

## Protocol

JSON `AppUpdate`s on stdout; tracing on stderr - nothing else. See [`apply-stdio`](../apply-stdio/README.md) for the message enum.

## CLI

```
lusid-apply --root <path> --plan <path.lusid> [--params '{"k":"v"}'] [--log info]
```
