# lusid-apply

Pipeline orchestrator. Loads a plan, builds the atom tree, schedules atoms into dependency epochs, and processes each epoch in order - probing state, computing changes, expanding operations, and executing them - streaming progress as newline-delimited JSON [`AppUpdate`](../apply-stdio)s on stdout for the [`lusid`](../lusid) TUI.

Shipped as both a library (`lusid_apply::apply`) and a binary (`lusid-apply`). Entry point: `src/lib.rs::apply()`.

## Pipeline

1. **Plan** - [`lusid_plan::plan`] evaluates Rimu, produces `PlanTree<ResourceParams>`.
2. **Resources** - each plan node expands into 1+ typed resource atoms ([`map_plan_subitems`] scopes intra-resource ids). `on_change` operations stay on each plan-item branch's `PlanMeta::handlers`; they are not lifted into the atom tree.
3. **Epoch scheduling** - [`lusid_causality::compute_epochs`] orders atoms into topological layers.
4. **Per-epoch processing** - for each layer in order:
   - **Phase A**: probe state for atoms in parallel, compute change per atom, run change ops through `compute_epochs` + `Operation::merge` and apply. When an atom changes, the apply loop walks parent links to find the nearest enclosing plan-item branch whose `meta.handlers` is non-empty and marks it in `changed_branches`.
   - **Phase B**: for every handler-bearing branch whose latest atom is in this epoch and which is in `changed_branches`, collect its `on_change` operations and apply them through the same merge + apply flow. Phase B fires after Phase A's ops complete and before the next epoch's Phase A begins, so handlers run strictly after the atoms they watch and strictly before any dependent's atoms.

State is probed *per epoch*, not upfront. By the time atoms in epoch N are probed, every atom in epochs 0..N has already been applied, so probes see fresh-from-disk state. This matters when a prior epoch creates the file or installs the package being probed.

Within each epoch, [`Operation::merge`] coalesces like-typed operations (e.g. multiple `apt install` → one multi-package call) per phase. Coalescing does NOT cross epochs; identical operations in two different epochs run twice.

## Protocol

JSON `AppUpdate`s on stdout; tracing on stderr - nothing else. See [`apply-stdio`](../apply-stdio/README.md) for the message enum.

## CLI

```
lusid-apply --root <path> --plan <path.lusid> [--params '{"k":"v"}'] [--log info]
```
