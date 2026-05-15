# lusid-apply

Pipeline orchestrator. Loads a plan, builds the resource → state → change → operation trees, schedules operations by dependency epoch, and executes them — streaming progress as newline-delimited JSON [`AppUpdate`](../apply-stdio)s on stdout for the [`lusid`](../lusid) TUI.

Shipped as both a library (`lusid_apply::apply`) and a binary (`lusid-apply`). Entry point: `src/lib.rs::apply()`.

## Pipeline

1. **Plan** — [`lusid_plan::plan`] evaluates Rimu, produces `PlanTree<ResourceParams>`.
2. **Resources** — each plan node expands into 1+ typed resources ([`map_plan_subitems`] scopes intra-resource ids).
3. **ResourceStates** — async `Resource::state()` probes, one per leaf.
4. **ResourceChanges** — diff `(Resource, State) → Option<Change>`; `None` leaves are pruned. Apply short-circuits if zero changes remain.
5. **Operations** — each change expands into an operation subtree.
6. **Epoch scheduling** — [`lusid_causality::compute_epochs`] orders operations into topological layers.
7. **Apply** — per-epoch, [`Operation::merge`] coalesces like-typed operations (e.g. multiple `apt install` → one multi-package call); each is executed with stdout/stderr streamed as events.

## Protocol

JSON `AppUpdate`s on stdout; tracing on stderr — nothing else. See [`apply-stdio`](../apply-stdio/README.md) for the message enum.

## CLI

```
lusid-apply --root <path> --plan <path.lusid> [--params '{"k":"v"}'] [--log info]
```
