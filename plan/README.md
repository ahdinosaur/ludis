# lusid-plan

Load a `.lusid` file, run its `setup(params, system)` function, and recursively produce a tree of typed resource params.

Entry point: `plan()` in `src/lib.rs`.

## Pipeline

For each `.lusid` source:

1. **Read** bytes from the [`Store`](../store).
2. **Load** — parse + evaluate Rimu, project into a [`Plan`](src/model.rs) (name, version, params schema, setup function).
3. **Validate** user params against the plan's schema (via [`lusid-params`](../params)).
4. **Evaluate** `setup(params, system)` to get a list of [`PlanItem`](src/model.rs)s.
5. **Convert** each item:
   - `module: "@resource/<id>"` → leaf with typed [`ResourceParams`](../resource).
   - Otherwise → sibling `.lusid` path, recurse into a branch.

The returned [`PlanTree<ResourceParams>`] preserves `id` / `requires` / `required_by` in [`PlanMeta`](src/tree.rs) (a `CausalityMeta<PlanNodeId>`) so downstream epoch scheduling can honour ordering.

## Identifier scopes

Three kinds of [`PlanNodeId`]:

- **`Plan`** — the root of a plan.
- **`PlanItem { plan_id, item_id }`** — user-authored `id:`, scoped by its plan.
- **`SubItem { scope_id, item_id }`** — minted *inside* a resource's expansion (e.g. ordering `chmod` after the initial write). Each `map_plan_subitems` call mints a fresh `scope_id`, so inner ids can never collide.

## Resource modules

`src/resource.rs` is the dispatch table — adding a resource means an arm here plus the pieces in [`lusid-resource`](../resource).
