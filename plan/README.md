# lusid-plan

Planning: load a `.lusid` file, run its `setup(params, system)` function, and
recursively produce a tree of typed resource params.

## Pipeline

`plan()` is the only public entry. For one `.lusid` source, `plan_recursive()`:

1. **Read** plan bytes from the [`Store`](../store).
2. **Load** — parse + evaluate Rimu, project into a [`Plan`](src/model.rs)
   (name, version, params schema, setup function).
3. **Validate** user params against the plan's schema (via
   [`lusid-params`](../params)).
4. **Evaluate** the setup function with `(params, system)` to get a list of
   [`PlanItem`](src/model.rs)s.
5. **Convert** each item:
   - `module: "@resource/<id>"` → leaf with typed [`ResourceParams`](../resource).
   - Otherwise → sibling `.lusid` path, recurse into a branch.

The returned [`PlanTree<ResourceParams>`] preserves
`id` / `requires` / `required_by` in [`PlanMeta`](src/tree.rs) (a
`CausalityMeta<PlanNodeId>`) so downstream epoch scheduling can honour ordering.

## Identifier scopes

Three kinds of [`PlanNodeId`]:

- **`Plan`** — the root of a plan.
- **`PlanItem { plan_id, item_id }`** — user-authored `id:` on a plan item; scoped
  by the plan it came from.
- **`SubItem { scope_id, item_id }`** — an id minted *inside* a resource's
  expansion (e.g. `"file"` used by the file resource to order mode/user/group
  after the initial write). Each `map_plan_subitems` call mints a fresh `cuid2`
  `scope_id`, so inner ids can never collide across resources.

## Resource modules

Built-in resources live under `@resource/<id>`: `apt`, `file`, `pacman`,
`command`, `git`. See [`src/resource.rs`](src/resource.rs) for the dispatch
table — adding a new resource means adding an arm here plus the pieces in
[`lusid-resource`](../resource).
