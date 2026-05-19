# lusid-resource

User-facing resource types - the "thing I want on my machine" layer.

Each resource implements [`ResourceType`], a five-step pipeline:

1. **Params** - user-facing struct, parsed from the plan's Rimu value.
2. **Resource** - one or more atoms produced from Params. A single `apt { packages: [a, b] }` expands to two atoms. Atoms sit in a [`CausalityTree`] so intra-resource ordering (e.g. `chmod` after `write`) can be declared.
3. **State** - observed state for an atom (e.g. `Installed` / `NotInstalled`).
4. **Change** - delta from State to Resource. `None` means already correct.
5. **Operations** - concrete actions derived from Change. Defined in [`lusid-operation`](../operation).

`ResourceParams`, `Resource`, `ResourceState`, `ResourceChange` are dispatch enums; each variant delegates to the per-type trait impl.

## Adding a new resource

1. New module under `src/resources/`.
2. Implement `ResourceType` for a zero-sized marker (`struct MyResource;`).
3. Add a variant to each of: `ResourceParams`, `Resource`, `ResourceState`, `ResourceStateError`, `ResourceChange`.
4. Thread it through the match arms in `src/lib.rs`.
5. Register the module in [`lusid-plan`](../plan) so plans can reference `@resource/<id>`.

## Notes

- Resource types implement `Display` for human-facing text. [`lusid-render`](../render) attaches a `Render` impl per type via its `display_render!` macro so the TUI can lower the structured wire to ratatui output.
- Params types use `#[serde(tag = "...")]` or `#[serde(untagged)]` to match the union arms in `param_types()`. Keep the two in sync.
