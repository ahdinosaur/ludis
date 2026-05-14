# lusid-resource

User-facing resource types — the "thing I want on my machine" layer.

Each resource (`apt`, `file`, `pacman`, `command`, `git`) implements the
[`ResourceType`] trait, which captures the same five-step pipeline:

1. **Params** — friendly user-facing struct, deserialised from the plan's Rimu
   value using the declared [`ParamTypes`] schema.
2. **Resource** — one or more indivisible "atoms" produced from Params. A single
   `apt { packages: [a, b] }` param expands to two atoms. Atoms are arranged in a
   [`CausalityTree`] so intra-resource ordering (e.g. `chmod` after `write`) can
   be declared.
3. **State** — current observed state for an atom (e.g. `Installed` /
   `NotInstalled`).
4. **Change** — delta from State to the desired Resource. `None` = already correct.
5. **Operations** — concrete actions (apt install, write file, …) derived from
   the Change. Defined in the `lusid-operation` crate.

The crate-level `Resource{Params,,State,Change}` enums are thin dispatchers —
each variant boxes the per-type data and delegates through the trait.

## Adding a new resource

1. New module under `src/resources/`.
2. Implement `ResourceType` for a zero-sized marker type (`struct MyResource;`).
3. Add a variant to each of: `ResourceParams`, `Resource`, `ResourceState`,
   `ResourceStateError`, `ResourceChange`.
4. Thread it through the five `match` arms in `src/lib.rs`.
5. Register the resource module in `lusid-plan` so plans can reference `@resource/<id>`.

## Conventions

- Resource structs/enums implement `Display` via `impl_display_render!`, giving
  them a `Render` impl that the TUI uses for human-readable updates.
- Params types use `#[serde(tag = "...")]` or `#[serde(untagged)]` to match
  the union arms declared in `param_types()`. Keep the two in sync.
- `change()` returns `None` for "already matches" — avoid emitting trivial
  operations.
