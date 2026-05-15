# lusid-causality

Dependency ordering for tree-structured workloads.

Wraps [`lusid_tree::Tree`] with [`CausalityMeta`]: each node gets an optional `id` plus `requires` and `required_by` lists. [`compute_epochs`] flattens the tree into topologically-sorted layers ("epochs") using Kahn's algorithm — each epoch holds nodes with no remaining dependencies, safe to run in parallel.

Algorithm lives in `src/epoch.rs`; `CausalityMeta` in `src/tree.rs`.

## Semantics

- **Branch-inherited constraints.** A branch's `requires` / `required_by` apply to every descendant leaf.
- **Group ids.** A branch's `id` refers to the set of all descendant leaves — depending on a branch id means depending on every leaf under it.
- **Marker leaves.** Leaves with a `None` node are kept in the dependency graph (so their ids still resolve) but excluded from the epoch output.
- **Unique ids.** Duplicates are a hard error.
