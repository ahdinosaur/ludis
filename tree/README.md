# lusid-tree

Generic tree data structures used throughout lusid.

Two representations:

- **`Tree<Node, Meta>`** - recursive, nested. `Branch` owns its children directly; `Leaf` holds a value. Every node carries a `Meta` payload.

- **`FlatTree<Node, Meta>`** - arena-backed. Nodes live in `Vec<Option<FlatTreeNode>>` and reference children by index. The `Option` layer lets us tombstone removed nodes without shifting indices - important because callers hold onto indices as stable identifiers.

`FlatTree` supports a series of `map_*` passes that transform leaves while preserving structure and indices. The async map variants take `write_start` / `write_update` callbacks for per-node progress reporting.

## Invariants

- Root is always at index 0.
- Lenient reconstruction: missing or out-of-bounds children are tolerated.
- `replace_tree(index)` recursively tombstones existing descendants, then appends new children to the end of the arena; the original node keeps its slot.
- Depth-first traversal is post-order.
