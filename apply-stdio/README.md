# lusid-apply-stdio

Shared wire protocol between `lusid-apply` (producer) and the `lusid` TUI
(consumer). Both crates depend on this one so every message is typed at
both ends.

## Message stream

[`AppUpdate`](src/lib.rs) is a `Serialize`+`Deserialize` enum. `lusid-apply`
emits one update per newline on stdout; the TUI deserializes each and folds
it into its [`AppView`](src/lib.rs) state.

Pipeline phases, each bracketed by a `*Start` and `*Complete`:

1. **ResourceParams** - the plan tree with typed params filled in.
2. **Resources** - per-node resource construction (`ResourceParams → Resource`).
3. **ResourceStates** - per-leaf state probe (async, emits `NodeStart` /
   `NodeComplete`).
4. **ResourceChanges** - diff of desired vs. actual; leaves with no change
   are dropped via `set_node_none`.
5. **Operations** - per-node operation tree expansion.
6. **OperationsApply** - per-epoch, per-operation execution; streams live
   `stdout`/`stderr` + final exit state.

## AppView

A phase-tagged enum that accumulates one [`FlatViewTree`](src/lib.rs) per
stage. Each new phase clones the prior phase's tree via
[`template()`](src/lib.rs) - same shape, leaves reset to `NotStarted` - so
the TUI can show the eventual layout immediately and fill it in as events
arrive.

## FlatViewTree

Arena-backed, root at index `0`, children are indices - same shape as
[`lusid_tree::FlatTree`](../tree) but carrying [`lusid_view::View`] (branches)
and `ViewNode` (leaves, with not-started/started/complete progress). Rendering
to text goes via `ViewTree` + `termtree` and is lenient about missing or
out-of-bounds entries.
