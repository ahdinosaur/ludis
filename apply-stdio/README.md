# lusid-apply-stdio

Shared wire protocol between `lusid-apply` (producer) and the `lusid` TUI
(consumer). Both crates depend on this one so every message is typed at
both ends.

## Message stream

[`AppUpdate`](src/lib.rs) is a `Serialize`+`Deserialize` enum. `lusid-apply`
emits one update per newline on stdout; the TUI deserializes each and folds
it into its [`AppView`](src/lib.rs) state.

Outline:

1. `ResourceParams` - the plan tree with typed params filled in.
2. `ResourcesStart` / `ResourcesNode { index: 0, tree }` / `ResourcesComplete`
   carrying the full atoms tree, one leaf per resource atom.
3. Per resource epoch, interleaved across atoms:
   - `ResourceStatesNodeStart` / `ResourceStatesNodeComplete` per leaf.
   - `ResourceChangesNode { node: Option<View> }` per leaf; `None` is a
     no-op leaf.
   - `OperationsNode { operations: ViewTree }` per changed leaf.
   - `OperationsApplyEpochAdded` + per-op apply events for Phase A; same
     events repeated for Phase B's `on_change` handlers.
4. `ApplyComplete { had_changes }`.

## AppView

A per-leaf state machine over the atoms tree, plus the operations apply
pane. Each resource atom advances through `Planned -> Probing -> Probed ->
NoChange | Changed { ops: None } -> Changed { ops: Some }`; per-leaf events
trigger transitions and invalid (state, event) pairs return
`AppViewError::InvalidLeafTransition`. See [`LeafState`](src/lib.rs).

The TUI navigates four per-stage trees (resources / states / changes /
operations). These are projections of the leaf states, built on demand by
[`AppView::resources_view`](src/lib.rs) and friends.

## FlatViewTree

Arena-backed, root at index `0`, children are indices - same shape as
[`lusid_tree::FlatTree`](../tree) but carrying [`lusid_view::View`] (branches)
and `ViewNode` (leaves, with not-started/started/complete progress). Rendering
to text goes via `ViewTree` + `termtree` and is lenient about missing or
out-of-bounds entries.
