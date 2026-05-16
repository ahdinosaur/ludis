# lusid-apply-stdio

Shared wire protocol between `lusid-apply` (producer) and the `lusid` TUI
(consumer). Both crates depend on this one so every message is typed at
both ends.

## Message stream

[`AppUpdate`](src/lib.rs) is a `Serialize`+`Deserialize` enum. `lusid-apply`
emits one update per newline on stdout; the TUI deserializes each and folds
it into its [`AppView`](src/lib.rs) state.

Variants carry the structured domain types ([`ResourceParams`], [`Resource`],
[`ResourceState`], [`ResourceChange`], [`Operation`], `PlanTree<...>`) directly;
there is no intermediate view layer. The consumer renders to text on demand
via [`lusid-render`](../render).

Outline:

1. `ResourceParams { resource_params: PlanTree<ResourceParams> }` - the plan
   tree with typed params filled in.
2. `ResourcesStart` / `ResourcesNode { index: 0, tree: PlanTree<Resource> }`
   / `ResourcesComplete` carrying the full atoms tree, one leaf per resource
   atom.
3. Per resource epoch, interleaved across atoms:
   - `ResourceStatesNodeStart` / `ResourceStatesNodeComplete { state: ResourceState }`
     per leaf.
   - `ResourceChangesNode { change: Option<ResourceChange> }` per leaf;
     `None` is a no-op leaf.
   - `OperationsNode { operations: PlanTree<Operation> }` per changed leaf.
   - `OperationsApplyEpochAdded { operations: Vec<Operation> }` + per-op
     apply events for Phase A; same events repeated for Phase B's
     `on_change` handlers.
4. `ApplyComplete { had_changes }`.

## AppView

A per-leaf state machine over the atoms tree, plus the operations apply
pane. Each resource atom advances through `Planned -> Probing -> Probed ->
NoChange | Changed { ops: None } -> Changed { ops: Some }`; per-leaf events
trigger transitions and invalid (state, event) pairs return
`AppViewError::InvalidLeafTransition`. See [`LeafState`](src/lib.rs).

The TUI navigates four per-stage projections (resources / states / changes /
operations) of the leaf states, built on demand by
[`AppView::resources_view`](src/lib.rs) and friends. Each projection is a
`ProjectedTree<T>` over the structured payload `T`; the TUI lowers nodes to
ratatui text by calling `lusid_render::Render::render`.
