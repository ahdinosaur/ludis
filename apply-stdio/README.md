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
3. `PipelineInfo { resource_epochs_total, atom_epoch }` - once-per-apply
   summary emitted after `ResourcesComplete` and before any per-epoch event.
   Ships the total number of resource epochs and a mapping from each atom
   arena index to its resource epoch.

   Under `--parse-only` the stream stops here: events (1) through (3) fire,
   then `lusid-apply` exits without probing target state or running ops.
4. Per resource epoch, in strict order:

   a. **Probe**: per-atom `ResourceStatesNodeStart` /
      `ResourceStatesNodeComplete { state: ResourceState }`. Atoms in the
      same epoch probe in parallel, so their events interleave.

   b. **Diff**: per-atom `ResourceChangesNode { change: Option<ResourceChange> }`.
      `None` is a no-op leaf and prunes the leaf from later projections.

   c. **Plan ops**: per-changed-atom
      `OperationsNode { operations: PlanTree<Operation> }`.

   d. **Confirm**: `EpochReady { resource_epoch, summary }` fires after
      (a)-(c) complete for this epoch, but only if at least one atom changed
      or a handler is queued (otherwise the epoch is empty and skipped).
      The producer then blocks reading one line of [`AckAction`] JSON from
      stdin (`{"action":"apply"}` / `{"action":"abort"}`). `--yes` skips
      both the emission and the read; EOF or parse error is treated as
      `Abort` and halts the apply.

   e. **Phase A**:
      `OperationsApplyEpochAdded { epoch_index, resource_epoch, phase: A, operations }`
      with merged change ops, then per-op
      `OperationApplyStart` / `OperationApplyStdout` / `OperationApplyStderr` /
      `OperationApplyComplete` events.

   f. **Phase B**: same shape with `phase: B`, carrying the `on_change`
      handlers for any plan-item branch whose latest atom landed in this
      epoch and which had at least one atom change.

5. `ApplyComplete { had_changes }`. Terminal. `lusid-apply` exits 0
   immediately after; consumers should flush and close. Exit code is
   non-zero on `Abort` (`AbortedByUser`) or any apply error.

`epoch_index` on `OperationsApplyEpochAdded` is a global monotonic counter
across Phase A and Phase B; multiple events can share a `resource_epoch`.
Empty resource epochs / empty phases emit no event and consume no index.

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

`AppView` also surfaces pipeline-level metadata from `PipelineInfo`
(`resource_epochs_total`, `epoch_of_atom`) and per-op-epoch metadata
(`resource_epoch`, `phase`) so the Epochs page can group ops without
re-running `compute_epochs`.
