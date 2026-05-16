//! Wire protocol between `lusid-apply` (producer) and the `lusid` TUI (consumer).
//!
//! `lusid-apply` emits newline-delimited JSON [`AppUpdate`]s on stdout as the
//! pipeline progresses. The TUI deserializes each update and folds it into an
//! [`AppView`]: a per-leaf state machine over the atoms tree, plus the
//! operations apply pane.
//!
//! ## Wire shape
//!
//! Variants carry serde-derived domain types from `lusid-plan`,
//! `lusid-resource`, and `lusid-operation`. The consumer renders these to
//! display text on demand; the producer ships structured data only.
//!
//! ## Pipeline shape
//!
//! 1. Plan source -> [`AppUpdate::ResourceParams`] (full tree, one event).
//! 2. Resource expansion -> [`AppUpdate::ResourcesNode`] carrying the full
//!    atoms tree (one event at index 0, bracketed by `ResourcesStart` and
//!    `ResourcesComplete`). One leaf per resource atom; no synthetic nodes
//!    for `on_change` handlers (those fire in Phase B and appear only in the
//!    apply pane).
//! 3. Per resource epoch (in causality order):
//!    - state probe events for atoms in this epoch,
//!    - change events,
//!    - operations sub-tree events,
//!    - Phase A: per-internal-operation-epoch
//!      [`AppUpdate::OperationsApplyEpochAdded`] with merged change ops,
//!      then per-op apply events,
//!    - Phase B: handler ops for any plan item whose latest atom was in
//!      this epoch and which had at least one atom change. Same lifecycle
//!      events as Phase A, advancing the same `op_epoch_index` counter.
//! 4. [`AppUpdate::ApplyComplete`].
//!
//! Events for different leaves interleave: state events for atoms in epoch 1
//! arrive after apply events for epoch 0's ops, and per-epoch atoms probe in
//! parallel. Each per-leaf event drives one leaf's state machine; invalid
//! (prior-state, event) pairs return [`AppViewError::InvalidLeafTransition`].
//!
//! ## State model
//!
//! [`LeafState`] captures each atom's lifecycle, carrying structured domain
//! payloads (no rendering at this layer). The four per-stage projections the
//! TUI navigates (resources / states / changes / operations) are built on
//! demand via [`AppView::resources_view`] and friends; each returns a
//! [`ProjectedTree`] whose leaves carry [`Lifecycle<T>`] over the relevant
//! domain type. [`PipelineProgress`] is a coarse classification of the whole
//! apply derived from one leaf walk.

use lusid_operation::Operation;
use lusid_plan::{PlanMeta, PlanTree};
use lusid_resource::{Resource, ResourceChange, ResourceParams, ResourceState};
use lusid_tree::Tree;
use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Protocol message from `lusid-apply` to the TUI.
///
/// Events from per-epoch processing arrive interleaved: state events for
/// atoms in epoch N can arrive after apply events for ops in epoch N-1, and
/// per-epoch atoms probe in parallel. The TUI does not enforce a global
/// phase order, but per-leaf transitions are strict - see [`LeafState`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AppUpdate {
    /// Full resource-params tree, one event up front. Carries the planner's
    /// [`PlanMeta`] per branch (id / requires / required_by / on_change
    /// handlers) so consumers can render labels, dependency annotations, and
    /// handler counts without re-rendering at the producer.
    ResourceParams {
        resource_params: PlanTree<ResourceParams>,
    },

    /// Begin filling in the atoms tree - the resource atoms produced by
    /// expanding each plan item's `ResourceParams`. One leaf per atom; no
    /// synthetic nodes for `on_change` handlers (those fire in Phase B and
    /// appear only in the apply pane).
    ResourcesStart,
    ResourcesNode {
        index: usize,
        tree: PlanTree<Resource>,
    },
    ResourcesComplete,

    /// Per-leaf state-probe lifecycle events. `Start` fires when the probe
    /// future is dispatched; `Complete` fires when it resolves.
    ResourceStatesNodeStart {
        index: usize,
    },
    ResourceStatesNodeComplete {
        index: usize,
        state: ResourceState,
    },

    /// Per-leaf computed change. `change` is `None` when the diff is empty
    /// (no-op); the TUI prunes the leaf in that case.
    ResourceChangesNode {
        index: usize,
        change: Option<ResourceChange>,
    },

    /// Per-leaf operations subtree (one or more concrete operations to
    /// execute). Replaces the leaf at `index` in the operations tree.
    OperationsNode {
        index: usize,
        operations: PlanTree<Operation>,
    },

    /// One internal operation epoch's merged op list, appended to the apply
    /// pane. The TUI grows `operations_epochs` as these arrive.
    OperationsApplyEpochAdded {
        epoch_index: usize,
        operations: Vec<Operation>,
    },

    /// Per-operation lifecycle during apply. `index = (op_epoch_index, op_index)`,
    /// where `op_epoch_index` is the running counter across all
    /// `OperationsApplyEpochAdded` events.
    OperationApplyStart {
        index: (usize, usize),
    },
    OperationApplyStdout {
        index: (usize, usize),
        stdout: String,
    },
    OperationApplyStderr {
        index: (usize, usize),
        stderr: String,
    },
    OperationApplyComplete {
        index: (usize, usize),
        error: Option<String>,
    },

    /// Final marker. Carries `had_changes` so the TUI can show the right
    /// "complete" message ("No changes" vs "Apply complete").
    ApplyComplete {
        had_changes: bool,
    },
}

/// One operation's live state during the apply phase. `stdout`/`stderr` are
/// appended to as `OperationApplyStdout`/`OperationApplyStderr` arrive; the
/// TUI renders the tail of these in the per-operation pane.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OperationView {
    pub label: Operation,
    pub stdout: String,
    pub stderr: String,
    pub is_complete: bool,
    pub error: Option<String>,
}

impl OperationView {
    fn new(label: Operation) -> Self {
        Self {
            label,
            stdout: String::new(),
            stderr: String::new(),
            is_complete: false,
            error: None,
        }
    }
}

/// Per-leaf lifecycle. Each atom in the atoms tree transitions through these
/// states monotonically as `AppUpdate` events arrive. Transitions are
/// validated at runtime; an invalid (prior_state, event) pair returns
/// `AppViewError::InvalidLeafTransition`.
///
/// ```text
/// Planned -> Probing -> Probed -> NoChange   (terminal)
///                            \-> Changed { ops: None } -> Changed { ops: Some }
/// ```
///
/// The lifecycle is per-leaf; events for different leaves interleave because
/// the apply loop probes each epoch's atoms in parallel.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(clippy::large_enum_variant)]
pub enum LeafState {
    Planned {
        resource: Resource,
    },
    Probing {
        resource: Resource,
    },
    Probed {
        resource: Resource,
        state: ResourceState,
    },
    NoChange {
        resource: Resource,
        state: ResourceState,
    },
    Changed {
        resource: Resource,
        state: ResourceState,
        change: ResourceChange,
        /// Populated when `OperationsNode` arrives. The `u64` is a monotonic
        /// arrival counter used to pin splice order during the operations
        /// projection so arena indices stay stable across renders.
        ops: Option<(PlanTree<Operation>, u64)>,
    },
}

impl LeafState {
    /// Stable name for error messages.
    fn name(&self) -> &'static str {
        match self {
            LeafState::Planned { .. } => "Planned",
            LeafState::Probing { .. } => "Probing",
            LeafState::Probed { .. } => "Probed",
            LeafState::NoChange { .. } => "NoChange",
            LeafState::Changed { ops: None, .. } => "Changed { ops: None }",
            LeafState::Changed { ops: Some(_), .. } => "Changed { ops: Some }",
        }
    }

    pub fn resource(&self) -> &Resource {
        match self {
            LeafState::Planned { resource }
            | LeafState::Probing { resource }
            | LeafState::Probed { resource, .. }
            | LeafState::NoChange { resource, .. }
            | LeafState::Changed { resource, .. } => resource,
        }
    }
}

/// Arena tree of resource atoms. Root index is 0. Branches carry the
/// planner's [`PlanMeta`] (id / requires / required_by / handlers); leaves
/// carry per-atom state. The per-stage projections shown in the TUI are
/// derived from this tree.
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct ResourcesTree {
    pub nodes: Vec<Option<ResourcesNode>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(clippy::large_enum_variant)]
pub enum ResourcesNode {
    Branch {
        meta: PlanMeta,
        children: Vec<usize>,
    },
    Leaf {
        state: LeafState,
    },
}

impl ResourcesTree {
    /// Iterate over every live leaf's state, in arena order.
    pub fn leaves(&self) -> impl Iterator<Item = &LeafState> {
        self.nodes.iter().filter_map(|n| match n.as_ref()? {
            ResourcesNode::Leaf { state } => Some(state),
            ResourcesNode::Branch { .. } => None,
        })
    }
}

/// Per-stage lifecycle marker for a [`ProjectedTree`] leaf. Tracks
/// pre-event / in-flight / completed phases parallel to [`LeafState`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Lifecycle<T> {
    NotStarted,
    Started,
    Complete(T),
}

impl<T> Lifecycle<T> {
    pub fn as_complete(&self) -> Option<&T> {
        match self {
            Lifecycle::Complete(value) => Some(value),
            _ => None,
        }
    }
}

/// Arena node in a [`ProjectedTree`]. Branches carry the source [`PlanMeta`];
/// leaves carry a [`Lifecycle`] over the relevant domain payload.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(clippy::large_enum_variant)]
pub enum ProjectedNode<T> {
    Branch {
        meta: PlanMeta,
        children: Vec<usize>,
    },
    Leaf {
        lifecycle: Lifecycle<T>,
    },
}

/// Arena-backed projection of the atoms tree, parameterised by the per-leaf
/// payload type (one of `ResourceParams`, `Resource`, `ResourceState`,
/// `ResourceChange`, `Operation`). Slots are `Vec<Option<Node>>`; missing
/// children / out-of-bounds indices are tolerated (lenient rendering).
/// Subtrees are appended; "replace subtree at index" recursively clears the
/// old children before writing the new.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ProjectedTree<T> {
    nodes: Vec<Option<ProjectedNode<T>>>,
}

#[derive(Debug, Error)]
pub enum ProjectedTreeError {
    #[error("index {0} is out of bounds")]
    IndexOutOfBounds(usize),

    #[error("node at index {0} is None")]
    NodeMissing(usize),
}

impl<T> ProjectedTree<T> {
    pub const fn root_index() -> usize {
        0
    }

    pub fn nodes(&self) -> &[Option<ProjectedNode<T>>] {
        &self.nodes
    }

    pub fn get(&self, index: usize) -> Result<&ProjectedNode<T>, ProjectedTreeError> {
        let slot = self
            .nodes
            .get(index)
            .ok_or(ProjectedTreeError::IndexOutOfBounds(index))?;
        slot.as_ref().ok_or(ProjectedTreeError::NodeMissing(index))
    }
}

impl<T: Clone> ProjectedTree<T> {
    /// Replace the subtree at `root_index` with a [`PlanTree<T>`]; each leaf
    /// becomes `Lifecycle::Complete(node)` and each branch keeps its meta.
    /// Pre-existing descendants are recursively tombstoned.
    fn splice_plan_subtree(&mut self, root_index: usize, subtree: PlanTree<T>) {
        if let Some(Some(ProjectedNode::Branch { children, .. })) = self.nodes.get(root_index) {
            for child in children.clone() {
                tombstone_subtree(&mut self.nodes, child);
            }
        }
        replace_with_plan_subtree(&mut self.nodes, root_index, subtree);
    }
}

fn tombstone_subtree<T>(nodes: &mut Vec<Option<ProjectedNode<T>>>, root_index: usize) {
    if let Some(Some(ProjectedNode::Branch { children, .. })) = nodes.get(root_index) {
        for child in children.clone() {
            tombstone_subtree(nodes, child);
        }
    }
    if root_index < nodes.len() {
        nodes[root_index] = None;
    }
}

fn replace_with_plan_subtree<T>(
    nodes: &mut Vec<Option<ProjectedNode<T>>>,
    root_index: usize,
    subtree: PlanTree<T>,
) {
    match subtree {
        Tree::Leaf { meta: _, node } => {
            if root_index >= nodes.len() {
                nodes.resize_with(root_index + 1, || None);
            }
            nodes[root_index] = Some(ProjectedNode::Leaf {
                lifecycle: Lifecycle::Complete(node),
            });
        }
        Tree::Branch { meta, children } => {
            let mut child_indices = Vec::with_capacity(children.len());
            for child in children {
                let child_index = append_plan_subtree(nodes, child);
                child_indices.push(child_index);
            }
            if root_index >= nodes.len() {
                nodes.resize_with(root_index + 1, || None);
            }
            nodes[root_index] = Some(ProjectedNode::Branch {
                meta,
                children: child_indices,
            });
        }
    }
}

fn append_plan_subtree<T>(
    nodes: &mut Vec<Option<ProjectedNode<T>>>,
    subtree: PlanTree<T>,
) -> usize {
    match subtree {
        Tree::Leaf { meta: _, node } => {
            let index = nodes.len();
            nodes.push(Some(ProjectedNode::Leaf {
                lifecycle: Lifecycle::Complete(node),
            }));
            index
        }
        Tree::Branch { meta, children } => {
            let index = nodes.len();
            nodes.push(Some(ProjectedNode::Branch {
                meta,
                children: Vec::new(),
            }));
            let mut child_indices = Vec::with_capacity(children.len());
            for child in children {
                let child_index = append_plan_subtree(nodes, child);
                child_indices.push(child_index);
            }
            if let Some(ProjectedNode::Branch { children, .. }) = nodes[index].as_mut() {
                *children = child_indices;
            }
            index
        }
    }
}

/// Build a fully-Complete [`ProjectedTree<T>`] from a [`PlanTree<T>`]. Used
/// for inputs that arrive whole (resource_params).
fn project_plan_tree<T>(tree: PlanTree<T>) -> ProjectedTree<T> {
    let mut nodes = Vec::new();
    append_plan_subtree(&mut nodes, tree);
    ProjectedTree { nodes }
}

/// Coarse pipeline progress derived from the `AppView`'s data. One traversal
/// of the leaf set produces the variant; downstream consumers (follow-mode
/// stage selection, feedback-line text) read this instead of walking again.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PipelineProgress {
    AwaitingParams,
    AwaitingResources,
    AwaitingStates,
    Probing,
    SomeResolved,
    SomeOpsExpanded,
    Applying,
    Done,
}

/// TUI state. Per-leaf lifecycle lives in `resources`; the four per-stage
/// projections (resources / resource_states / resource_changes / operations_tree)
/// are produced from it on demand. `operations_epochs` is the live apply
/// pane, untouched by the projection.
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct AppView {
    pub resource_params: Option<ProjectedTree<ResourceParams>>,
    pub resources: Option<ResourcesTree>,
    pub operations_epochs: Vec<Vec<OperationView>>,
    /// Whether any leaf has reached `Changed`. Also set by `ApplyComplete`'s
    /// payload (the producer's authoritative bit) so a dropped per-leaf event
    /// doesn't leave this false. Once true, stays true.
    pub had_changes: bool,
    /// True after `ApplyComplete`.
    pub done: bool,
    /// Monotonic counter stamped on each successful `OperationsNode` event
    /// so the operations projection can splice in arrival order.
    ops_seq_counter: u64,
}

#[derive(Debug, Error)]
pub enum AppViewError {
    #[error("operation index out of bounds: epoch={0}, op={1}")]
    OperationIndexOutOfBounds(usize, usize),

    /// `OperationsApplyEpochAdded` is the only event that grows
    /// `operations_epochs`; the producer must emit them strictly in order
    /// (epoch 0, then 1, then 2, ...). Anything else means the protocol
    /// stream is corrupt or the producer has a bug.
    #[error("operations-apply epoch index {got} arrived but expected {expected}")]
    NonMonotonicEpochIndex { got: usize, expected: usize },

    #[error("event {event} targeted leaf {index}, but no resources tree has been received yet")]
    NoResourcesTree { event: &'static str, index: usize },

    #[error("event {event} targeted index {index}, but no such node exists")]
    NodeNotFound { event: &'static str, index: usize },

    #[error("event {event} targeted leaf {index}, but it is a branch")]
    NotALeaf { event: &'static str, index: usize },

    #[error("event {event} targeted leaf {index} in state {state}; transition not allowed")]
    InvalidLeafTransition {
        event: &'static str,
        index: usize,
        state: &'static str,
    },
}

impl AppView {
    /// Fold one [`AppUpdate`] into the view. See [`LeafState`] for the
    /// per-leaf transition diagram.
    pub fn update(mut self, update: AppUpdate) -> Result<Self, AppViewError> {
        use AppUpdate::*;
        match update {
            ResourceParams { resource_params } => {
                self.resource_params = Some(project_plan_tree(resource_params));
            }

            // The full atoms tree arrives as one `ResourcesNode { index: 0 }`
            // event. Start/Complete bracket it for the consumer.
            ResourcesStart => {}
            ResourcesNode { index: _, tree } => {
                self.resources = Some(build_resources_tree(tree));
            }
            ResourcesComplete => {}

            ResourceStatesNodeStart { index } => {
                self.transition_leaf("ResourceStatesNodeStart", index, |state| match state {
                    LeafState::Planned { resource } => Ok(LeafState::Probing {
                        resource: resource.clone(),
                    }),
                    other => Err(other.name()),
                })?;
            }
            ResourceStatesNodeComplete { index, state } => {
                self.transition_leaf("ResourceStatesNodeComplete", index, |prev| match prev {
                    LeafState::Probing { resource } => Ok(LeafState::Probed {
                        resource: resource.clone(),
                        state: state.clone(),
                    }),
                    other => Err(other.name()),
                })?;
            }
            ResourceChangesNode { index, change } => {
                let is_change = change.is_some();
                self.transition_leaf("ResourceChangesNode", index, |prev| match prev {
                    LeafState::Probed {
                        resource,
                        state: probed_state,
                    } => match &change {
                        None => Ok(LeafState::NoChange {
                            resource: resource.clone(),
                            state: probed_state.clone(),
                        }),
                        Some(change) => Ok(LeafState::Changed {
                            resource: resource.clone(),
                            state: probed_state.clone(),
                            change: change.clone(),
                            ops: None,
                        }),
                    },
                    other => Err(other.name()),
                })?;
                if is_change {
                    self.had_changes = true;
                }
            }
            OperationsNode { index, operations } => {
                let seq = self.ops_seq_counter;
                self.transition_leaf("OperationsNode", index, |prev| match prev {
                    LeafState::Changed {
                        resource,
                        state: probed_state,
                        change,
                        ops: None,
                    } => Ok(LeafState::Changed {
                        resource: resource.clone(),
                        state: probed_state.clone(),
                        change: change.clone(),
                        ops: Some((operations.clone(), seq)),
                    }),
                    other => Err(other.name()),
                })?;
                // Bump only after a successful transition so rejected events
                // don't leave gaps in the splice ordering.
                self.ops_seq_counter += 1;
            }

            OperationsApplyEpochAdded {
                epoch_index,
                operations,
            } => {
                if self.operations_epochs.len() != epoch_index {
                    return Err(AppViewError::NonMonotonicEpochIndex {
                        got: epoch_index,
                        expected: self.operations_epochs.len(),
                    });
                }
                self.operations_epochs
                    .push(operations.into_iter().map(OperationView::new).collect());
            }

            OperationApplyStart { index: (e, o) } => {
                let op = self.op_mut(e, o)?;
                op.stdout.clear();
                op.stderr.clear();
                op.is_complete = false;
                op.error = None;
            }
            OperationApplyStdout {
                index: (e, o),
                stdout,
            } => {
                let op = self.op_mut(e, o)?;
                op.stdout.push_str(&stdout);
                op.stdout.push('\n');
            }
            OperationApplyStderr {
                index: (e, o),
                stderr,
            } => {
                let op = self.op_mut(e, o)?;
                op.stderr.push_str(&stderr);
                op.stderr.push('\n');
            }
            OperationApplyComplete {
                index: (e, o),
                error,
            } => {
                let op = self.op_mut(e, o)?;
                op.is_complete = true;
                op.error = error;
            }

            ApplyComplete { had_changes } => {
                self.had_changes = self.had_changes || had_changes;
                self.done = true;
            }
        }
        Ok(self)
    }

    /// Apply a per-leaf transition. The closure receives the current state
    /// and returns either the next state or the rejection's state name.
    fn transition_leaf<F>(
        &mut self,
        event: &'static str,
        index: usize,
        f: F,
    ) -> Result<(), AppViewError>
    where
        F: FnOnce(&LeafState) -> Result<LeafState, &'static str>,
    {
        let resources = self
            .resources
            .as_mut()
            .ok_or(AppViewError::NoResourcesTree { event, index })?;
        let slot = resources
            .nodes
            .get_mut(index)
            .and_then(|s| s.as_mut())
            .ok_or(AppViewError::NodeNotFound { event, index })?;
        let state = match slot {
            ResourcesNode::Leaf { state } => state,
            ResourcesNode::Branch { .. } => {
                return Err(AppViewError::NotALeaf { event, index });
            }
        };
        let next = f(state).map_err(|name| AppViewError::InvalidLeafTransition {
            event,
            index,
            state: name,
        })?;
        *state = next;
        Ok(())
    }

    fn op_mut(&mut self, e: usize, o: usize) -> Result<&mut OperationView, AppViewError> {
        let epoch = self
            .operations_epochs
            .get_mut(e)
            .ok_or(AppViewError::OperationIndexOutOfBounds(e, o))?;
        epoch
            .get_mut(o)
            .ok_or(AppViewError::OperationIndexOutOfBounds(e, o))
    }

    pub fn resource_params(&self) -> Option<&ProjectedTree<ResourceParams>> {
        self.resource_params.as_ref()
    }

    pub fn operations_epochs(&self) -> Option<&Vec<Vec<OperationView>>> {
        if self.operations_epochs.is_empty() {
            None
        } else {
            Some(&self.operations_epochs)
        }
    }

    /// Classify the pipeline's coarse stage from the leaf set in a single
    /// walk. Used by follow-mode advancement and the feedback line.
    pub fn progress(&self) -> PipelineProgress {
        if self.done {
            return PipelineProgress::Done;
        }
        if !self.operations_epochs.is_empty() {
            return PipelineProgress::Applying;
        }
        let Some(tree) = self.resources.as_ref() else {
            return if self.resource_params.is_some() {
                PipelineProgress::AwaitingResources
            } else {
                PipelineProgress::AwaitingParams
            };
        };
        let mut has_ops = false;
        let mut has_resolved = false;
        let mut has_probing = false;
        for state in tree.leaves() {
            match state {
                LeafState::Changed { ops: Some(_), .. } => has_ops = true,
                LeafState::NoChange { .. } | LeafState::Changed { ops: None, .. } => {
                    has_resolved = true;
                }
                LeafState::Probing { .. } | LeafState::Probed { .. } => has_probing = true,
                LeafState::Planned { .. } => {}
            }
        }
        if has_ops {
            PipelineProgress::SomeOpsExpanded
        } else if has_resolved {
            PipelineProgress::SomeResolved
        } else if has_probing {
            PipelineProgress::Probing
        } else {
            PipelineProgress::AwaitingStates
        }
    }

    /// Project the atoms tree so every leaf is the atom's [`Resource`].
    /// Always-Complete: the whole tree arrives at once.
    pub fn resources_view(&self) -> Option<ProjectedTree<Resource>> {
        let tree = self.resources.as_ref()?;
        Some(project(tree, |state| {
            Some(Lifecycle::Complete(state.resource().clone()))
        }))
    }

    /// Project per-leaf probe progress: `NotStarted` -> `Started` -> the
    /// probed state value.
    pub fn resource_states_view(&self) -> Option<ProjectedTree<ResourceState>> {
        let tree = self.resources.as_ref()?;
        Some(project(tree, |state| match state {
            LeafState::Planned { .. } => Some(Lifecycle::NotStarted),
            LeafState::Probing { .. } => Some(Lifecycle::Started),
            LeafState::Probed { state, .. }
            | LeafState::NoChange { state, .. }
            | LeafState::Changed { state, .. } => Some(Lifecycle::Complete(state.clone())),
        }))
    }

    /// Project per-leaf computed changes. `NoChange` leaves are pruned;
    /// pre-resolution leaves render `NotStarted`.
    pub fn resource_changes_view(&self) -> Option<ProjectedTree<ResourceChange>> {
        let tree = self.resources.as_ref()?;
        Some(project(tree, |state| match state {
            LeafState::Planned { .. } | LeafState::Probing { .. } | LeafState::Probed { .. } => {
                Some(Lifecycle::NotStarted)
            }
            LeafState::NoChange { .. } => None,
            LeafState::Changed { change, .. } => Some(Lifecycle::Complete(change.clone())),
        }))
    }

    /// Project per-leaf operations subtrees. `NoChange` leaves are pruned;
    /// leaves with `Changed { ops: Some(_) }` have their op subtrees spliced
    /// in. Splicing happens in `ops_seq` (arrival) order so the appended
    /// arena region stays stable across renders even when ops events arrive
    /// out of arena-index order.
    pub fn operations_tree_view(&self) -> Option<ProjectedTree<Operation>> {
        let resources = self.resources.as_ref()?;

        // Pass 1: build the base arena. Splice-target slots get a placeholder
        // `NotStarted` leaf that pass 2 overwrites; `NoChange` slots are
        // pruned outright.
        let mut tree: ProjectedTree<Operation> = project(resources, |state| match state {
            LeafState::NoChange { .. } => None,
            _ => Some(Lifecycle::NotStarted),
        });

        // Pass 2: collect splice targets, sort by arrival order so appended
        // arena indices stay stable across renders, then splice.
        let mut splice_targets: Vec<(usize, &PlanTree<Operation>, u64)> = resources
            .nodes
            .iter()
            .enumerate()
            .filter_map(|(idx, slot)| match slot {
                Some(ResourcesNode::Leaf {
                    state:
                        LeafState::Changed {
                            ops: Some((subtree, seq)),
                            ..
                        },
                }) => Some((idx, subtree, *seq)),
                _ => None,
            })
            .collect();
        splice_targets.sort_by_key(|(_, _, seq)| *seq);
        for (idx, subtree, _) in splice_targets {
            tree.splice_plan_subtree(idx, subtree.clone());
        }
        Some(tree)
    }
}

/// Walk `resources` once, building a parallel [`ProjectedTree`] arena.
/// Branches always pass through their meta and child indices; leaves are
/// mapped via `leaf_to_lifecycle` (returning `None` prunes the slot).
fn project<T, F>(resources: &ResourcesTree, leaf_to_lifecycle: F) -> ProjectedTree<T>
where
    F: Fn(&LeafState) -> Option<Lifecycle<T>>,
{
    let nodes = resources
        .nodes
        .iter()
        .map(|slot| match slot {
            None => None,
            Some(ResourcesNode::Branch { meta, children }) => Some(ProjectedNode::Branch {
                meta: meta.clone(),
                children: children.clone(),
            }),
            Some(ResourcesNode::Leaf { state }) => {
                leaf_to_lifecycle(state).map(|lifecycle| ProjectedNode::Leaf { lifecycle })
            }
        })
        .collect();
    ProjectedTree { nodes }
}

/// Build a `ResourcesTree` from the producer's nested `PlanTree<Resource>`.
/// Branches carry their `PlanMeta` through; every leaf starts in
/// `LeafState::Planned`.
fn build_resources_tree(tree: PlanTree<Resource>) -> ResourcesTree {
    let mut nodes = Vec::new();
    append_resources_tree_nodes(&mut nodes, tree);
    ResourcesTree { nodes }
}

fn append_resources_tree_nodes(
    nodes: &mut Vec<Option<ResourcesNode>>,
    tree: PlanTree<Resource>,
) -> usize {
    match tree {
        Tree::Leaf { meta: _, node } => {
            let index = nodes.len();
            nodes.push(Some(ResourcesNode::Leaf {
                state: LeafState::Planned { resource: node },
            }));
            index
        }
        Tree::Branch { meta, children } => {
            let index = nodes.len();
            nodes.push(Some(ResourcesNode::Branch {
                meta,
                children: Vec::new(),
            }));
            let child_indices: Vec<usize> = children
                .into_iter()
                .map(|c| append_resources_tree_nodes(nodes, c))
                .collect();
            if let Some(ResourcesNode::Branch { children, .. }) = nodes[index].as_mut() {
                *children = child_indices;
            }
            index
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use lusid_operation::Operation;
    use lusid_operation::operations::command::{CommandExecutor, CommandOperation};
    use lusid_operation::operations::file::FilePath;
    use lusid_plan::{PlanId, PlanMeta, PlanNodeId, PlanTree};
    use lusid_resource::{
        Resource, ResourceChange, ResourceParams, ResourceState,
        apt::{AptChange, AptParams, AptState},
        file::{FileResource, FileState},
    };
    use std::path::PathBuf;

    fn resource_leaf(path: &str) -> PlanTree<Resource> {
        PlanTree::Leaf {
            meta: PlanMeta::default(),
            node: Resource::File(FileResource::Present {
                path: FilePath::new(path),
            }),
        }
    }

    fn file_state() -> ResourceState {
        ResourceState::File(FileState::Absent)
    }

    fn apt_change() -> ResourceChange {
        ResourceChange::Apt(AptChange::Install {
            package: "nginx".into(),
        })
    }

    fn command_op(label: &str) -> Operation {
        Operation::Command(CommandOperation {
            command: label.to_string(),
            executor: CommandExecutor::Shell,
        })
    }

    fn op_leaf(label: &str) -> PlanTree<Operation> {
        PlanTree::Leaf {
            meta: PlanMeta::default(),
            node: command_op(label),
        }
    }

    fn op_branch(label: &str) -> PlanTree<Operation> {
        PlanTree::Branch {
            meta: PlanMeta {
                id: Some(PlanNodeId::PlanItem {
                    plan_id: PlanId::Path(PathBuf::from("test.lusid")),
                    item_id: label.into(),
                }),
                ..PlanMeta::default()
            },
            children: vec![
                op_leaf(&format!("{label}-1")),
                op_leaf(&format!("{label}-2")),
            ],
        }
    }

    /// Build an `AppView` whose atoms tree has two sibling resource leaves under
    /// one branch. Used as the common starting shape for transition tests.
    fn app_view_with_two_leaves() -> AppView {
        let tree = PlanTree::Branch {
            meta: PlanMeta::default(),
            children: vec![resource_leaf("/a"), resource_leaf("/b")],
        };
        AppView::default()
            .update(AppUpdate::ResourcesStart)
            .unwrap()
            .update(AppUpdate::ResourcesNode { index: 0, tree })
            .unwrap()
    }

    fn leaf_state(view: &AppView, idx: usize) -> &LeafState {
        match view
            .resources
            .as_ref()
            .unwrap()
            .nodes
            .get(idx)
            .and_then(Option::as_ref)
        {
            Some(ResourcesNode::Leaf { state }) => state,
            other => panic!("expected leaf at {idx}, got {other:?}"),
        }
    }

    fn leaf_lifecycle<'a, T>(tree: &'a ProjectedTree<T>, idx: usize) -> &'a Lifecycle<T> {
        match tree.nodes[idx].as_ref().unwrap() {
            ProjectedNode::Leaf { lifecycle } => lifecycle,
            _ => panic!("expected leaf at {idx}"),
        }
    }

    #[test]
    fn full_lifecycle_advances_each_state() {
        let v = app_view_with_two_leaves();
        let v = v
            .update(AppUpdate::ResourceStatesNodeStart { index: 1 })
            .unwrap();
        assert!(matches!(leaf_state(&v, 1), LeafState::Probing { .. }));

        let v = v
            .update(AppUpdate::ResourceStatesNodeComplete {
                index: 1,
                state: file_state(),
            })
            .unwrap();
        assert!(matches!(leaf_state(&v, 1), LeafState::Probed { .. }));

        let v = v
            .update(AppUpdate::ResourceChangesNode {
                index: 1,
                change: Some(apt_change()),
            })
            .unwrap();
        assert!(matches!(
            leaf_state(&v, 1),
            LeafState::Changed { ops: None, .. }
        ));
        assert!(v.had_changes);

        let v = v
            .update(AppUpdate::OperationsNode {
                index: 1,
                operations: op_leaf("op-a"),
            })
            .unwrap();
        assert!(matches!(
            leaf_state(&v, 1),
            LeafState::Changed { ops: Some(_), .. }
        ));
    }

    #[test]
    fn no_change_path_terminates_at_no_change() {
        let v = app_view_with_two_leaves()
            .update(AppUpdate::ResourceStatesNodeStart { index: 1 })
            .unwrap()
            .update(AppUpdate::ResourceStatesNodeComplete {
                index: 1,
                state: file_state(),
            })
            .unwrap()
            .update(AppUpdate::ResourceChangesNode {
                index: 1,
                change: None,
            })
            .unwrap();
        assert!(matches!(leaf_state(&v, 1), LeafState::NoChange { .. }));
        assert!(!v.had_changes);
    }

    #[test]
    fn changes_before_probe_is_rejected() {
        let err = app_view_with_two_leaves()
            .update(AppUpdate::ResourceChangesNode {
                index: 1,
                change: None,
            })
            .unwrap_err();
        assert!(
            matches!(
                err,
                AppViewError::InvalidLeafTransition {
                    state: "Planned",
                    ..
                }
            ),
            "got {err:?}"
        );
    }

    #[test]
    fn ops_before_change_is_rejected() {
        let err = app_view_with_two_leaves()
            .update(AppUpdate::ResourceStatesNodeStart { index: 1 })
            .unwrap()
            .update(AppUpdate::ResourceStatesNodeComplete {
                index: 1,
                state: file_state(),
            })
            .unwrap()
            .update(AppUpdate::OperationsNode {
                index: 1,
                operations: op_leaf("op"),
            })
            .unwrap_err();
        assert!(
            matches!(
                err,
                AppViewError::InvalidLeafTransition {
                    state: "Probed",
                    ..
                }
            ),
            "got {err:?}"
        );
    }

    #[test]
    fn ops_re_emitted_is_rejected() {
        let v = app_view_with_two_leaves()
            .update(AppUpdate::ResourceStatesNodeStart { index: 1 })
            .unwrap()
            .update(AppUpdate::ResourceStatesNodeComplete {
                index: 1,
                state: file_state(),
            })
            .unwrap()
            .update(AppUpdate::ResourceChangesNode {
                index: 1,
                change: Some(apt_change()),
            })
            .unwrap()
            .update(AppUpdate::OperationsNode {
                index: 1,
                operations: op_leaf("op"),
            })
            .unwrap();
        let err = v
            .update(AppUpdate::OperationsNode {
                index: 1,
                operations: op_leaf("op2"),
            })
            .unwrap_err();
        assert!(matches!(
            err,
            AppViewError::InvalidLeafTransition {
                state: "Changed { ops: Some }",
                ..
            }
        ));
    }

    #[test]
    fn leaf_event_on_branch_index_is_rejected() {
        let err = app_view_with_two_leaves()
            .update(AppUpdate::ResourceStatesNodeStart { index: 0 })
            .unwrap_err();
        assert!(matches!(err, AppViewError::NotALeaf { index: 0, .. }));
    }

    #[test]
    fn leaf_event_on_unknown_index_is_rejected() {
        let err = app_view_with_two_leaves()
            .update(AppUpdate::ResourceStatesNodeStart { index: 99 })
            .unwrap_err();
        assert!(matches!(err, AppViewError::NodeNotFound { index: 99, .. }));
    }

    #[test]
    fn leaf_event_before_resources_tree_is_rejected() {
        let err = AppView::default()
            .update(AppUpdate::ResourceStatesNodeStart { index: 1 })
            .unwrap_err();
        assert!(matches!(err, AppViewError::NoResourcesTree { .. }));
    }

    #[test]
    fn no_change_leaf_is_pruned_in_changes_and_ops_projections() {
        let v = app_view_with_two_leaves()
            .update(AppUpdate::ResourceStatesNodeStart { index: 1 })
            .unwrap()
            .update(AppUpdate::ResourceStatesNodeComplete {
                index: 1,
                state: file_state(),
            })
            .unwrap()
            .update(AppUpdate::ResourceChangesNode {
                index: 1,
                change: None,
            })
            .unwrap();
        let changes = v.resource_changes_view().unwrap();
        assert!(changes.nodes[1].is_none(), "no-change leaf pruned");
        let ops = v.operations_tree_view().unwrap();
        assert!(ops.nodes[1].is_none(), "no-change leaf pruned");
    }

    #[test]
    fn changes_while_probing_is_rejected() {
        let err = app_view_with_two_leaves()
            .update(AppUpdate::ResourceStatesNodeStart { index: 1 })
            .unwrap()
            .update(AppUpdate::ResourceChangesNode {
                index: 1,
                change: None,
            })
            .unwrap_err();
        assert!(
            matches!(
                err,
                AppViewError::InvalidLeafTransition {
                    state: "Probing",
                    ..
                }
            ),
            "got {err:?}"
        );
    }

    #[test]
    fn ops_after_no_change_is_rejected() {
        let err = app_view_with_two_leaves()
            .update(AppUpdate::ResourceStatesNodeStart { index: 1 })
            .unwrap()
            .update(AppUpdate::ResourceStatesNodeComplete {
                index: 1,
                state: file_state(),
            })
            .unwrap()
            .update(AppUpdate::ResourceChangesNode {
                index: 1,
                change: None,
            })
            .unwrap()
            .update(AppUpdate::OperationsNode {
                index: 1,
                operations: op_leaf("op"),
            })
            .unwrap_err();
        assert!(
            matches!(
                err,
                AppViewError::InvalidLeafTransition {
                    state: "NoChange",
                    ..
                }
            ),
            "got {err:?}"
        );
    }

    /// One leaf walks the whole lifecycle while we check every projection
    /// produces the right cell at each step.
    #[test]
    fn projection_table_matches_design() {
        let v = app_view_with_two_leaves();
        // Planned: resources=Complete, states/changes/ops=NotStarted.
        assert!(matches!(
            leaf_lifecycle(&v.resources_view().unwrap(), 1),
            Lifecycle::Complete(_)
        ));
        assert!(matches!(
            leaf_lifecycle(&v.resource_states_view().unwrap(), 1),
            Lifecycle::NotStarted
        ));
        assert!(matches!(
            leaf_lifecycle(&v.resource_changes_view().unwrap(), 1),
            Lifecycle::NotStarted
        ));
        assert!(matches!(
            leaf_lifecycle(&v.operations_tree_view().unwrap(), 1),
            Lifecycle::NotStarted
        ));

        let v = v
            .update(AppUpdate::ResourceStatesNodeStart { index: 1 })
            .unwrap();
        // Probing: states=Started, others same.
        assert!(matches!(
            leaf_lifecycle(&v.resource_states_view().unwrap(), 1),
            Lifecycle::Started
        ));

        let v = v
            .update(AppUpdate::ResourceStatesNodeComplete {
                index: 1,
                state: file_state(),
            })
            .unwrap();
        // Probed: states=Complete, changes/ops still NotStarted.
        assert!(matches!(
            leaf_lifecycle(&v.resource_states_view().unwrap(), 1),
            Lifecycle::Complete(_)
        ));
        assert!(matches!(
            leaf_lifecycle(&v.resource_changes_view().unwrap(), 1),
            Lifecycle::NotStarted
        ));
        assert!(matches!(
            leaf_lifecycle(&v.operations_tree_view().unwrap(), 1),
            Lifecycle::NotStarted
        ));

        let v = v
            .update(AppUpdate::ResourceChangesNode {
                index: 1,
                change: Some(apt_change()),
            })
            .unwrap();
        // Changed { ops: None }: changes=Complete, ops still NotStarted.
        assert!(matches!(
            leaf_lifecycle(&v.resource_changes_view().unwrap(), 1),
            Lifecycle::Complete(_)
        ));
        assert!(matches!(
            leaf_lifecycle(&v.operations_tree_view().unwrap(), 1),
            Lifecycle::NotStarted
        ));

        let v = v
            .update(AppUpdate::OperationsNode {
                index: 1,
                operations: op_leaf("op"),
            })
            .unwrap();
        // Changed { ops: Some }: ops slot is the spliced subtree's root.
        // The op subtree is a Leaf, so the slot becomes a Leaf with the
        // Complete(op) variant per splice_plan_subtree.
        assert!(matches!(
            leaf_lifecycle(&v.operations_tree_view().unwrap(), 1),
            Lifecycle::Complete(_)
        ));
    }

    /// Two leaves transition to `Changed { ops: Some }` with branch ops
    /// payloads (so splicing appends children to the arena tail). The second
    /// arrival is for the lower-arena-index leaf; the first arrival's child
    /// indices must stay stable because splice happens in `ops_seq` (arrival)
    /// order, not arena-index order.
    #[test]
    fn ops_splice_indices_are_stable_across_arrivals() {
        let advance_to_changed = |v: AppView, idx: usize| -> AppView {
            v.update(AppUpdate::ResourceStatesNodeStart { index: idx })
                .unwrap()
                .update(AppUpdate::ResourceStatesNodeComplete {
                    index: idx,
                    state: file_state(),
                })
                .unwrap()
                .update(AppUpdate::ResourceChangesNode {
                    index: idx,
                    change: Some(apt_change()),
                })
                .unwrap()
        };

        // Leaf at arena index 2 gets ops first (so leaf 2's children are
        // appended to the tail).
        let v = app_view_with_two_leaves();
        let v = advance_to_changed(v, 1);
        let v = advance_to_changed(v, 2);
        let v = v
            .update(AppUpdate::OperationsNode {
                index: 2,
                operations: op_branch("b"),
            })
            .unwrap();
        let before = v.operations_tree_view().unwrap();
        let b_children_before: Vec<usize> = match before.nodes[2].as_ref().unwrap() {
            ProjectedNode::Branch { children, .. } => children.clone(),
            _ => panic!("leaf 2 should now be a spliced branch"),
        };

        // Then leaf 1's ops arrive. Splice in arrival order means leaf 1's
        // children land after leaf 2's, leaving leaf 2's child indices alone.
        let v = v
            .update(AppUpdate::OperationsNode {
                index: 1,
                operations: op_branch("a"),
            })
            .unwrap();
        let after = v.operations_tree_view().unwrap();

        let b_children_after: Vec<usize> = match after.nodes[2].as_ref().unwrap() {
            ProjectedNode::Branch { children, .. } => children.clone(),
            _ => panic!("leaf 2 should still be a spliced branch"),
        };
        assert_eq!(
            b_children_before, b_children_after,
            "leaf 2's child indices stay stable across the second splice",
        );
        let a_children_after: Vec<usize> = match after.nodes[1].as_ref().unwrap() {
            ProjectedNode::Branch { children, .. } => children.clone(),
            _ => panic!("leaf 1 should now be a spliced branch"),
        };
        assert!(
            a_children_after
                .iter()
                .all(|i| !b_children_after.contains(i)),
            "a's children {a_children_after:?} should not collide with b's {b_children_after:?}",
        );
    }

    #[test]
    fn progress_reports_each_stage() {
        let params = PlanTree::Leaf {
            meta: PlanMeta::default(),
            node: ResourceParams::Apt(AptParams::Package {
                package: "p".into(),
            }),
        };
        let v = AppView::default();
        assert_eq!(v.progress(), PipelineProgress::AwaitingParams);

        let v = v
            .update(AppUpdate::ResourceParams {
                resource_params: params,
            })
            .unwrap();
        assert_eq!(v.progress(), PipelineProgress::AwaitingResources);

        let tree = PlanTree::Branch {
            meta: PlanMeta::default(),
            children: vec![resource_leaf("/a")],
        };
        let v = v
            .update(AppUpdate::ResourcesStart)
            .unwrap()
            .update(AppUpdate::ResourcesNode { index: 0, tree })
            .unwrap();
        assert_eq!(v.progress(), PipelineProgress::AwaitingStates);

        let v = v
            .update(AppUpdate::ResourceStatesNodeStart { index: 1 })
            .unwrap();
        assert_eq!(v.progress(), PipelineProgress::Probing);

        let v = v
            .update(AppUpdate::ResourceStatesNodeComplete {
                index: 1,
                state: ResourceState::Apt(AptState::NotInstalled),
            })
            .unwrap();
        assert_eq!(v.progress(), PipelineProgress::Probing);

        let v = v
            .update(AppUpdate::ResourceChangesNode {
                index: 1,
                change: Some(apt_change()),
            })
            .unwrap();
        assert_eq!(v.progress(), PipelineProgress::SomeResolved);

        let v = v
            .update(AppUpdate::OperationsNode {
                index: 1,
                operations: op_leaf("op"),
            })
            .unwrap();
        assert_eq!(v.progress(), PipelineProgress::SomeOpsExpanded);

        let v = v
            .update(AppUpdate::OperationsApplyEpochAdded {
                epoch_index: 0,
                operations: vec![command_op("op")],
            })
            .unwrap();
        assert_eq!(v.progress(), PipelineProgress::Applying);

        let v = v
            .update(AppUpdate::ApplyComplete { had_changes: true })
            .unwrap();
        assert_eq!(v.progress(), PipelineProgress::Done);
    }

    #[test]
    fn apply_complete_carries_had_changes_when_no_per_leaf_event_set_it() {
        let v = AppView::default()
            .update(AppUpdate::ApplyComplete { had_changes: true })
            .unwrap();
        assert!(v.had_changes);
        assert!(v.done);
    }
}
