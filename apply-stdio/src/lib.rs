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
//!    for `on_change` handlers (those fire in the `OnChange` phase and appear
//!    only in the apply pane).
//! 3. Per resource epoch (in causality order):
//!    - state probe events for atoms in this epoch,
//!    - change events,
//!    - operations sub-tree events,
//!    - `Change` phase: per-internal-operation-epoch
//!      [`AppUpdate::OperationsApplyEpochAdded`] with merged change ops,
//!      then per-op apply events,
//!    - `OnChange` phase: handler ops for any plan item whose latest atom was
//!      in this epoch and which had at least one atom change. Same lifecycle
//!      events as the `Change` phase, advancing the same `op_epoch_index`
//!      counter.
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

use std::collections::HashMap;

use base64::Engine as _;
use base64::engine::general_purpose::STANDARD_NO_PAD;
use lusid_operation::Operation;
use lusid_plan::{PlanMeta, PlanTree};
pub use lusid_resource::ChangeKind;
use lusid_resource::{Resource, ResourceChange, ResourceParams, ResourceState};
use lusid_tree::Tree;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use thiserror::Error;

/// Base64 codec for raw operation-output bytes on the JSON wire.
///
/// Stdout/stderr from arbitrary programs is not guaranteed UTF-8 (ANSI
/// escapes survive, but a `\xff` from an unlucky encoding does not), so the
/// payload travels as bytes. Base64 over the NDJSON line keeps the wire
/// printable and small (~1.33x) compared to a JSON array of integers (~4x).
/// `NO_PAD` shaves the trailing `=`s; the decoder accepts both forms.
mod bytes_base64 {
    use super::*;

    pub fn serialize<S: Serializer>(bytes: &[u8], serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&STANDARD_NO_PAD.encode(bytes))
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Vec<u8>, D::Error> {
        let s = String::deserialize(deserializer)?;
        STANDARD_NO_PAD
            .decode(s.as_bytes())
            .or_else(|_| base64::engine::general_purpose::STANDARD.decode(s.as_bytes()))
            .map_err(serde::de::Error::custom)
    }
}

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

    /// Once-per-apply summary emitted after `ResourcesComplete` and before any
    /// per-epoch event (including under `--parse-only`): the total number of
    /// resource epochs, and a mapping from each leaf (atom) arena index in the
    /// shipped [`ResourcesTree`] to the resource epoch it runs in. Branch
    /// arena slots are not keys; consumers should fall back to walking the
    /// tree for branch-level epoch annotations. Consumers use it to size the
    /// epoch header strip and to render per-atom epoch tags without re-running
    /// `compute_epochs`.
    PipelineInfo {
        resource_epochs_total: usize,
        atom_epoch: HashMap<usize, usize>,
    },

    /// Begin filling in the atoms tree - the resource atoms produced by
    /// expanding each plan item's `ResourceParams`. One leaf per atom; no
    /// synthetic nodes for `on_change` handlers (those fire in the `OnChange`
    /// phase and appear only in the apply pane).
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
    ///
    /// `resource_epoch` identifies which outer resource epoch this internal
    /// op epoch belongs to; `phase` distinguishes `Change` (atom change ops)
    /// from `OnChange` (`on_change` handlers). Multiple
    /// `OperationsApplyEpochAdded` events can share a `resource_epoch` value.
    /// `epoch_index` is strictly contiguous from 0 across the apply
    /// (incremented per emitted event; empty resource epochs / empty phases
    /// produce no event and so do not consume an index).
    OperationsApplyEpochAdded {
        epoch_index: usize,
        resource_epoch: usize,
        phase: Phase,
        operations: Vec<Operation>,
    },

    /// Per-operation lifecycle during apply. `index = (op_epoch_index, op_index)`,
    /// where `op_epoch_index` is the running counter across all
    /// `OperationsApplyEpochAdded` events.
    OperationApplyStart {
        index: (usize, usize),
    },
    /// One chunk of stdout bytes from the operation, delimited by `\r` or
    /// `\n` (whichever comes first); the terminator is retained so the
    /// consumer can render bare-`\r` progress redraws in place. Carries
    /// raw bytes so ANSI escape sequences survive the wire and the
    /// consumer can render them via a terminal emulator.
    OperationApplyStdout {
        index: (usize, usize),
        #[serde(with = "bytes_base64")]
        stdout: Vec<u8>,
    },
    /// One chunk of stderr bytes from the operation. Same shape and
    /// delimiter semantics as `OperationApplyStdout`.
    OperationApplyStderr {
        index: (usize, usize),
        #[serde(with = "bytes_base64")]
        stderr: Vec<u8>,
    },
    OperationApplyComplete {
        index: (usize, usize),
        error: Option<String>,
    },

    /// One atom whose change-phase or on-change-phase op failed. Emitted
    /// once per affected atom, after `OperationApplyComplete { error: Some(..) }`
    /// and before the producer halts. Transition is
    /// `Changed { ops: Some } -> Failed`; any other prior state is rejected.
    ///
    /// Per-atom rather than per-op because op merging coalesces a family
    /// across atoms (one merged apt-install can cover N atoms' installs).
    ResourceApplyFailed {
        index: usize,
        error: String,
    },

    /// Emitted between the `Change` phase's probe/change-computation and the first op
    /// for a resource epoch, but only when the epoch has at least one atom
    /// change or one handler queued (empty epochs skip emission to avoid
    /// prompt fatigue). The producer then blocks reading one line of
    /// [`AckAction`] JSON from stdin before running any op in this epoch.
    /// With `--yes`, the producer skips both the emission and the read.
    EpochReady {
        resource_epoch: usize,
        summary: EpochSummary,
    },

    /// Final marker. Carries `had_changes` so the TUI can show the right
    /// "complete" message ("No changes" vs "Apply complete").
    ApplyComplete {
        had_changes: bool,
    },
}

/// Reverse-direction ack written one line per [`AppUpdate::EpochReady`] by the
/// consumer to the producer's stdin. The producer reads it as
/// `{"action": "apply"}` / `{"action": "abort"}`. EOF, parse error, or
/// `Abort` halts the apply at this epoch boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "action", rename_all = "lowercase")]
pub enum AckAction {
    Apply,
    Abort,
}

/// One row in [`EpochSummary::change_labels`]: a per-atom one-liner so the
/// consumer can list "what's about to apply" without re-rendering the full
/// `ResourceChange` tree itself. `atom_id` is a short human label (typically
/// the atom's resource Display).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChangeLabel {
    pub atom_id: String,
    pub kind: ChangeKind,
    pub summary: String,
}

/// Payload for [`AppUpdate::EpochReady`]: counts to size the prompt header
/// plus a (possibly truncated) list of per-atom one-liners. `truncated_count`
/// is the number of additional change labels not included in `change_labels`
/// (zero when the list is complete).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EpochSummary {
    pub atoms_total: usize,
    pub atoms_changed: usize,
    pub handlers_pending: usize,
    pub change_labels: Vec<ChangeLabel>,
    pub truncated_count: usize,
}

/// Which phase of a resource epoch an [`AppUpdate::OperationsApplyEpochAdded`]
/// belongs to. `Change` is the change ops produced by the epoch's atoms;
/// `OnChange` is the `on_change` handlers fired after `Change` completes for
/// any handler-bearing plan-item branch whose latest atom landed in this
/// epoch.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Phase {
    Change,
    OnChange,
}

/// Per-internal-op-epoch metadata stored on [`AppView`] in parallel to
/// [`AppView::operations_epochs`]. Indexed by `epoch_index` (the global,
/// monotonically increasing counter shipped on each
/// [`AppUpdate::OperationsApplyEpochAdded`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct OperationEpochMeta {
    pub resource_epoch: usize,
    pub phase: Phase,
}

/// One operation's live state during the apply phase. Stdout/stderr bytes
/// are not kept here: consumers that want a terminal-style view (the TUI)
/// stream the wire events directly into a `vt100::Parser`, and the plain
/// renderer prints each event as it arrives. Accumulating the full byte
/// transcript here would double-buffer for no current consumer.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OperationView {
    pub label: Operation,
    pub is_complete: bool,
    pub error: Option<String>,
}

impl OperationView {
    fn new(label: Operation) -> Self {
        Self {
            label,
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
///                            \-> Changed { ops: None } -> Changed { ops: Some } -> Failed   (terminal)
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
    /// One of the atom's change-phase or on-change-phase ops failed during
    /// apply. The atom's change/ops payloads are preserved so the per-stage
    /// projections still show what was planned; the live op error lives on
    /// the corresponding [`OperationView`] in `operations_epochs`.
    Failed {
        resource: Resource,
        state: ResourceState,
        change: ResourceChange,
        ops: (PlanTree<Operation>, u64),
        error: String,
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
            LeafState::Failed { .. } => "Failed",
        }
    }

    pub fn resource(&self) -> &Resource {
        match self {
            LeafState::Planned { resource }
            | LeafState::Probing { resource }
            | LeafState::Probed { resource, .. }
            | LeafState::NoChange { resource, .. }
            | LeafState::Changed { resource, .. }
            | LeafState::Failed { resource, .. } => resource,
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
    /// Per-internal-op-epoch metadata (resource_epoch, phase) parallel to
    /// `operations_epochs`. Same length as that vec after each
    /// `OperationsApplyEpochAdded` is folded.
    pub operation_epoch_meta: Vec<OperationEpochMeta>,
    /// Total resource-epoch count, set by `PipelineInfo`. `None` until that
    /// event arrives; consumers should display `?` until then.
    pub resource_epochs_total: Option<usize>,
    /// Leaf (atom) arena index -> resource epoch, set by `PipelineInfo`.
    /// Branch arena slots are not keys. Empty until that event arrives.
    pub atom_epoch: HashMap<usize, usize>,
    /// Whether any leaf has reached `Changed`. Also set by `ApplyComplete`'s
    /// payload (the producer's authoritative bit) so a dropped per-leaf event
    /// doesn't leave this false. Once true, stays true.
    pub had_changes: bool,
    /// True after `ApplyComplete`.
    pub done: bool,
    /// Set to `Some((resource_epoch, summary))` on each
    /// [`AppUpdate::EpochReady`] and cleared the next time the producer makes
    /// progress for that epoch (the next `OperationsApplyEpochAdded`, or
    /// `ApplyComplete`). Consumers consult this to show the confirm prompt.
    pub pending_epoch: Option<(usize, EpochSummary)>,
    /// Most recent atom (leaf arena index) touched by a probe/result event.
    /// Updated by transitions to `Probing`, `Probed`, and `Changed` so
    /// follow-mode can track which atom the producer is working on.
    /// `NoChange` transitions are skipped - those produce no operator-facing
    /// activity worth following to.
    pub last_activity_atom: Option<usize>,
    /// Most recent op (epoch_index, op_index) touched by an `OperationApply*`
    /// event. Follow-mode pins selection here on the Epochs page so the
    /// running op is always on screen.
    pub last_activity_op: Option<(usize, usize)>,
    /// Latch flipped to `true` on the first transition that enters `Probing`.
    /// The TUI checks pre/post around `update` to detect the one-shot edge and
    /// arms follow-mode exactly once. Stays `true` for the rest of the apply
    /// (we never re-arm, even if the operator turns follow back off).
    pub auto_follow_armed: bool,
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

            PipelineInfo {
                resource_epochs_total,
                atom_epoch,
            } => {
                self.resource_epochs_total = Some(resource_epochs_total);
                self.atom_epoch = atom_epoch;
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
                resource_epoch,
                phase,
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
                self.operation_epoch_meta.push(OperationEpochMeta {
                    resource_epoch,
                    phase,
                });
                debug_assert_eq!(
                    self.operations_epochs.len(),
                    self.operation_epoch_meta.len(),
                    "operations_epochs and operation_epoch_meta must stay parallel",
                );
                // The acknowledged epoch is now running; drop the prompt
                // state so the UI footer goes back to status-only.
                if let Some((pending_epoch, _)) = &self.pending_epoch
                    && *pending_epoch == resource_epoch
                {
                    self.pending_epoch = None;
                }
            }

            EpochReady {
                resource_epoch,
                summary,
            } => {
                // The producer emits at most one EpochReady per resource
                // epoch, and only after the prior epoch has acked. If a
                // second one lands while one is already pending, the
                // protocol is desynced - flag it loudly in debug builds.
                debug_assert!(
                    self.pending_epoch.is_none(),
                    "EpochReady for epoch {resource_epoch} arrived while {pending:?} was pending",
                    pending = self.pending_epoch.as_ref().map(|(e, _)| e),
                );
                self.pending_epoch = Some((resource_epoch, summary));
            }

            OperationApplyStart { index: (e, o) } => {
                let op = self.op_mut(e, o)?;
                op.is_complete = false;
                op.error = None;
                self.last_activity_op = Some((e, o));
            }
            OperationApplyStdout {
                index: (e, o),
                stdout: _,
            } => {
                // Bytes are not retained on the view: they flow straight
                // through to consumers (the TUI's vt100 parser, the plain
                // renderer's per-event digest). Touch the slot to validate
                // the (e, o) index and update follow-mode tracking.
                self.op_mut(e, o)?;
                self.last_activity_op = Some((e, o));
            }
            OperationApplyStderr {
                index: (e, o),
                stderr: _,
            } => {
                self.op_mut(e, o)?;
                self.last_activity_op = Some((e, o));
            }
            OperationApplyComplete {
                index: (e, o),
                error,
            } => {
                let op = self.op_mut(e, o)?;
                op.is_complete = true;
                op.error = error;
                self.last_activity_op = Some((e, o));
            }

            ResourceApplyFailed { index, error } => {
                self.transition_leaf("ResourceApplyFailed", index, |prev| match prev {
                    LeafState::Changed {
                        resource,
                        state,
                        change,
                        ops: Some(ops),
                    } => Ok(LeafState::Failed {
                        resource: resource.clone(),
                        state: state.clone(),
                        change: change.clone(),
                        ops: ops.clone(),
                        error: error.clone(),
                    }),
                    other => Err(other.name()),
                })?;
            }

            ApplyComplete { had_changes } => {
                self.had_changes = self.had_changes || had_changes;
                self.done = true;
                self.pending_epoch = None;
            }
        }
        Ok(self)
    }

    /// Apply a per-leaf transition. The closure receives the current state
    /// and returns either the next state or the rejection's state name.
    /// After a successful transition, updates `last_activity_atom` for
    /// follow-mode and arms `auto_follow_armed` on the first Probing entry.
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
        // Follow-mode tracking: refresh `last_activity_atom` for operator-
        // facing transitions. `NoChange` is intentionally skipped - the leaf
        // resolved without anything happening, so there's no activity to
        // follow.
        match &next {
            LeafState::Probing { .. } => {
                self.last_activity_atom = Some(index);
                self.auto_follow_armed = true;
            }
            LeafState::Probed { .. } | LeafState::Changed { .. } | LeafState::Failed { .. } => {
                self.last_activity_atom = Some(index);
            }
            LeafState::Planned { .. } | LeafState::NoChange { .. } => {}
        }
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

    /// Plan-item metadata for a branch in the atoms tree: id, requires,
    /// required_by, and on_change handlers.
    ///
    /// Returns `None` if no `ResourcesNode` has been folded yet, the index is
    /// out of range, the slot has been tombstoned, or the slot is a leaf.
    pub fn plan_item_meta(&self, branch_arena_index: usize) -> Option<&PlanMeta> {
        match self.resources.as_ref()?.nodes.get(branch_arena_index)? {
            Some(ResourcesNode::Branch { meta, .. }) => Some(meta),
            _ => None,
        }
    }

    /// Total resource-epoch count, set by `PipelineInfo`. `None` until the
    /// event arrives.
    pub fn resource_epochs_total(&self) -> Option<usize> {
        self.resource_epochs_total
    }

    /// Resource epoch the atom at `arena_index` runs in. `None` if
    /// `PipelineInfo` hasn't arrived yet or the index isn't a leaf in the
    /// shipped atoms tree.
    pub fn epoch_of_atom(&self, arena_index: usize) -> Option<usize> {
        self.atom_epoch.get(&arena_index).copied()
    }

    /// Per-internal-op-epoch metadata (`resource_epoch` + `phase`) for the
    /// given `epoch_index` (the counter shipped on each
    /// `OperationsApplyEpochAdded`). `None` if no such epoch has been folded
    /// yet.
    pub fn operation_epoch_meta(&self, epoch_index: usize) -> Option<&OperationEpochMeta> {
        self.operation_epoch_meta.get(epoch_index)
    }

    /// Count of leaves whose computed diff was non-empty: `LeafState::Changed`
    /// and `LeafState::Failed` (the latter had a change that the apply
    /// attempted but couldn't complete). Branches contribute nothing (the
    /// rollup is recomputed at draw time); other states don't count. Drives
    /// the header strip's `~N changes` indicator.
    pub fn changed_count(&self) -> usize {
        let Some(tree) = self.resources.as_ref() else {
            return 0;
        };
        tree.leaves()
            .filter(|s| matches!(s, LeafState::Changed { .. } | LeafState::Failed { .. }))
            .count()
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
                LeafState::Changed { ops: Some(_), .. } | LeafState::Failed { .. } => {
                    has_ops = true
                }
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
            | LeafState::Changed { state, .. }
            | LeafState::Failed { state, .. } => Some(Lifecycle::Complete(state.clone())),
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
            LeafState::Changed { change, .. } | LeafState::Failed { change, .. } => {
                Some(Lifecycle::Complete(change.clone()))
            }
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
                })
                | Some(ResourcesNode::Leaf {
                    state:
                        LeafState::Failed {
                            ops: (subtree, seq),
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
                sudo: false,
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
            sudo: false,
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
                    scope_path: Vec::new(),
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

    fn leaf_lifecycle<T>(tree: &ProjectedTree<T>, idx: usize) -> &Lifecycle<T> {
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
                resource_epoch: 0,
                phase: Phase::Change,
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

    /// A branch with `id`, `requires`, `required_by`, and an `on_change`
    /// handler folds through `ResourcesNode` and is then readable via
    /// `plan_item_meta(branch_arena_index)`. The arena index mirrors the
    /// pre-order walk of the original `PlanTree<Resource>`.
    #[test]
    fn plan_item_meta_round_trips_through_resources_node() {
        let plan_item_id = PlanNodeId::PlanItem {
            scope_path: Vec::new(),
            plan_id: PlanId::Path(PathBuf::from("test.lusid")),
            item_id: "nginx-config".into(),
        };
        let upstream_id = PlanNodeId::PlanItem {
            scope_path: Vec::new(),
            plan_id: PlanId::Path(PathBuf::from("test.lusid")),
            item_id: "nginx-install".into(),
        };
        let downstream_id = PlanNodeId::PlanItem {
            scope_path: Vec::new(),
            plan_id: PlanId::Path(PathBuf::from("test.lusid")),
            item_id: "nginx-reload".into(),
        };
        let plan_item_branch = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(plan_item_id.clone()),
                requires: vec![upstream_id.clone()],
                required_by: vec![downstream_id.clone()],
                handlers: vec![command_op("systemctl reload nginx")],
            },
            children: vec![resource_leaf("/etc/nginx/nginx.conf")],
        };
        let root = PlanTree::Branch {
            meta: PlanMeta::default(),
            children: vec![plan_item_branch],
        };

        let v = AppView::default()
            .update(AppUpdate::ResourcesStart)
            .unwrap()
            .update(AppUpdate::ResourcesNode {
                index: 0,
                tree: root,
            })
            .unwrap()
            .update(AppUpdate::ResourcesComplete)
            .unwrap();

        // Arena indices (pre-order): 0=root, 1=plan-item branch, 2=leaf.
        let root_meta = v.plan_item_meta(0).expect("root meta");
        assert!(root_meta.id.is_none());
        assert!(root_meta.requires.is_empty());
        assert!(root_meta.required_by.is_empty());
        assert!(root_meta.handlers.is_empty());

        let meta = v.plan_item_meta(1).expect("plan-item meta");
        assert_eq!(meta.id.as_ref(), Some(&plan_item_id));
        assert_eq!(meta.requires, vec![upstream_id]);
        assert_eq!(meta.required_by, vec![downstream_id]);
        assert_eq!(meta.handlers.len(), 1);
        assert!(
            matches!(
                meta.handlers[0],
                Operation::Command(CommandOperation { ref command, .. }) if command == "systemctl reload nginx",
            ),
            "handler {:?}",
            meta.handlers[0],
        );

        assert!(v.plan_item_meta(2).is_none(), "leaf index returns None");
        assert!(v.plan_item_meta(99).is_none(), "out-of-range returns None");
    }

    #[test]
    fn plan_item_meta_returns_none_before_resources_arrive() {
        let v = AppView::default();
        assert!(v.plan_item_meta(0).is_none());
    }

    #[test]
    fn pipeline_info_populates_total_and_atom_epoch_map() {
        let v = AppView::default();
        assert_eq!(v.resource_epochs_total(), None);
        assert!(v.epoch_of_atom(0).is_none());

        let atom_epoch: HashMap<usize, usize> = [(1, 0), (2, 1), (3, 2)].into_iter().collect();
        let v = v
            .update(AppUpdate::PipelineInfo {
                resource_epochs_total: 3,
                atom_epoch: atom_epoch.clone(),
            })
            .unwrap();

        assert_eq!(v.resource_epochs_total(), Some(3));
        assert_eq!(v.epoch_of_atom(1), Some(0));
        assert_eq!(v.epoch_of_atom(2), Some(1));
        assert_eq!(v.epoch_of_atom(3), Some(2));
        assert_eq!(v.epoch_of_atom(99), None);
    }

    #[test]
    fn operations_apply_epoch_added_records_resource_epoch_and_phase() {
        let v = AppView::default()
            .update(AppUpdate::OperationsApplyEpochAdded {
                epoch_index: 0,
                resource_epoch: 1,
                phase: Phase::Change,
                operations: vec![command_op("op-a")],
            })
            .unwrap()
            .update(AppUpdate::OperationsApplyEpochAdded {
                epoch_index: 1,
                resource_epoch: 1,
                phase: Phase::OnChange,
                operations: vec![command_op("op-b")],
            })
            .unwrap();

        let change_meta = v.operation_epoch_meta(0).expect("change-phase meta");
        assert_eq!(change_meta.resource_epoch, 1);
        assert_eq!(change_meta.phase, Phase::Change);

        let on_change_meta = v.operation_epoch_meta(1).expect("on-change-phase meta");
        assert_eq!(on_change_meta.resource_epoch, 1);
        assert_eq!(on_change_meta.phase, Phase::OnChange);

        assert!(
            v.operation_epoch_meta(2).is_none(),
            "out-of-range returns None"
        );
    }

    fn change_label(atom: &str) -> ChangeLabel {
        ChangeLabel {
            atom_id: atom.into(),
            kind: ChangeKind::Modified,
            summary: format!("change {atom}"),
        }
    }

    #[test]
    fn epoch_ready_sets_pending_until_first_op_for_that_epoch() {
        let summary = EpochSummary {
            atoms_total: 2,
            atoms_changed: 1,
            handlers_pending: 0,
            change_labels: vec![change_label("/etc/foo")],
            truncated_count: 0,
        };
        let v = AppView::default()
            .update(AppUpdate::EpochReady {
                resource_epoch: 0,
                summary: summary.clone(),
            })
            .unwrap();
        let pending = v.pending_epoch.as_ref().expect("pending set");
        assert_eq!(pending.0, 0);
        assert_eq!(pending.1.atoms_changed, 1);

        // First op for the same resource_epoch clears the prompt state.
        let v = v
            .update(AppUpdate::OperationsApplyEpochAdded {
                epoch_index: 0,
                resource_epoch: 0,
                phase: Phase::Change,
                operations: vec![command_op("op")],
            })
            .unwrap();
        assert!(v.pending_epoch.is_none(), "running clears the prompt");
    }

    /// An op event for a *different* resource_epoch (rare: empty epoch
    /// pre-ack landed between EpochReady and the matching epoch) must not
    /// clear the prompt. Otherwise the user would see "ready" disappear
    /// without acking the right epoch.
    #[test]
    fn epoch_ready_pending_survives_op_for_other_epoch() {
        let summary = EpochSummary {
            atoms_total: 1,
            atoms_changed: 1,
            handlers_pending: 0,
            change_labels: vec![],
            truncated_count: 0,
        };
        let v = AppView::default()
            .update(AppUpdate::EpochReady {
                resource_epoch: 2,
                summary,
            })
            .unwrap()
            .update(AppUpdate::OperationsApplyEpochAdded {
                epoch_index: 0,
                resource_epoch: 1,
                phase: Phase::Change,
                operations: vec![command_op("op")],
            })
            .unwrap();
        assert!(v.pending_epoch.is_some());
        assert_eq!(v.pending_epoch.as_ref().unwrap().0, 2);
    }

    #[test]
    fn apply_complete_clears_pending() {
        let summary = EpochSummary {
            atoms_total: 1,
            atoms_changed: 1,
            handlers_pending: 0,
            change_labels: vec![],
            truncated_count: 0,
        };
        let v = AppView::default()
            .update(AppUpdate::EpochReady {
                resource_epoch: 0,
                summary,
            })
            .unwrap()
            .update(AppUpdate::ApplyComplete { had_changes: false })
            .unwrap();
        assert!(v.pending_epoch.is_none());
    }

    #[test]
    fn ack_action_round_trips_through_envelope() {
        let apply_line = serde_json::to_string(&AckAction::Apply).unwrap();
        assert_eq!(apply_line, r#"{"action":"apply"}"#);
        let abort_line = serde_json::to_string(&AckAction::Abort).unwrap();
        assert_eq!(abort_line, r#"{"action":"abort"}"#);

        let back: AckAction = serde_json::from_str(&apply_line).unwrap();
        assert_eq!(back, AckAction::Apply);
        let back: AckAction = serde_json::from_str(&abort_line).unwrap();
        assert_eq!(back, AckAction::Abort);
    }

    /// A rejected `OperationsApplyEpochAdded` must not advance either
    /// `operations_epochs` or `operation_epoch_meta`; otherwise the parallel
    /// invariant between the two vecs drifts on the next valid event.
    #[test]
    fn non_monotonic_epoch_index_does_not_push_meta() {
        let v = AppView::default()
            .update(AppUpdate::OperationsApplyEpochAdded {
                epoch_index: 0,
                resource_epoch: 0,
                phase: Phase::Change,
                operations: vec![command_op("op")],
            })
            .unwrap();
        assert_eq!(v.operations_epochs.len(), 1);
        assert_eq!(v.operation_epoch_meta.len(), 1);

        // Re-emit epoch_index 0 — the fold expects 1 next.
        let err = v
            .clone()
            .update(AppUpdate::OperationsApplyEpochAdded {
                epoch_index: 0,
                resource_epoch: 1,
                phase: Phase::OnChange,
                operations: vec![command_op("op2")],
            })
            .unwrap_err();
        assert!(matches!(
            err,
            AppViewError::NonMonotonicEpochIndex {
                got: 0,
                expected: 1,
            }
        ));

        // Original view's parallel-vec invariant is untouched.
        assert_eq!(v.operations_epochs.len(), v.operation_epoch_meta.len());
    }

    /// `last_activity_atom` advances on probe/result transitions but stays put
    /// when a leaf resolves to `NoChange` (operators don't follow to a
    /// "nothing happened" terminal).
    #[test]
    fn last_activity_atom_tracks_probing_probed_changed_skips_no_change() {
        let v = app_view_with_two_leaves();
        assert!(v.last_activity_atom.is_none());

        // ResourceStatesNodeStart drives Planned -> Probing for leaf 1.
        let v = v
            .update(AppUpdate::ResourceStatesNodeStart { index: 1 })
            .unwrap();
        assert_eq!(v.last_activity_atom, Some(1));

        let v = v
            .update(AppUpdate::ResourceStatesNodeComplete {
                index: 1,
                state: file_state(),
            })
            .unwrap();
        assert_eq!(v.last_activity_atom, Some(1));

        let v = v
            .update(AppUpdate::ResourceChangesNode {
                index: 1,
                change: Some(apt_change()),
            })
            .unwrap();
        assert_eq!(v.last_activity_atom, Some(1));

        // Move on to leaf 2; resolve it to NoChange. last_activity_atom
        // shifts on Probing (2) and Probed (still 2), but does NOT shift on
        // the terminal NoChange.
        let v = v
            .update(AppUpdate::ResourceStatesNodeStart { index: 2 })
            .unwrap();
        assert_eq!(v.last_activity_atom, Some(2));
        let v = v
            .update(AppUpdate::ResourceStatesNodeComplete {
                index: 2,
                state: file_state(),
            })
            .unwrap();
        assert_eq!(v.last_activity_atom, Some(2));
        let v = v
            .update(AppUpdate::ResourceChangesNode {
                index: 2,
                change: None,
            })
            .unwrap();
        // NoChange must not advance the pointer past its prior position;
        // since we're at 2 already this just asserts it didn't get cleared
        // or reset.
        assert_eq!(v.last_activity_atom, Some(2));
    }

    /// `last_activity_op` updates on every op lifecycle event so follow-mode
    /// can pin selection to the running op on the Epochs page.
    #[test]
    fn last_activity_op_tracks_every_op_lifecycle_event() {
        let v = app_view_with_two_leaves()
            .update(AppUpdate::OperationsApplyEpochAdded {
                epoch_index: 0,
                resource_epoch: 0,
                phase: Phase::Change,
                operations: vec![command_op("op-a"), command_op("op-b")],
            })
            .unwrap();
        assert!(v.last_activity_op.is_none());

        let v = v
            .update(AppUpdate::OperationApplyStart { index: (0, 0) })
            .unwrap();
        assert_eq!(v.last_activity_op, Some((0, 0)));

        let v = v
            .update(AppUpdate::OperationApplyStdout {
                index: (0, 0),
                stdout: b"line".to_vec(),
            })
            .unwrap();
        assert_eq!(v.last_activity_op, Some((0, 0)));

        let v = v
            .update(AppUpdate::OperationApplyStderr {
                index: (0, 1),
                stderr: b"warn".to_vec(),
            })
            .unwrap();
        assert_eq!(v.last_activity_op, Some((0, 1)));

        let v = v
            .update(AppUpdate::OperationApplyComplete {
                index: (0, 0),
                error: None,
            })
            .unwrap();
        assert_eq!(v.last_activity_op, Some((0, 0)));
    }

    /// `changed_count` counts only `Changed` leaves; other states (Planned,
    /// Probing, Probed, NoChange) and branches don't contribute.
    #[test]
    fn changed_count_counts_only_changed_leaves() {
        // Zero before resources arrive.
        assert_eq!(AppView::default().changed_count(), 0);

        // Zero with two Planned leaves.
        let v = app_view_with_two_leaves();
        assert_eq!(v.changed_count(), 0);

        // Move leaf 1 through to Changed; leaf 2 to NoChange. Only the
        // Changed leaf counts (one).
        let v = v
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
            .update(AppUpdate::ResourceStatesNodeStart { index: 2 })
            .unwrap()
            .update(AppUpdate::ResourceStatesNodeComplete {
                index: 2,
                state: file_state(),
            })
            .unwrap()
            .update(AppUpdate::ResourceChangesNode {
                index: 2,
                change: None,
            })
            .unwrap();
        assert_eq!(v.changed_count(), 1);
    }

    /// Many-changes case: build a fresh view with three leaves, all Changed.
    /// Verifies the count scales (and that branches in the arena are
    /// ignored).
    #[test]
    fn changed_count_with_many_changes() {
        let tree = PlanTree::Branch {
            meta: PlanMeta::default(),
            children: vec![
                resource_leaf("/a"),
                resource_leaf("/b"),
                resource_leaf("/c"),
            ],
        };
        let mut v = AppView::default()
            .update(AppUpdate::ResourcesStart)
            .unwrap()
            .update(AppUpdate::ResourcesNode { index: 0, tree })
            .unwrap();
        for idx in [1, 2, 3] {
            v = v
                .update(AppUpdate::ResourceStatesNodeStart { index: idx })
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
                .unwrap();
        }
        assert_eq!(v.changed_count(), 3);
    }

    /// Drive a leaf all the way to `Changed { ops: Some }` so the
    /// `ResourceApplyFailed` transition tests below can reuse a single
    /// helper.
    fn advance_to_changed_with_ops(view: AppView, idx: usize) -> AppView {
        view.update(AppUpdate::ResourceStatesNodeStart { index: idx })
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
            .update(AppUpdate::OperationsNode {
                index: idx,
                operations: op_leaf("op"),
            })
            .unwrap()
    }

    #[test]
    fn resource_apply_failed_transitions_changed_to_failed() {
        let v = advance_to_changed_with_ops(app_view_with_two_leaves(), 1);
        let v = v
            .update(AppUpdate::ResourceApplyFailed {
                index: 1,
                error: "boom".into(),
            })
            .unwrap();
        match leaf_state(&v, 1) {
            LeafState::Failed { error, .. } => assert_eq!(error, "boom"),
            other => panic!("expected Failed, got {other:?}"),
        }
        // Failed leaves should keep counting toward "atoms with a change"
        // because they had a non-empty diff the apply attempted to land.
        assert_eq!(v.changed_count(), 1);
    }

    #[test]
    fn resource_apply_failed_rejected_before_ops_assigned() {
        // Changed { ops: None }: ops subtree event hasn't arrived for this
        // leaf, so the producer would never legitimately ship a failure for
        // an op we never announced. Reject loudly.
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
            .unwrap();
        let err = v
            .update(AppUpdate::ResourceApplyFailed {
                index: 1,
                error: "boom".into(),
            })
            .unwrap_err();
        assert!(
            matches!(
                err,
                AppViewError::InvalidLeafTransition {
                    state: "Changed { ops: None }",
                    ..
                }
            ),
            "got {err:?}"
        );
    }

    #[test]
    fn resource_apply_failed_rejected_on_no_change_leaf() {
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
        let err = v
            .update(AppUpdate::ResourceApplyFailed {
                index: 1,
                error: "boom".into(),
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

    #[test]
    fn resource_apply_failed_is_terminal() {
        // Re-emission of the same event on a Failed leaf is rejected; once
        // failed, the producer must not transition it back. Guards against
        // protocol-level double-fires that would otherwise silently overwrite
        // the recorded error.
        let v = advance_to_changed_with_ops(app_view_with_two_leaves(), 1);
        let v = v
            .update(AppUpdate::ResourceApplyFailed {
                index: 1,
                error: "first".into(),
            })
            .unwrap();
        let err = v
            .update(AppUpdate::ResourceApplyFailed {
                index: 1,
                error: "second".into(),
            })
            .unwrap_err();
        assert!(
            matches!(
                err,
                AppViewError::InvalidLeafTransition {
                    state: "Failed",
                    ..
                }
            ),
            "got {err:?}"
        );
    }

    #[test]
    fn failed_leaf_updates_last_activity_atom() {
        let v = advance_to_changed_with_ops(app_view_with_two_leaves(), 1);
        // Wipe the activity recorded by earlier transitions so this
        // assertion is unambiguous about which event refreshed it.
        let v = AppView {
            last_activity_atom: None,
            ..v
        };
        let v = v
            .update(AppUpdate::ResourceApplyFailed {
                index: 1,
                error: "boom".into(),
            })
            .unwrap();
        assert_eq!(v.last_activity_atom, Some(1));
    }

    /// `auto_follow_armed` flips on the first Probing transition and stays
    /// `true` forever after. The TUI uses a pre/post check around `update`
    /// to detect the one-shot edge.
    #[test]
    fn auto_follow_armed_flips_on_first_probing_only() {
        let v = app_view_with_two_leaves();
        assert!(!v.auto_follow_armed, "starts disarmed");

        let v = v
            .update(AppUpdate::ResourceStatesNodeStart { index: 1 })
            .unwrap();
        assert!(v.auto_follow_armed, "armed on first Probing");

        // A second Probing keeps the flag on; the flag never falls back.
        let v = v
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
            .update(AppUpdate::ResourceStatesNodeStart { index: 2 })
            .unwrap();
        assert!(v.auto_follow_armed, "stays armed after second Probing");
    }

    /// The base64 adapter must round-trip arbitrary bytes verbatim - ANSI
    /// escape sequences in particular, since the whole point of swapping
    /// the wire from `String` to `Vec<u8>` was to preserve them.
    #[test]
    fn stdout_payload_round_trips_ansi_bytes() {
        let original = b"\x1b[31mred\x1b[0m \x07\xff\x00 done".to_vec();
        let update = AppUpdate::OperationApplyStdout {
            index: (0, 0),
            stdout: original.clone(),
        };
        let line = serde_json::to_string(&update).unwrap();
        let decoded: AppUpdate = serde_json::from_str(&line).unwrap();
        match decoded {
            AppUpdate::OperationApplyStdout { stdout, .. } => assert_eq!(stdout, original),
            other => panic!("expected stdout variant, got {other:?}"),
        }
    }

    /// Folding `OperationApplyStdout`/`Stderr` validates the slot index and
    /// updates `last_activity_op`, but does not retain the bytes on the
    /// view (consumers stream them).
    #[test]
    fn stdout_event_validates_slot_and_advances_activity() {
        let v = app_view_with_two_leaves()
            .update(AppUpdate::OperationsApplyEpochAdded {
                epoch_index: 0,
                resource_epoch: 0,
                phase: Phase::Change,
                operations: vec![command_op("op-a")],
            })
            .unwrap()
            .update(AppUpdate::OperationApplyStdout {
                index: (0, 0),
                stdout: b"hello".to_vec(),
            })
            .unwrap();
        assert_eq!(v.last_activity_op, Some((0, 0)));
        // An event targeting a slot that doesn't exist is rejected.
        let err = v
            .update(AppUpdate::OperationApplyStderr {
                index: (5, 5),
                stderr: b"x".to_vec(),
            })
            .unwrap_err();
        assert!(matches!(err, AppViewError::OperationIndexOutOfBounds(5, 5)));
    }
}
