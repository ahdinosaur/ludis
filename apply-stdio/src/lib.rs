//! Wire protocol between `lusid-apply` (producer) and the `lusid` TUI (consumer).
//!
//! `lusid-apply` emits newline-delimited JSON [`AppUpdate`]s on stdout as the
//! pipeline progresses. The TUI deserializes each update and folds it into an
//! [`AppView`] - a flat collection of optional per-stage [`FlatViewTree`]s
//! that fill in over time, plus a per-epoch operations apply pane.
//!
//! ## Pipeline shape
//!
//! 1. Plan source -> [`AppUpdate::ResourceParams`] (full tree, one event).
//! 2. Resource expansion -> `Resources*` events (the atoms tree, matching the
//!    plan as written - one leaf per resource atom).
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
//! Events from the per-epoch loop interleave: state events for atoms in
//! epoch 1 arrive after apply events for epoch 0's ops. The TUI tolerates this
//! - each event mutates only its own field.
//!
//! ## FlatViewTree
//!
//! Mirrors [`lusid_tree::FlatTree`](lusid_tree::FlatTree) but storing
//! [`lusid_view::View`]s instead of domain nodes, so the TUI never needs to
//! understand lusid's domain types. Arena is `Vec<Option<Node>>`; missing
//! children / out-of-bounds indices are tolerated (lenient rendering).
//! Subtrees are appended; "replace subtree at index" recursively clears the
//! old children before writing the new.
//!
//! [`FlatViewTree::template`] strips leaves back to [`ViewNode::NotStarted`]
//! while preserving the structure - each pipeline phase builds from the
//! resources template, so the TUI shows the eventual shape up-front and fills
//! leaves in as work completes.

use lusid_view::{Fragment, Render, View, ViewTree};
use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Per-leaf progress marker, rendered with an emoji prefix:
/// 🟩 not-started, ⌛ in-flight, ✅ + the finished view.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub enum ViewNode {
    #[default]
    NotStarted,
    Started,
    Complete(View),
}

impl Render for ViewNode {
    fn render(&self) -> View {
        match self {
            ViewNode::NotStarted => View::Span("🟩".into()),
            ViewNode::Started => View::Span("⌛".into()),
            ViewNode::Complete(view) => {
                View::Fragment(Fragment::new(vec![View::Span("✅".into()), view.clone()]))
            }
        }
    }
}

/// Arena entry. Branch children are indices into the containing
/// [`FlatViewTree`]; leaves carry a [`ViewNode`] progress marker directly
/// (not a [`View`]), since leaves are the nodes that advance through the
/// "not started → started → complete" lifecycle.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FlatViewTreeNode {
    Branch { view: View, children: Vec<usize> },
    Leaf { view: ViewNode },
}

/// Arena-backed view tree, root fixed at index `0`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct FlatViewTree {
    nodes: Vec<Option<FlatViewTreeNode>>,
}

#[derive(Debug, Error)]
pub enum FlatViewTreeError {
    #[error("index {0} is out of bounds")]
    IndexOutOfBounds(usize),

    #[error("node at index {0} is None")]
    NodeMissing(usize),

    #[error("expected leaf at index {0}")]
    NotALeaf(usize),
}

impl FlatViewTree {
    /// The root index is always zero.
    pub const fn root_index() -> usize {
        0
    }

    /// Get a reference to the root node, if any.
    pub fn root(&self) -> Option<&FlatViewTreeNode> {
        self.nodes.first().and_then(|n| n.as_ref())
    }

    pub fn nodes(&self) -> impl Iterator<Item = &Option<FlatViewTreeNode>> {
        self.nodes.iter()
    }

    /// Returns true if the root node is missing.
    pub fn is_empty(&self) -> bool {
        self.root().is_none()
    }

    /// Get a node by index, with error handling.
    pub fn get(&self, index: usize) -> Result<&FlatViewTreeNode, FlatViewTreeError> {
        let node = self
            .nodes
            .get(index)
            .ok_or(FlatViewTreeError::IndexOutOfBounds(index))?;
        node.as_ref().ok_or(FlatViewTreeError::NodeMissing(index))
    }

    /// Get a mutable node by index, with error handling.
    pub fn get_mut(&mut self, index: usize) -> Result<&mut FlatViewTreeNode, FlatViewTreeError> {
        let node = self
            .nodes
            .get_mut(index)
            .ok_or(FlatViewTreeError::IndexOutOfBounds(index))?;
        node.as_mut().ok_or(FlatViewTreeError::NodeMissing(index))
    }

    /// Build a flat tree by appending a completed ViewTree (children are appended).
    pub fn from_view_tree_completed(view_tree: ViewTree) -> Self {
        let mut nodes = Vec::<Option<FlatViewTreeNode>>::new();
        append_view_tree_nodes(&mut nodes, view_tree);
        FlatViewTree { nodes }
    }

    /// Replace the subtree at `root_index` with a completed `view_tree`.
    pub fn replace_subtree_completed(&mut self, root_index: usize, view_tree: ViewTree) {
        replace_view_tree_nodes(&mut self.nodes, Some(view_tree), root_index);
    }

    /// Mark a leaf as started.
    pub fn set_leaf_started(&mut self, index: usize) -> Result<(), FlatViewTreeError> {
        self.set_leaf_view(index, ViewNode::Started)
    }

    /// Replace an existing leaf with a ViewNode.
    pub fn set_leaf_view(
        &mut self,
        index: usize,
        new_view: ViewNode,
    ) -> Result<(), FlatViewTreeError> {
        self.ensure_index_exists(index);
        match self.nodes[index].as_mut() {
            Some(FlatViewTreeNode::Leaf { view }) => {
                *view = new_view;
                Ok(())
            }
            Some(FlatViewTreeNode::Branch { .. }) => Err(FlatViewTreeError::NotALeaf(index)),
            None => {
                self.nodes[index] = Some(FlatViewTreeNode::Leaf { view: new_view });
                Ok(())
            }
        }
    }

    /// Remove the node at index (used for pruning "no-change" leaves).
    pub fn set_node_none(&mut self, index: usize) {
        self.ensure_index_exists(index);
        self.nodes[index] = None;
    }

    /// Produce a "template" tree that mirrors this structure but resets all
    /// leaves to ViewNode::NotStarted. Branch views and child indices are kept.
    pub fn template(&self) -> FlatViewTree {
        let mut nodes = Vec::with_capacity(self.nodes.len());
        for node in self.nodes.iter() {
            let mapped = match node {
                None => None,
                Some(FlatViewTreeNode::Leaf { .. }) => Some(FlatViewTreeNode::Leaf {
                    view: ViewNode::NotStarted,
                }),
                Some(FlatViewTreeNode::Branch { view, children }) => {
                    Some(FlatViewTreeNode::Branch {
                        view: view.clone(),
                        children: children.clone(),
                    })
                }
            };
            nodes.push(mapped);
        }
        FlatViewTree { nodes }
    }

    fn ensure_index_exists(&mut self, index: usize) {
        if self.nodes.len() <= index {
            self.nodes.resize(index + 1, None);
        }
    }
}

/// Append a (completed) view tree into a flat arena, returning the root index.
/// Root is at index 0 if this is the first append.
fn append_view_tree_nodes(nodes: &mut Vec<Option<FlatViewTreeNode>>, view_tree: ViewTree) -> usize {
    match view_tree {
        ViewTree::Leaf { view } => {
            let index = nodes.len();
            nodes.push(Some(FlatViewTreeNode::Leaf {
                view: ViewNode::Complete(view),
            }));
            index
        }
        ViewTree::Branch { view, children } => {
            let index = nodes.len();
            nodes.push(Some(FlatViewTreeNode::Branch {
                view,
                children: Vec::new(),
            }));
            let mut child_indices = Vec::with_capacity(children.len());
            for child in children {
                let child_index = append_view_tree_nodes(nodes, child);
                child_indices.push(child_index);
            }
            if let Some(FlatViewTreeNode::Branch { children, .. }) = nodes[index].as_mut() {
                *children = child_indices;
            }
            index
        }
    }
}

/// Replace the subtree at `root_index` in-place with `view_tree` (or remove if None).
fn replace_view_tree_nodes(
    nodes: &mut Vec<Option<FlatViewTreeNode>>,
    view_tree: Option<ViewTree>,
    root_index: usize,
) {
    // Recursively remove previous children under this root (if it is a branch).
    if let Some(Some(FlatViewTreeNode::Branch { children, .. })) = nodes.get(root_index) {
        for child in children.clone() {
            replace_view_tree_nodes(nodes, None, child);
        }
    }

    match view_tree {
        None => {
            if root_index < nodes.len() {
                nodes[root_index] = None;
            } else {
                // If out-of-bounds, extend and set None for clarity.
                nodes.resize(root_index + 1, None);
                nodes[root_index] = None;
            }
        }
        Some(ViewTree::Leaf { view }) => {
            if root_index >= nodes.len() {
                nodes.resize(root_index + 1, None);
            }
            nodes[root_index] = Some(FlatViewTreeNode::Leaf {
                view: ViewNode::Complete(view),
            });
        }
        Some(ViewTree::Branch { view, children }) => {
            // Append all children and attach to branch.
            let mut child_indices = Vec::with_capacity(children.len());
            for child in children {
                let child_index = append_view_tree_nodes(nodes, child);
                child_indices.push(child_index);
            }
            if root_index >= nodes.len() {
                nodes.resize(root_index + 1, None);
            }
            nodes[root_index] = Some(FlatViewTreeNode::Branch {
                view,
                children: child_indices,
            });
        }
    }
}

/// Protocol message from `lusid-apply` to the TUI.
///
/// The protocol is permissive: events from per-epoch processing arrive
/// interleaved (state events for atoms in epoch N can arrive after apply
/// events for ops in epoch N-1). The TUI applies each event to its own
/// field without enforcing a global phase order.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AppUpdate {
    /// Full resource-params tree, one event up front.
    ResourceParams {
        resource_params: ViewTree,
    },

    /// Begin filling in the atoms tree - the resource atoms produced by
    /// expanding each plan item's `ResourceParams`. One leaf per atom; no
    /// synthetic nodes for `on_change` handlers (those fire in Phase B and
    /// appear only in the apply pane).
    ResourcesStart,
    ResourcesNode {
        index: usize,
        tree: ViewTree,
    },
    ResourcesComplete,

    /// Tells the TUI we have N resource epochs scheduled.
    ResourceEpochsStart {
        count: usize,
    },

    /// Per-leaf state-probe lifecycle events. `Start` fires when the probe
    /// future is dispatched; `Complete` fires when it resolves.
    ResourceStatesNodeStart {
        index: usize,
    },
    ResourceStatesNodeComplete {
        index: usize,
        node: View,
    },

    /// Per-leaf computed change. `node` is `None` when the diff is empty
    /// (no-op); the TUI prunes the leaf in that case.
    ResourceChangesNode {
        index: usize,
        node: Option<View>,
    },

    /// Per-leaf operations subtree (one or more concrete operations to
    /// execute). Replaces the leaf at `index` in the operations tree.
    OperationsNode {
        index: usize,
        operations: ViewTree,
    },

    /// One internal operation epoch's merged op list, appended to the apply
    /// pane. The TUI grows `operations_epochs` as these arrive.
    OperationsApplyEpochAdded {
        epoch_index: usize,
        operations: Vec<View>,
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
    pub label: View,
    pub stdout: String,
    pub stderr: String,
    pub is_complete: bool,
    pub error: Option<String>,
}

impl OperationView {
    fn new(label: View) -> Self {
        Self {
            label,
            stdout: String::new(),
            stderr: String::new(),
            is_complete: false,
            error: None,
        }
    }
}

/// TUI state. Each per-stage tree is `Option<FlatViewTree>` so the UI can show
/// partial progress as soon as data arrives, without enforcing a strict phase
/// order. The pipeline emits events in causality order, but events for
/// different stages interleave per resource epoch.
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct AppView {
    pub resource_params: Option<FlatViewTree>,
    /// The atoms tree - one leaf per resource atom, matching the plan as written.
    pub resources: Option<FlatViewTree>,
    pub resource_states: Option<FlatViewTree>,
    pub resource_changes: Option<FlatViewTree>,
    pub operations_tree: Option<FlatViewTree>,
    pub operations_epochs: Vec<Vec<OperationView>>,
    /// Number of resource epochs scheduled, once known.
    pub resource_epochs_total: Option<usize>,
    /// Set true the first time any `ResourceChangesNode` arrives with `Some`.
    pub had_changes: bool,
    /// True after `ApplyComplete`.
    pub done: bool,
}

#[derive(Debug, Error)]
pub enum AppViewError {
    #[error(transparent)]
    FlatTree(#[from] FlatViewTreeError),

    #[error("operation index out of bounds: epoch={0}, op={1}")]
    OperationIndexOutOfBounds(usize, usize),

    /// `OperationsApplyEpochAdded` is the only event that grows
    /// `operations_epochs`; the producer must emit them strictly in order
    /// (epoch 0, then 1, then 2, ...). Anything else means the protocol
    /// stream is corrupt or the producer has a bug.
    #[error("operations-apply epoch index {got} arrived but expected {expected}")]
    NonMonotonicEpochIndex { got: usize, expected: usize },

    #[error("event {update:?} arrived before {expected_field} was initialised")]
    MissingField {
        expected_field: &'static str,
        update: String,
    },
}

impl AppView {
    /// Fold one [`AppUpdate`] into the view.
    ///
    /// Most events update exactly one field. Stage-template events
    /// (`ResourcesComplete`, `ResourceEpochsStart`) populate the per-stage
    /// trees as templates derived from the resources tree, so the TUI can
    /// render the eventual shape up-front while leaves fill in.
    pub fn update(mut self, update: AppUpdate) -> Result<Self, AppViewError> {
        use AppUpdate::*;
        match update {
            ResourceParams { resource_params } => {
                self.resource_params =
                    Some(FlatViewTree::from_view_tree_completed(resource_params));
            }

            ResourcesStart => {
                let template = self
                    .resource_params
                    .as_ref()
                    .map(|t| t.template())
                    .unwrap_or_default();
                self.resources = Some(template);
            }
            ResourcesNode { index, tree } => {
                let resources = self.resources.get_or_insert_with(FlatViewTree::default);
                resources.replace_subtree_completed(index, tree);
            }
            ResourcesComplete => {
                let template = self.resources.as_ref().map(|t| t.template());
                self.resource_states = template.clone();
                self.resource_changes = template.clone();
                self.operations_tree = template;
            }

            ResourceEpochsStart { count } => {
                self.resource_epochs_total = Some(count);
            }

            ResourceStatesNodeStart { index } => {
                let states = self
                    .resource_states
                    .get_or_insert_with(FlatViewTree::default);
                states.set_leaf_started(index)?;
            }
            ResourceStatesNodeComplete { index, node } => {
                let states = self
                    .resource_states
                    .get_or_insert_with(FlatViewTree::default);
                states.set_leaf_view(index, ViewNode::Complete(node))?;
            }

            ResourceChangesNode { index, node } => {
                let changes = self
                    .resource_changes
                    .get_or_insert_with(FlatViewTree::default);
                match node {
                    Some(view) => {
                        changes.set_leaf_view(index, ViewNode::Complete(view))?;
                        self.had_changes = true;
                    }
                    None => changes.set_node_none(index),
                }
            }

            OperationsNode { index, operations } => {
                let ops = self
                    .operations_tree
                    .get_or_insert_with(FlatViewTree::default);
                ops.replace_subtree_completed(index, operations);
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

    fn op_mut(&mut self, e: usize, o: usize) -> Result<&mut OperationView, AppViewError> {
        let epoch = self
            .operations_epochs
            .get_mut(e)
            .ok_or(AppViewError::OperationIndexOutOfBounds(e, o))?;
        epoch
            .get_mut(o)
            .ok_or(AppViewError::OperationIndexOutOfBounds(e, o))
    }

    pub fn resource_params(&self) -> Option<&FlatViewTree> {
        self.resource_params.as_ref()
    }

    pub fn resources(&self) -> Option<&FlatViewTree> {
        self.resources.as_ref()
    }

    pub fn resource_states(&self) -> Option<&FlatViewTree> {
        self.resource_states.as_ref()
    }

    pub fn resource_changes(&self) -> Option<&FlatViewTree> {
        self.resource_changes.as_ref()
    }

    pub fn operations_tree(&self) -> Option<&FlatViewTree> {
        self.operations_tree.as_ref()
    }

    pub fn operations_epochs(&self) -> Option<&Vec<Vec<OperationView>>> {
        if self.operations_epochs.is_empty() {
            None
        } else {
            Some(&self.operations_epochs)
        }
    }
}

/// Lenient conversion to nested ViewTree:
/// - Skips missing or invalid children
/// - If the root is missing, returns a single-node tree with "?".
impl From<FlatViewTree> for Option<ViewTree> {
    fn from(value: FlatViewTree) -> Self {
        fn build(tree: &mut [Option<FlatViewTreeNode>], index: usize) -> Option<ViewTree> {
            if index >= tree.len() {
                return None;
            }
            let node = tree[index].take()?;
            match node {
                FlatViewTreeNode::Leaf { view } => {
                    let view = view.render();
                    Some(ViewTree::Leaf { view })
                }
                FlatViewTreeNode::Branch { view, children } => {
                    let children: Vec<_> = children
                        .iter()
                        .filter_map(|child| build(tree, *child))
                        .collect();
                    if children.is_empty() {
                        return None;
                    }
                    Some(ViewTree::Branch { view, children })
                }
            }
        }

        let mut nodes = value.nodes;
        build(&mut nodes, 0)
    }
}

impl std::fmt::Display for FlatViewTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(tree) = Option::<ViewTree>::from(self.clone()) {
            tree.fmt(f)
        } else {
            write!(f, "<empty>")
        }
    }
}
