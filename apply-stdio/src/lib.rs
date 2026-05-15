//! Wire protocol between `lusid-apply` (producer) and the `lusid` TUI (consumer).
//!
//! `lusid-apply` emits newline-delimited JSON [`AppUpdate`]s on stdout as the
//! pipeline progresses (params → resources → states → changes → operations →
//! apply). The TUI deserializes each update and folds it into an [`AppView`]
//! — a phase-tagged state machine that accumulates one [`FlatViewTree`] per
//! pipeline stage, plus a `Vec<Vec<OperationView>>` for the per-epoch
//! streaming stdout/stderr during apply.
//!
//! ## FlatViewTree
//!
//! Mirrors [`lusid_tree::FlatTree`](lusid_tree::FlatTree) but storing
//! [`lusid_view::View`]s instead of domain nodes, so the TUI never needs to
//! understand lusid's domain types:
//!
//! - Root is always index `0`.
//! - Arena is `Vec<Option<Node>>`; missing children / out-of-bounds indices
//!   are tolerated (lenient rendering).
//! - Subtrees are appended; "replace subtree at index" recursively clears the
//!   old children before writing the new.
//!
//! [`FlatViewTree::template`] strips leaves back to [`ViewNode::NotStarted`]
//! while preserving the structure — each pipeline phase builds from the
//! previous phase's template, so the TUI shows the eventual shape up-front
//! and fills leaves in as work completes.
//!
//! ## AppView as a state machine
//!
//! [`AppView::update`] takes `(self, AppUpdate) -> Result<Self, AppViewError>`.
//! Invalid transitions return [`AppViewError::InvalidTransition`] so bad
//! input from the pipe can't silently corrupt UI state. Accessors
//! ([`AppView::resources`] etc.) return `None` before that phase has been
//! reached, so the TUI can render partial progress.

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

/// Protocol message from `lusid-apply` to the TUI. Each phase has a
/// `*Start` / per-node / `*Complete` triple. The `Operations*` cluster at
/// the end carries per-operation stdout/stderr streamed as work executes.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AppUpdate {
    ResourceParams {
        resource_params: ViewTree,
    },

    ResourcesStart,
    ResourcesNode {
        index: usize,
        tree: ViewTree,
    },
    ResourcesComplete,

    ResourceStatesStart,
    ResourceStatesNodeStart {
        index: usize,
    },
    ResourceStatesNodeComplete {
        index: usize,
        node: View,
    },
    ResourceStatesComplete,

    ResourceChangesStart,
    ResourceChangesNode {
        index: usize,
        node: Option<View>,
    },
    ResourceChangesComplete {
        has_changes: bool,
    },

    OperationsStart,
    OperationsNode {
        index: usize,
        operations: ViewTree,
    },
    OperationsComplete,

    OperationsApplyStart {
        operations: Vec<Vec<View>>,
    },
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
    OperationsApplyComplete,
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

/// TUI state. Each variant carries everything from the prior phases plus the
/// newly-started one. `Done` is the terminal phase — data is frozen and the
/// TUI waits for user exit.
///
/// Note(cc): variants duplicate a growing list of fields rather than
/// composing. Adding a new pipeline stage means touching every downstream
/// arm. Acceptable while there are seven stages; worth revisiting if more
/// are added.
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub enum AppView {
    #[default]
    Start,
    ResourceParams {
        resource_params: FlatViewTree,
    },
    Resources {
        resource_params: FlatViewTree,
        resources: FlatViewTree,
    },
    ResourceStates {
        resource_params: FlatViewTree,
        resources: FlatViewTree,
        resource_states: FlatViewTree,
    },
    ResourceChanges {
        resource_params: FlatViewTree,
        resources: FlatViewTree,
        resource_states: FlatViewTree,
        resource_changes: FlatViewTree,
        has_changes: Option<bool>,
    },
    Operations {
        resource_params: FlatViewTree,
        resources: FlatViewTree,
        resource_states: FlatViewTree,
        resource_changes: FlatViewTree,
        has_changes: Option<bool>,
        operations_tree: FlatViewTree,
    },
    OperationsApply {
        resource_params: FlatViewTree,
        resources: FlatViewTree,
        resource_states: FlatViewTree,
        resource_changes: FlatViewTree,
        has_changes: Option<bool>,
        operations_tree: FlatViewTree,
        operations_epochs: Vec<Vec<OperationView>>,
    },
    Done {
        resource_params: FlatViewTree,
        resources: FlatViewTree,
        resource_states: FlatViewTree,
        resource_changes: FlatViewTree,
        has_changes: Option<bool>,
        operations_tree: FlatViewTree,
        operations_epochs: Vec<Vec<OperationView>>,
    },
}

#[derive(Debug, Error)]
pub enum AppViewError {
    #[error("invalid transition: {from} -> {update}")]
    InvalidTransition { from: String, update: String },

    #[error(transparent)]
    FlatTree(#[from] FlatViewTreeError),

    #[error("operation index out of bounds: epoch={0}, op={1}")]
    OperationIndexOutOfBounds(usize, usize),
}

impl AppView {
    /// Fold one [`AppUpdate`] into the view. Consumes `self` to prevent
    /// transient invalid states from being observed; any unexpected
    /// (phase, update) combination returns [`AppViewError::InvalidTransition`]
    /// and the caller can decide whether to abort or skip.
    pub fn update(self, update: AppUpdate) -> Result<Self, AppViewError> {
        use AppUpdate::*;
        match (self, update) {
            // Phase: Start -> ResourceParams
            (AppView::Start, ResourceParams { resource_params }) => Ok(AppView::ResourceParams {
                resource_params: FlatViewTree::from_view_tree_completed(resource_params),
            }),

            // Phase: ResourceParams -> Resources
            (AppView::ResourceParams { resource_params }, ResourcesStart) => {
                let resources = resource_params.template();
                Ok(AppView::Resources {
                    resource_params,
                    resources,
                })
            }

            // Phase: Resources
            (
                AppView::Resources {
                    resource_params,
                    mut resources,
                },
                ResourcesNode { index, tree },
            ) => {
                resources.replace_subtree_completed(index, tree);
                Ok(AppView::Resources {
                    resource_params,
                    resources,
                })
            }
            (
                AppView::Resources {
                    resource_params,
                    resources,
                },
                ResourcesComplete,
            ) => Ok(AppView::Resources {
                resource_params,
                resources,
            }),

            // Phase: Resources -> ResourceStates
            (
                AppView::Resources {
                    resource_params,
                    resources,
                },
                ResourceStatesStart,
            ) => {
                let resource_states = resources.template();
                Ok(AppView::ResourceStates {
                    resource_params,
                    resources,
                    resource_states,
                })
            }

            // Phase: ResourceStates
            (
                AppView::ResourceStates {
                    resource_params,
                    resources,
                    mut resource_states,
                },
                ResourceStatesNodeStart { index },
            ) => {
                resource_states.set_leaf_started(index)?;
                Ok(AppView::ResourceStates {
                    resource_params,
                    resources,
                    resource_states,
                })
            }
            (
                AppView::ResourceStates {
                    resource_params,
                    resources,
                    mut resource_states,
                },
                ResourceStatesNodeComplete { index, node },
            ) => {
                resource_states.set_leaf_view(index, ViewNode::Complete(node))?;
                Ok(AppView::ResourceStates {
                    resource_params,
                    resources,
                    resource_states,
                })
            }
            (
                AppView::ResourceStates {
                    resource_params,
                    resources,
                    resource_states,
                },
                ResourceStatesComplete,
            ) => Ok(AppView::ResourceStates {
                resource_params,
                resources,
                resource_states,
            }),

            // Phase: ResourceStates -> ResourceChanges
            (
                AppView::ResourceStates {
                    resource_params,
                    resources,
                    resource_states,
                },
                ResourceChangesStart,
            ) => {
                let template = resource_states.template();
                Ok(AppView::ResourceChanges {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes: template,
                    has_changes: None,
                })
            }

            // Phase: ResourceChanges
            (
                AppView::ResourceChanges {
                    resource_params,
                    resources,
                    resource_states,
                    mut resource_changes,
                    has_changes,
                },
                ResourceChangesNode { index, node },
            ) => {
                match node {
                    Some(view) => {
                        resource_changes.set_leaf_view(index, ViewNode::Complete(view))?
                    }
                    None => resource_changes.set_node_none(index),
                }
                Ok(AppView::ResourceChanges {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes,
                    has_changes,
                })
            }
            (
                AppView::ResourceChanges {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes,
                    has_changes: _,
                },
                ResourceChangesComplete { has_changes },
            ) => Ok(AppView::ResourceChanges {
                resource_params,
                resources,
                resource_states,
                resource_changes,
                has_changes: Some(has_changes),
            }),

            // Phase: ResourceChanges -> Operations
            (
                AppView::ResourceChanges {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes,
                    has_changes,
                },
                OperationsStart,
            ) => {
                let template = resource_changes.template();
                Ok(AppView::Operations {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes,
                    has_changes,
                    operations_tree: template,
                })
            }

            // Phase: Operations
            (
                AppView::Operations {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes,
                    has_changes,
                    mut operations_tree,
                },
                OperationsNode { index, operations },
            ) => {
                operations_tree.replace_subtree_completed(index, operations);
                Ok(AppView::Operations {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes,
                    has_changes,
                    operations_tree,
                })
            }
            (
                AppView::Operations {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes,
                    has_changes,
                    operations_tree,
                },
                OperationsComplete,
            ) => Ok(AppView::Operations {
                resource_params,
                resources,
                resource_states,
                resource_changes,
                has_changes,
                operations_tree,
            }),

            // Phase: Operations -> OperationsApply
            (
                AppView::Operations {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes,
                    has_changes,
                    operations_tree,
                },
                OperationsApplyStart { operations },
            ) => {
                let epochs = operations
                    .into_iter()
                    .map(|epoch| epoch.into_iter().map(OperationView::new).collect())
                    .collect::<Vec<Vec<OperationView>>>();
                Ok(AppView::OperationsApply {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes,
                    has_changes,
                    operations_tree,
                    operations_epochs: epochs,
                })
            }

            // Phase: OperationsApply (live IO)
            (
                AppView::OperationsApply {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes,
                    has_changes,
                    operations_tree,
                    mut operations_epochs,
                },
                OperationApplyStart { index: (e, o) },
            ) => {
                let epoch = operations_epochs
                    .get_mut(e)
                    .ok_or(AppViewError::OperationIndexOutOfBounds(e, o))?;
                let op = epoch
                    .get_mut(o)
                    .ok_or(AppViewError::OperationIndexOutOfBounds(e, o))?;
                op.stdout.clear();
                op.stderr.clear();
                op.is_complete = false;
                Ok(AppView::OperationsApply {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes,
                    has_changes,
                    operations_tree,
                    operations_epochs,
                })
            }
            (
                AppView::OperationsApply {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes,
                    has_changes,
                    operations_tree,
                    mut operations_epochs,
                },
                OperationApplyStdout {
                    index: (e, o),
                    stdout,
                },
            ) => {
                let epoch = operations_epochs
                    .get_mut(e)
                    .ok_or(AppViewError::OperationIndexOutOfBounds(e, o))?;
                let op = epoch
                    .get_mut(o)
                    .ok_or(AppViewError::OperationIndexOutOfBounds(e, o))?;
                op.stdout.push_str(&stdout);
                op.stdout.push('\n');
                Ok(AppView::OperationsApply {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes,
                    has_changes,
                    operations_tree,
                    operations_epochs,
                })
            }
            (
                AppView::OperationsApply {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes,
                    has_changes,
                    operations_tree,
                    mut operations_epochs,
                },
                OperationApplyStderr {
                    index: (e, o),
                    stderr,
                },
            ) => {
                let epoch = operations_epochs
                    .get_mut(e)
                    .ok_or(AppViewError::OperationIndexOutOfBounds(e, o))?;
                let op = epoch
                    .get_mut(o)
                    .ok_or(AppViewError::OperationIndexOutOfBounds(e, o))?;
                op.stderr.push_str(&stderr);
                // TODO(cc): this pushes '\n' to stdout, not stderr — almost
                // certainly a copy-paste bug (see the matching stdout arm
                // above). Effect: stderr has no line breaks, stdout gains
                // spurious blank lines whenever stderr arrives.
                op.stdout.push('\n');
                Ok(AppView::OperationsApply {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes,
                    has_changes,
                    operations_tree,
                    operations_epochs,
                })
            }
            (
                AppView::OperationsApply {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes,
                    has_changes,
                    operations_tree,
                    mut operations_epochs,
                },
                OperationApplyComplete {
                    index: (e, o),
                    error,
                },
            ) => {
                let epoch = operations_epochs
                    .get_mut(e)
                    .ok_or(AppViewError::OperationIndexOutOfBounds(e, o))?;
                let op = epoch
                    .get_mut(o)
                    .ok_or(AppViewError::OperationIndexOutOfBounds(e, o))?;
                op.is_complete = true;
                op.error = error;
                Ok(AppView::OperationsApply {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes,
                    has_changes,
                    operations_tree,
                    operations_epochs,
                })
            }
            (
                AppView::OperationsApply {
                    resource_params,
                    resources,
                    resource_states,
                    resource_changes,
                    has_changes,
                    operations_tree,
                    operations_epochs,
                },
                OperationsApplyComplete,
            ) => Ok(AppView::Done {
                resource_params,
                resources,
                resource_states,
                resource_changes,
                has_changes,
                operations_tree,
                operations_epochs,
            }),

            (state, update) => Err(AppViewError::InvalidTransition {
                from: format!("{state:?}"),
                update: format!("{update:?}"),
            }),
        }
    }

    pub fn resource_params(&self) -> Option<&FlatViewTree> {
        match self {
            Self::Start => None,
            Self::ResourceParams { resource_params }
            | Self::Resources {
                resource_params, ..
            }
            | Self::ResourceStates {
                resource_params, ..
            }
            | Self::ResourceChanges {
                resource_params, ..
            }
            | Self::Operations {
                resource_params, ..
            }
            | Self::OperationsApply {
                resource_params, ..
            }
            | Self::Done {
                resource_params, ..
            } => Some(resource_params),
        }
    }

    pub fn resources(&self) -> Option<&FlatViewTree> {
        match self {
            Self::Start | Self::ResourceParams { .. } => None,
            Self::Resources { resources, .. }
            | Self::ResourceStates { resources, .. }
            | Self::ResourceChanges { resources, .. }
            | Self::Operations { resources, .. }
            | Self::OperationsApply { resources, .. }
            | Self::Done { resources, .. } => Some(resources),
        }
    }

    pub fn resource_states(&self) -> Option<&FlatViewTree> {
        match self {
            AppView::Start | AppView::ResourceParams { .. } | AppView::Resources { .. } => None,
            AppView::ResourceStates {
                resource_states, ..
            }
            | AppView::ResourceChanges {
                resource_states, ..
            }
            | AppView::Operations {
                resource_states, ..
            }
            | AppView::OperationsApply {
                resource_states, ..
            }
            | AppView::Done {
                resource_states, ..
            } => Some(resource_states),
        }
    }

    pub fn resource_changes(&self) -> Option<&FlatViewTree> {
        match self {
            AppView::Start
            | AppView::ResourceParams { .. }
            | AppView::Resources { .. }
            | AppView::ResourceStates { .. } => None,
            AppView::ResourceChanges {
                resource_changes, ..
            }
            | AppView::Operations {
                resource_changes, ..
            }
            | AppView::OperationsApply {
                resource_changes, ..
            }
            | AppView::Done {
                resource_changes, ..
            } => Some(resource_changes),
        }
    }

    pub fn operations_tree(&self) -> Option<&FlatViewTree> {
        match self {
            AppView::Start
            | AppView::ResourceParams { .. }
            | AppView::Resources { .. }
            | AppView::ResourceStates { .. }
            | AppView::ResourceChanges { .. } => None,
            AppView::Operations {
                operations_tree, ..
            }
            | AppView::OperationsApply {
                operations_tree, ..
            }
            | AppView::Done {
                operations_tree, ..
            } => Some(operations_tree),
        }
    }

    pub fn operations_epochs(&self) -> Option<&Vec<Vec<OperationView>>> {
        match self {
            AppView::Start
            | AppView::ResourceParams { .. }
            | AppView::Resources { .. }
            | AppView::ResourceStates { .. }
            | AppView::ResourceChanges { .. }
            | AppView::Operations { .. } => None,
            AppView::OperationsApply {
                operations_epochs, ..
            } => Some(operations_epochs),
            AppView::Done {
                operations_epochs, ..
            } => Some(operations_epochs),
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
