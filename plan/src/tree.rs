//! Tree aliases and helpers for planned trees.

use cuid2::create_id;
use lusid_causality::CausalityMeta;
use lusid_operation::Operation;
use lusid_tree::{FlatTree, FlatTreeNode, Tree};
use lusid_view::{Render, ViewTree};

use crate::PlanNodeId;

/// A nested planned tree. Branch/leaf metadata carries [`PlanNodeId`] identifiers
/// for dependency scheduling, plus any `on_change` handler operations parsed at
/// plan-time and waiting to be grafted into the operation tree.
pub type PlanTree<Node> = Tree<Node, PlanMeta>;

/// Plan-side metadata: causality fields plus install-hook handlers.
///
/// Handlers live here (not on `CausalityMeta`) because they're plan-layer
/// concepts. The pipeline preserves `handlers` from the originating
/// `@resource/*` leaf all the way through resource → state → change → operations
/// expansion; the branch-level `inject_handlers` pass then grafts them into
/// the operation tree before causality flattening.
///
/// Invariant: every `map_tree` call that turns a `PlanTree` leaf into a branch
/// must pass `meta` straight through to the produced branch — otherwise
/// `handlers` is silently dropped.
#[derive(Debug, Clone, Default)]
pub struct PlanMeta {
    pub id: Option<PlanNodeId>,
    pub requires: Vec<PlanNodeId>,
    pub required_by: Vec<PlanNodeId>,
    pub handlers: Vec<Operation>,
}

impl PlanMeta {
    /// Drop handlers, returning the pure causality view of this meta.
    ///
    /// Used at every site that converts a `PlanTree`/`PlanFlatTree` to a
    /// `CausalityTree`/`CausalityFlatTree`. Kept as an explicit method (no
    /// `From` / `Into`) so the lossy drop is visible at the call site.
    pub fn to_causality(self) -> CausalityMeta<PlanNodeId> {
        CausalityMeta {
            id: self.id,
            requires: self.requires,
            required_by: self.required_by,
        }
    }
}

/// Flat (arena-backed) view of a [`PlanTree`].
pub type PlanFlatTree<Node> = FlatTree<Node, PlanMeta>;
/// A single node in a [`PlanFlatTree`].
pub type PlanFlatTreeNode<Node> = FlatTreeNode<Node, PlanMeta>;

/// Reserved sub-item id prefix.
///
/// Resource atoms must not emit intra-scope ids beginning with `@@` — that
/// namespace is owned by the plan layer for synthetic ids (e.g. the
/// `@@handler-anchor` minted by `inject_handlers`). Enforced by [`map_plan_subitems`]
/// via `debug_assert!`.
pub const RESERVED_SUBITEM_PREFIX: &str = "@@";

/// Expand a node into a set of child trees whose `CausalityMeta<String>` ids (e.g. the
/// `"file"` id emitted by `file` to order mode/user/group atoms) are scoped under a
/// fresh `cuid2` and rewrapped as [`PlanNodeId::SubItem`].
///
/// This is what keeps intra-resource ids unique across the whole plan: every call mints
/// its own `scope_id`, so `"file"` from two different file resources can never collide.
///
/// `debug_assert!`s catch any resource emitting (or referencing) a reserved
/// `@@`-prefixed intra-scope id; that prefix is owned by the plan layer for
/// synthetic ids like `@@handler-anchor`.
pub fn map_plan_subitems<Node, NextNode, MapFn, MapFnIter>(
    node: Node,
    map: MapFn,
) -> impl Iterator<Item = PlanTree<NextNode>>
where
    MapFn: Fn(Node) -> MapFnIter,
    MapFnIter: IntoIterator<Item = Tree<NextNode, CausalityMeta<String>>>,
{
    let scope_id = create_id();
    map(node).into_iter().map(move |tree| {
        tree.map_meta(|meta| {
            if let Some(ref item_id) = meta.id {
                debug_assert!(
                    !item_id.starts_with(RESERVED_SUBITEM_PREFIX),
                    "resource emitted reserved intra-scope id: {item_id}",
                );
            }
            for r in &meta.requires {
                debug_assert!(
                    !r.starts_with(RESERVED_SUBITEM_PREFIX),
                    "resource emitted reserved intra-scope requires: {r}",
                );
            }
            for r in &meta.required_by {
                debug_assert!(
                    !r.starts_with(RESERVED_SUBITEM_PREFIX),
                    "resource emitted reserved intra-scope required_by: {r}",
                );
            }
            PlanMeta {
                id: meta.id.map(|item_id| PlanNodeId::SubItem {
                    scope_id: scope_id.clone(),
                    item_id,
                }),
                requires: meta
                    .requires
                    .into_iter()
                    .map(|item_id| PlanNodeId::SubItem {
                        scope_id: scope_id.clone(),
                        item_id,
                    })
                    .collect(),
                required_by: meta
                    .required_by
                    .into_iter()
                    .map(|item_id| PlanNodeId::SubItem {
                        scope_id: scope_id.clone(),
                        item_id,
                    })
                    .collect(),
                handlers: Vec::new(),
            }
        })
    })
}

/// Synthetic sub-item id used by [`inject_handlers`] to anchor on_change
/// handlers after every resource-side leaf. Lives under a fresh `scope_id` per
/// plan item, so collision with other plan items is impossible. The `@@`
/// prefix is reserved (enforced in [`map_plan_subitems`]) so no resource atom
/// can shadow it within its own scope.
const HANDLER_ANCHOR: &str = "@@handler-anchor";

/// Branch-level post-pass that grafts `on_change` handlers into the operation
/// tree.
///
/// For each `PlanTree::Branch` whose `meta.handlers` is non-empty AND has any
/// descendant `Some(_)` leaf (i.e. the resource actually had a change), the
/// branch's children are wrapped in this shape:
///
/// ```text
/// Branch (outer, meta with handlers cleared) {
///   Branch (anchor, id = SubItem(fresh_scope, "@@handler-anchor")) {
///     <original resource-side children, unchanged>
///   },
///   Leaf { requires = [anchor_id], node = Some(handler_op) },
///   ... (one leaf per handler)
/// }
/// ```
///
/// The anchor branch carries an id; per causality's branch-as-group semantics,
/// any leaf requiring that id transitively waits for every leaf inside the
/// anchor. So every handler leaf runs strictly after every resource-side op.
///
/// The outer branch retains the plan item's original `id`/`requires`/
/// `required_by`, so any plan item declaring `requires: [this-id]` still waits
/// for the resource AND its handlers (handler leaves are descendants of the
/// outer branch and therefore also covered by its id).
///
/// Branches with empty `handlers`, or with handlers but no descendant change,
/// pass through unchanged.
pub fn inject_handlers(
    tree: PlanTree<Option<Operation>>,
) -> PlanTree<Option<Operation>> {
    match tree {
        Tree::Leaf { meta, node } => Tree::Leaf { meta, node },
        Tree::Branch { meta, children } => {
            // Recurse first so nested plan-item branches get their own wrap.
            let children: Vec<_> = children.into_iter().map(inject_handlers).collect();

            if meta.handlers.is_empty() || !has_any_change(&children) {
                return Tree::Branch { meta, children };
            }
            wrap_with_handler_structure(meta, children)
        }
    }
}

fn has_any_change(children: &[PlanTree<Option<Operation>>]) -> bool {
    children.iter().any(|t| match t {
        Tree::Leaf { node, .. } => node.is_some(),
        Tree::Branch { children, .. } => has_any_change(children),
    })
}

fn wrap_with_handler_structure(
    branch_meta: PlanMeta,
    resource_children: Vec<PlanTree<Option<Operation>>>,
) -> PlanTree<Option<Operation>> {
    let scope_id = create_id();
    let anchor_id = PlanNodeId::SubItem {
        scope_id,
        item_id: HANDLER_ANCHOR.to_string(),
    };

    // Wrap the original children inside an anchor branch with its own id.
    // Requiring this id == requiring every leaf inside it (branch-as-group).
    let anchor_branch = Tree::Branch {
        meta: PlanMeta {
            id: Some(anchor_id.clone()),
            ..PlanMeta::default()
        },
        children: resource_children,
    };

    // Convert each handler op into a leaf that requires the anchor.
    let handler_leaves: Vec<_> = branch_meta
        .handlers
        .iter()
        .cloned()
        .map(|op| Tree::Leaf {
            meta: PlanMeta {
                requires: vec![anchor_id.clone()],
                ..PlanMeta::default()
            },
            node: Some(op),
        })
        .collect();

    // Outer branch keeps the original causality fields but drops handlers
    // (defensive: re-running inject_handlers must be a no-op).
    let outer_meta = PlanMeta {
        handlers: Vec::new(),
        ..branch_meta
    };
    let mut all_children = Vec::with_capacity(1 + handler_leaves.len());
    all_children.push(anchor_branch);
    all_children.extend(handler_leaves);
    Tree::Branch {
        meta: outer_meta,
        children: all_children,
    }
}

/// Convert a [`PlanTree`] into a [`ViewTree`] for TUI display. Branch labels use the
/// branch's `PlanNodeId` (rendered) or `.` if the branch is anonymous.
pub fn render_plan_tree<Node>(tree: PlanTree<Node>) -> ViewTree
where
    Node: Render,
{
    match tree {
        Tree::Branch { meta, children } => ViewTree::Branch {
            view: meta.id.map(|id| id.render()).unwrap_or(".".render()),
            children: children.into_iter().map(render_plan_tree).collect(),
        },
        Tree::Leaf { meta: _, node } => ViewTree::Leaf {
            view: node.render(),
        },
    }
}
