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
