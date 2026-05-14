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

/// Expand a node into a set of child trees whose `CausalityMeta<String>` ids (e.g. the
/// `"file"` id emitted by `file` to order mode/user/group atoms) are scoped under a
/// fresh `cuid2` and rewrapped as [`PlanNodeId::SubItem`].
///
/// This is what keeps intra-resource ids unique across the whole plan: every call mints
/// its own `scope_id`, so `"file"` from two different file resources can never collide.
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
        tree.map_meta(|meta| PlanMeta {
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
        })
    })
}

/// Synthetic sub-item id used by [`inject_handlers`] to anchor on_change
/// handlers after every resource-side leaf. Each call to [`inject_handlers`]
/// mints a fresh `scope_id` for the anchor (via [`cuid2`]), so this id can't
/// collide with the resource's own atom ids (which live under a different
/// scope minted by [`map_plan_subitems`]).
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

    // Move handlers out of branch_meta to build handler_leaves below. The
    // outer branch's handlers field must be present in the struct literal,
    // so we re-build PlanMeta with the original causality fields.
    let PlanMeta {
        id,
        requires,
        required_by,
        handlers,
    } = branch_meta;
    let outer_meta = PlanMeta {
        id,
        requires,
        required_by,
        handlers: Vec::new(),
    };

    // Convert each handler op into a leaf that requires the anchor. Consume
    // `handlers` (no `.iter().cloned()`) — outer_meta already dropped them.
    let handler_leaves: Vec<_> = handlers
        .into_iter()
        .map(|op| Tree::Leaf {
            meta: PlanMeta {
                requires: vec![anchor_id.clone()],
                ..PlanMeta::default()
            },
            node: Some(op),
        })
        .collect();
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::PlanId;
    use lusid_operation::operations::command::{CommandExecutor, CommandOperation};
    use std::path::PathBuf;

    fn handler_op() -> Operation {
        Operation::Command(CommandOperation {
            command: "true".to_string(),
            executor: CommandExecutor::Shell,
        })
    }

    fn some_leaf() -> PlanTree<Option<Operation>> {
        Tree::Leaf {
            meta: PlanMeta::default(),
            node: Some(handler_op()),
        }
    }

    fn none_leaf() -> PlanTree<Option<Operation>> {
        Tree::Leaf {
            meta: PlanMeta::default(),
            node: None,
        }
    }

    fn plan_item_id(item: &str) -> PlanNodeId {
        PlanNodeId::PlanItem {
            plan_id: PlanId::Path(PathBuf::from("test.lusid")),
            item_id: item.to_string(),
        }
    }

    #[test]
    fn no_handlers_passes_through() {
        let tree = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(plan_item_id("a")),
                ..PlanMeta::default()
            },
            children: vec![some_leaf()],
        };
        let result = inject_handlers(tree);
        // Same structure: one branch with one leaf child.
        let Tree::Branch { children, .. } = result else {
            panic!("expected branch");
        };
        assert_eq!(children.len(), 1);
        assert!(matches!(children[0], Tree::Leaf { .. }));
    }

    #[test]
    fn handlers_with_descendant_change_wrap() {
        let tree = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(plan_item_id("nginx-conf")),
                handlers: vec![handler_op()],
                ..PlanMeta::default()
            },
            children: vec![some_leaf(), none_leaf()],
        };
        let Tree::Branch {
            meta: outer_meta,
            children: outer_children,
        } = inject_handlers(tree)
        else {
            panic!("expected outer branch");
        };

        // Outer branch keeps the original plan-item id but drops handlers.
        assert!(outer_meta.handlers.is_empty(), "outer handlers cleared");
        assert!(
            matches!(&outer_meta.id, Some(PlanNodeId::PlanItem { item_id, .. }) if item_id == "nginx-conf"),
            "outer branch retains plan-item id"
        );
        assert_eq!(outer_children.len(), 2, "anchor branch + 1 handler leaf");

        // First child: anchor branch with the SubItem(_, @@handler-anchor) id.
        let Tree::Branch {
            meta: anchor_meta,
            children: anchor_children,
        } = &outer_children[0]
        else {
            panic!("expected anchor branch as first child");
        };
        assert!(
            matches!(&anchor_meta.id, Some(PlanNodeId::SubItem { item_id, .. }) if item_id == HANDLER_ANCHOR),
            "anchor branch carries the handler-anchor sub-id",
        );
        assert_eq!(anchor_children.len(), 2, "anchor wraps original children");

        // Second child: handler leaf with requires=[anchor_id], node=Some(_).
        let Tree::Leaf {
            meta: handler_meta,
            node: handler_node,
        } = &outer_children[1]
        else {
            panic!("expected handler leaf as second child");
        };
        assert!(handler_node.is_some());
        assert_eq!(handler_meta.requires.len(), 1);
        assert!(
            matches!(&handler_meta.requires[0], PlanNodeId::SubItem { item_id, .. } if item_id == HANDLER_ANCHOR),
            "handler requires the anchor",
        );
    }

    #[test]
    fn handlers_without_change_skip_wrap() {
        // Resource is converged (all leaves None). Even with handlers, no wrap.
        let tree = PlanTree::Branch {
            meta: PlanMeta {
                handlers: vec![handler_op()],
                ..PlanMeta::default()
            },
            children: vec![none_leaf(), none_leaf()],
        };
        let Tree::Branch { children, .. } = inject_handlers(tree) else {
            panic!("expected branch");
        };
        assert_eq!(children.len(), 2, "still the original 2 leaves, no anchor");
        for child in &children {
            assert!(matches!(child, Tree::Leaf { node: None, .. }));
        }
    }

    #[test]
    fn nested_branches_each_wrap_independently() {
        // Outer plan item has no handlers; inner plan item has handlers and a
        // change. Only the inner branch should wrap.
        let inner = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(plan_item_id("inner")),
                handlers: vec![handler_op()],
                ..PlanMeta::default()
            },
            children: vec![some_leaf()],
        };
        let outer = PlanTree::Branch {
            meta: PlanMeta::default(),
            children: vec![inner],
        };
        let Tree::Branch {
            children: outer_children,
            ..
        } = inject_handlers(outer)
        else {
            panic!("expected outer branch");
        };
        assert_eq!(outer_children.len(), 1);
        let Tree::Branch {
            meta: inner_meta,
            children: inner_children,
        } = &outer_children[0]
        else {
            panic!("expected inner branch after inject");
        };
        // Inner branch was wrapped: handlers cleared, 1 anchor + 1 handler leaf.
        assert!(inner_meta.handlers.is_empty());
        assert_eq!(inner_children.len(), 2);
    }

    #[test]
    fn handler_leaf_lands_in_strictly_later_epoch_than_resource_leaves() {
        // End-to-end contract: inject_handlers + compute_epochs must place the
        // handler operation in an epoch strictly after every resource-side
        // operation. This is the load-bearing semantic the doc promises.
        use lusid_causality::compute_epochs;

        let resource_op = Operation::Command(CommandOperation {
            command: "RESOURCE".to_string(),
            executor: CommandExecutor::Shell,
        });
        let handler = Operation::Command(CommandOperation {
            command: "HANDLER".to_string(),
            executor: CommandExecutor::Shell,
        });
        let tree = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(plan_item_id("res")),
                handlers: vec![handler],
                ..PlanMeta::default()
            },
            children: vec![
                Tree::Leaf {
                    meta: PlanMeta::default(),
                    node: Some(resource_op),
                },
            ],
        };
        let injected = inject_handlers(tree);
        let causality = injected.map_meta(PlanMeta::to_causality);
        let epochs = compute_epochs(causality).expect("compute_epochs");

        // Find which epoch each op landed in by inspecting its command string.
        let find = |needle: &str| -> usize {
            for (i, epoch) in epochs.iter().enumerate() {
                for op in epoch {
                    if let Operation::Command(c) = op
                        && c.command == needle
                    {
                        return i;
                    }
                }
            }
            panic!("did not find op {needle:?} in any epoch");
        };
        let resource_epoch = find("RESOURCE");
        let handler_epoch = find("HANDLER");
        assert!(
            handler_epoch > resource_epoch,
            "handler epoch ({handler_epoch}) must be strictly later than resource epoch ({resource_epoch})",
        );
    }

}
