//! Tree aliases and helpers for planned trees.

use cuid2::create_id;
use lusid_causality::CausalityMeta;
use lusid_operation::Operation;
use lusid_resource::Resource;
use lusid_tree::{FlatTree, FlatTreeNode, Tree};
use lusid_view::{Render, View, ViewTree};

use crate::PlanNodeId;

/// A nested planned tree. Branch/leaf metadata carries [`PlanNodeId`] identifiers
/// for dependency scheduling, plus any `on_change` handler operations parsed at
/// plan-time and waiting to be grafted into the atom tree by [`inject_handlers`].
pub type PlanTree<Node> = Tree<Node, PlanMeta>;

/// Plan-side metadata: causality fields plus install-hook handlers.
///
/// Handlers live here (not on `CausalityMeta`) because they're plan-layer
/// concepts. They flow alongside resource params from plan-load through atom
/// expansion; [`inject_handlers`] is the post-pass that rewrites the tree so
/// each plan item's handlers appear as their own atom leaves, gated by the
/// anchor branch's causality id.
///
/// Invariant: every `map_tree` call that turns a `PlanTree` leaf into a branch
/// must pass `meta` straight through to the produced branch - otherwise
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

/// One leaf in the augmented atom tree built by [`inject_handlers`].
///
/// `Resource` is a real resource atom; the apply pipeline probes its state,
/// computes a change, and (if any) emits operations.
///
/// `Handler` is a parsed `on_change` operation, materialised as its own leaf
/// so causality scheduling can place it in a strictly-later resource epoch
/// than the atoms it watches. It fires only if at least one of those atoms
/// resolved to a change in the current apply.
#[derive(Debug, Clone)]
pub enum AtomNode {
    Resource {
        resource: Resource,
        /// Anchor branch ids whose handlers fire when this atom changes. An
        /// atom may be under several nested anchors (outer plan-item with
        /// handlers wrapping an inner plan-item with its own handlers); all
        /// of them get notified.
        anchor_ids: Vec<PlanNodeId>,
    },
    Handler {
        operation: Operation,
        /// Synthetic anchor id. The pipeline fires this handler iff the
        /// `anchors_changed` set built during epoch processing contains this
        /// id by the time the handler's epoch is reached.
        anchor_id: PlanNodeId,
    },
}

impl Render for AtomNode {
    fn render(&self) -> View {
        match self {
            AtomNode::Resource { resource, .. } => resource.render(),
            AtomNode::Handler { operation, .. } => operation.render(),
        }
    }
}

/// Branch-level post-pass that grafts `on_change` handlers into the atom tree.
///
/// For each `PlanTree::Branch` carrying handlers, the branch's children are
/// wrapped in this shape:
///
/// ```text
/// Branch (outer, meta with handlers cleared) {
///   Branch (anchor, id = SubItem(fresh_scope, "@@handler-anchor")) {
///     <original children, recursively transformed>
///   },
///   Leaf (AtomNode::Handler, requires = [anchor_id]),
///   ... (one leaf per handler operation)
/// }
/// ```
///
/// The anchor branch carries an id; per causality's branch-as-group semantics,
/// any leaf requiring that id transitively waits for every leaf inside the
/// anchor. So every handler leaf runs in a strictly-later resource epoch than
/// every resource-side atom it watches.
///
/// The outer branch retains the plan item's original `id`/`requires`/
/// `required_by`, so dependents declaring `requires: [this-id]` still wait
/// for the resource AND its handlers (handler leaves are descendants of the
/// outer branch and therefore also covered by its id).
///
/// Unlike the previous design, the wrap is unconditional: at this point in
/// the pipeline we don't yet know which atoms will resolve to a change.
/// Conditional firing is the apply pipeline's job: it tracks `anchor_ids` on
/// each resource atom and decides each handler atom's fate when its epoch
/// arrives.
pub fn inject_handlers(tree: PlanTree<Resource>) -> PlanTree<AtomNode> {
    let mut active_anchors: Vec<PlanNodeId> = Vec::new();
    inject_recursive(tree, &mut active_anchors)
}

fn inject_recursive(
    tree: PlanTree<Resource>,
    active_anchors: &mut Vec<PlanNodeId>,
) -> PlanTree<AtomNode> {
    match tree {
        Tree::Leaf { meta, node } => Tree::Leaf {
            meta,
            node: AtomNode::Resource {
                resource: node,
                anchor_ids: active_anchors.clone(),
            },
        },
        Tree::Branch { meta, children } => {
            let PlanMeta {
                id,
                requires,
                required_by,
                handlers,
            } = meta;

            if handlers.is_empty() {
                let new_children: Vec<_> = children
                    .into_iter()
                    .map(|c| inject_recursive(c, active_anchors))
                    .collect();
                return Tree::Branch {
                    meta: PlanMeta {
                        id,
                        requires,
                        required_by,
                        handlers,
                    },
                    children: new_children,
                };
            }

            let anchor_id = PlanNodeId::SubItem {
                scope_id: create_id(),
                item_id: HANDLER_ANCHOR.to_string(),
            };

            active_anchors.push(anchor_id.clone());
            let new_children: Vec<_> = children
                .into_iter()
                .map(|c| inject_recursive(c, active_anchors))
                .collect();
            active_anchors.pop();

            let anchor_branch = Tree::Branch {
                meta: PlanMeta {
                    id: Some(anchor_id.clone()),
                    ..PlanMeta::default()
                },
                children: new_children,
            };

            let handler_leaves: Vec<_> = handlers
                .into_iter()
                .map(|op| Tree::Leaf {
                    meta: PlanMeta {
                        requires: vec![anchor_id.clone()],
                        ..PlanMeta::default()
                    },
                    node: AtomNode::Handler {
                        operation: op,
                        anchor_id: anchor_id.clone(),
                    },
                })
                .collect();

            let mut all_children = Vec::with_capacity(1 + handler_leaves.len());
            all_children.push(anchor_branch);
            all_children.extend(handler_leaves);

            Tree::Branch {
                meta: PlanMeta {
                    id,
                    requires,
                    required_by,
                    handlers: Vec::new(),
                },
                children: all_children,
            }
        }
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
    use lusid_operation::operations::file::FilePath;
    use lusid_resource::file::FileResource;
    use std::path::PathBuf;

    fn handler_op() -> Operation {
        Operation::Command(CommandOperation {
            command: "true".to_string(),
            executor: CommandExecutor::Shell,
        })
    }

    fn resource_leaf() -> PlanTree<Resource> {
        Tree::Leaf {
            meta: PlanMeta::default(),
            node: Resource::File(FileResource::Present {
                path: FilePath::new("/tmp/x"),
            }),
        }
    }

    fn plan_item_id(item: &str) -> PlanNodeId {
        PlanNodeId::PlanItem {
            plan_id: PlanId::Path(PathBuf::from("test.lusid")),
            item_id: item.to_string(),
        }
    }

    #[test]
    fn no_handlers_passes_through_with_empty_anchor_ids() {
        let tree = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(plan_item_id("a")),
                ..PlanMeta::default()
            },
            children: vec![resource_leaf()],
        };
        let result = inject_handlers(tree);
        let Tree::Branch { meta, children } = result else {
            panic!("expected branch");
        };
        assert!(meta.handlers.is_empty());
        assert_eq!(children.len(), 1);
        let Tree::Leaf { node, .. } = &children[0] else {
            panic!("expected leaf");
        };
        match node {
            AtomNode::Resource { anchor_ids, .. } => assert!(anchor_ids.is_empty()),
            _ => panic!("expected Resource leaf"),
        }
    }

    #[test]
    fn handlers_wrap_unconditionally_and_stamp_anchor_id_on_atoms() {
        let tree = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(plan_item_id("nginx-conf")),
                handlers: vec![handler_op()],
                ..PlanMeta::default()
            },
            children: vec![resource_leaf(), resource_leaf()],
        };
        let Tree::Branch {
            meta: outer_meta,
            children: outer_children,
        } = inject_handlers(tree)
        else {
            panic!("expected outer branch");
        };

        assert!(outer_meta.handlers.is_empty(), "outer handlers cleared");
        assert!(
            matches!(&outer_meta.id, Some(PlanNodeId::PlanItem { item_id, .. }) if item_id == "nginx-conf"),
            "outer branch retains plan-item id"
        );
        assert_eq!(outer_children.len(), 2, "anchor branch + 1 handler leaf");

        let Tree::Branch {
            meta: anchor_meta,
            children: anchor_children,
        } = &outer_children[0]
        else {
            panic!("expected anchor branch as first child");
        };
        let anchor_id = anchor_meta.id.clone().expect("anchor has id");
        assert!(
            matches!(&anchor_id, PlanNodeId::SubItem { item_id, .. } if item_id == HANDLER_ANCHOR),
            "anchor branch carries the handler-anchor sub-id",
        );
        assert_eq!(anchor_children.len(), 2, "anchor wraps original children");

        // Each Resource atom inside the anchor carries the anchor id.
        for child in anchor_children {
            let Tree::Leaf { node, .. } = child else {
                panic!("expected resource leaf in anchor");
            };
            match node {
                AtomNode::Resource { anchor_ids, .. } => {
                    assert_eq!(anchor_ids.len(), 1);
                    assert_eq!(anchor_ids[0], anchor_id);
                }
                _ => panic!("expected Resource"),
            }
        }

        let Tree::Leaf {
            meta: handler_meta,
            node: handler_node,
        } = &outer_children[1]
        else {
            panic!("expected handler leaf as second child");
        };
        assert_eq!(handler_meta.requires, vec![anchor_id.clone()]);
        match handler_node {
            AtomNode::Handler {
                anchor_id: handler_anchor_id,
                ..
            } => {
                assert_eq!(handler_anchor_id, &anchor_id);
            }
            _ => panic!("expected Handler"),
        }
    }

    #[test]
    fn nested_anchors_stack_anchor_ids_on_innermost_atoms() {
        let inner = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(plan_item_id("inner")),
                handlers: vec![handler_op()],
                ..PlanMeta::default()
            },
            children: vec![resource_leaf()],
        };
        let outer = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(plan_item_id("outer")),
                handlers: vec![handler_op()],
                ..PlanMeta::default()
            },
            children: vec![inner],
        };
        let Tree::Branch {
            children: outer_children,
            ..
        } = inject_handlers(outer)
        else {
            panic!("expected outer branch");
        };
        let Tree::Branch {
            meta: outer_anchor_meta,
            children: outer_anchor_children,
        } = &outer_children[0]
        else {
            panic!("expected outer anchor branch");
        };
        let outer_anchor_id = outer_anchor_meta.id.clone().expect("outer anchor id");

        // Inside the outer anchor: the inner plan-item branch (still wrapped).
        let Tree::Branch {
            children: inner_outer_children,
            ..
        } = &outer_anchor_children[0]
        else {
            panic!("expected inner plan-item branch under outer anchor");
        };
        let Tree::Branch {
            meta: inner_anchor_meta,
            children: inner_anchor_children,
        } = &inner_outer_children[0]
        else {
            panic!("expected inner anchor branch");
        };
        let inner_anchor_id = inner_anchor_meta.id.clone().expect("inner anchor id");

        let Tree::Leaf { node, .. } = &inner_anchor_children[0] else {
            panic!("expected resource leaf");
        };
        match node {
            AtomNode::Resource { anchor_ids, .. } => {
                assert_eq!(anchor_ids.len(), 2, "leaf is under both anchors");
                assert!(anchor_ids.contains(&outer_anchor_id));
                assert!(anchor_ids.contains(&inner_anchor_id));
            }
            _ => panic!("expected Resource"),
        }
    }

    #[test]
    fn handler_branch_nested_under_handler_free_branch_passes_through_outer() {
        // Outer plan-item branch has no handlers; inner has handlers and a
        // resource leaf. Wrap should happen on the inner branch only.
        let inner = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(plan_item_id("inner")),
                handlers: vec![handler_op()],
                ..PlanMeta::default()
            },
            children: vec![resource_leaf()],
        };
        let outer = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(plan_item_id("outer")),
                ..PlanMeta::default()
            },
            children: vec![inner],
        };
        let Tree::Branch {
            meta: outer_meta,
            children: outer_children,
        } = inject_handlers(outer)
        else {
            panic!("expected outer branch");
        };

        // Outer branch is unwrapped: its id and structure pass through, no
        // anchor synthesised at the outer layer.
        assert!(
            matches!(&outer_meta.id, Some(PlanNodeId::PlanItem { item_id, .. }) if item_id == "outer"),
            "outer id retained",
        );
        assert!(outer_meta.handlers.is_empty());
        assert_eq!(outer_children.len(), 1, "outer keeps its single child");

        // Inner branch was wrapped: we should see an anchor + handler leaf.
        let Tree::Branch {
            meta: inner_meta,
            children: inner_children,
        } = &outer_children[0]
        else {
            panic!("expected inner branch under outer");
        };
        assert!(
            matches!(&inner_meta.id, Some(PlanNodeId::PlanItem { item_id, .. }) if item_id == "inner"),
            "inner id retained",
        );
        assert_eq!(
            inner_children.len(),
            2,
            "inner: anchor branch + handler leaf"
        );
        assert!(matches!(&inner_children[0], Tree::Branch { .. }));
        assert!(matches!(
            &inner_children[1],
            Tree::Leaf {
                node: AtomNode::Handler { .. },
                ..
            }
        ));
    }

    #[test]
    fn handler_leaf_lands_in_strictly_later_epoch_than_resource_leaves() {
        // End-to-end: inject_handlers + compute_epochs places the handler
        // operation in an epoch strictly after every resource-side atom under
        // its anchor.
        use lusid_causality::compute_epochs;

        let tree = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(plan_item_id("res")),
                handlers: vec![handler_op()],
                ..PlanMeta::default()
            },
            children: vec![resource_leaf()],
        };
        let injected = inject_handlers(tree);
        let causality = injected.map(Some).map_meta(PlanMeta::to_causality);
        let epochs = compute_epochs(causality).expect("compute_epochs");

        let mut resource_epoch = None;
        let mut handler_epoch = None;
        for (i, epoch) in epochs.iter().enumerate() {
            for atom in epoch {
                match atom {
                    AtomNode::Resource { .. } => resource_epoch = Some(i),
                    AtomNode::Handler { .. } => handler_epoch = Some(i),
                }
            }
        }
        let resource_epoch = resource_epoch.expect("found resource");
        let handler_epoch = handler_epoch.expect("found handler");
        assert!(
            handler_epoch > resource_epoch,
            "handler epoch ({handler_epoch}) must be strictly later than resource epoch ({resource_epoch})",
        );
    }
}
