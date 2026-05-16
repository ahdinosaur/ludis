//! Tree aliases and helpers for planned trees.

use cuid2::create_id;
use lusid_causality::CausalityMeta;
use lusid_operation::Operation;
use lusid_tree::{FlatTree, FlatTreeNode, Tree};
use lusid_view::{Render, ViewTree};

use crate::PlanNodeId;

/// A nested planned tree. Branch/leaf metadata carries [`PlanNodeId`] identifiers
/// for dependency scheduling, plus any `on_change` handler operations parsed at
/// plan-time.
pub type PlanTree<Node> = Tree<Node, PlanMeta>;

/// Plan-side metadata: causality fields plus install-hook handlers.
///
/// Handlers live here (not on `CausalityMeta`) because they're plan-layer
/// concepts. They flow alongside resource params from plan-load through atom
/// expansion and are fired by the apply loop directly, keyed by the arena
/// index of the owning plan-item branch.
///
/// Invariant: every `map_tree` call that turns a `PlanTree` leaf into a branch
/// must pass `meta` straight through to the produced branch. The apply loop
/// reads `meta.handlers` from the atoms tree at apply time, so a dropped
/// `handlers` vector silently disables `on_change` for that plan item with no
/// other symptom.
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
    use lusid_causality::compute_epochs;
    use lusid_operation::operations::file::FilePath;
    use lusid_resource::Resource;
    use lusid_resource::file::FileResource;
    use std::path::PathBuf;

    fn file_resource(path: &str) -> Resource {
        Resource::File(FileResource::Present {
            path: FilePath::new(path),
        })
    }

    fn plan_item_id(item: &str) -> PlanNodeId {
        PlanNodeId::PlanItem {
            plan_id: PlanId::Path(PathBuf::from("test.lusid")),
            item_id: item.to_string(),
        }
    }

    /// `requires: [<branch-id>]` puts the requirer in a strictly-later epoch
    /// than every leaf inside the required branch. This is the foundation the
    /// apply layer's Phase A/B ordering relies on.
    #[test]
    fn dependent_lands_strictly_after_plan_item_atoms() {
        let p = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(plan_item_id("p")),
                ..PlanMeta::default()
            },
            children: vec![Tree::Leaf {
                meta: PlanMeta::default(),
                node: file_resource("/tmp/x"),
            }],
        };
        let b = PlanTree::Leaf {
            meta: PlanMeta {
                requires: vec![plan_item_id("p")],
                ..PlanMeta::default()
            },
            node: file_resource("/tmp/b"),
        };
        let root = PlanTree::Branch {
            meta: PlanMeta::default(),
            children: vec![p, b],
        };
        let causality = root.map(Some).map_meta(PlanMeta::to_causality);
        let epochs = compute_epochs(causality).expect("compute_epochs");

        let epoch_of = |needle: &str| -> usize {
            for (i, epoch) in epochs.iter().enumerate() {
                for resource in epoch {
                    if let Resource::File(FileResource::Present { path }) = resource
                        && path.as_path() == std::path::Path::new(needle)
                    {
                        return i;
                    }
                }
            }
            panic!("did not find resource for {needle:?}");
        };
        let p_epoch = epoch_of("/tmp/x");
        let b_epoch = epoch_of("/tmp/b");
        assert!(
            b_epoch > p_epoch,
            "b's atom epoch ({b_epoch}) must be strictly later than p's ({p_epoch})",
        );
    }
}
