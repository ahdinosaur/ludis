use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Debug,
    hash::Hash,
};
use thiserror::Error;

use crate::{CausalityMeta, tree::CausalityTree};

#[derive(Debug, Error)]
pub enum EpochError<NodeId> {
    #[error("Duplicate id: {0}")]
    DuplicateId(NodeId),

    #[error("Unknown id referenced in 'requires': {0}")]
    UnknownRequiresRef(NodeId),

    #[error("Unknown id referenced in 'required_by': {0}")]
    UnknownRequiredByRef(NodeId),

    #[error("Cycle detected in dependency graph (remaining nodes: {remaining})")]
    CycleDetected { remaining: usize },
}

/// Flatten a causality tree into topologically-sorted dependency layers (epochs).
///
/// Uses Kahn's algorithm. Each returned epoch is a `Vec<Node>` whose members have no
/// remaining dependencies within the graph, so they can be applied concurrently.
///
/// # Semantics
///
/// - Branch-level `requires` / `required_by` / `id` flow down to every descendant leaf.
/// - Leaves wrapped in `None` are carried through the dependency graph (for their id
///   references) but dropped from the output - useful as pure "marker" nodes.
/// - Returns an empty epoch-free output if the input tree has no leaves.
///
/// # Errors
///
/// - [`EpochError::DuplicateId`] if any id appears twice across the tree.
/// - [`EpochError::UnknownRequiresRef`] / [`EpochError::UnknownRequiredByRef`] if a
///   `requires` / `required_by` entry references an id that wasn't declared.
/// - [`EpochError::CycleDetected`] if the dependency graph contains a cycle.
pub fn compute_epochs<Node, NodeId>(
    tree: CausalityTree<Option<Node>, NodeId>,
) -> Result<Vec<Vec<Node>>, EpochError<NodeId>>
where
    Node: Debug + Clone,
    NodeId: Debug + Clone + Eq + Hash,
{
    #[derive(Debug)]
    struct CollectedLeaf<Node, NodeId> {
        node: Option<Node>,
        requires: Vec<NodeId>,
        required_by: Vec<NodeId>,
    }

    // Flatten the tree into a list of leaves, each with their *effective* requires/
    // required_by (ancestor branches' constraints merged in). A parallel map
    // `id_to_leaves` records which leaf indices a given id refers to; for branch ids
    // this is the set of all descendant leaves.
    let mut leaves: Vec<CollectedLeaf<Node, NodeId>> = Vec::new();
    let mut id_to_leaves: HashMap<NodeId, Vec<usize>> = HashMap::new();
    let mut seen_ids: HashSet<NodeId> = HashSet::new();

    fn collect_recursive<Node, NodeId>(
        tree: CausalityTree<Option<Node>, NodeId>,
        ancestor_requires: &mut Vec<NodeId>,
        ancestor_required_by: &mut Vec<NodeId>,
        active_branch_ids: &mut Vec<NodeId>,
        seen_ids: &mut HashSet<NodeId>,
        id_to_leaves: &mut HashMap<NodeId, Vec<usize>>,
        leaves: &mut Vec<CollectedLeaf<Node, NodeId>>,
    ) -> Result<(), EpochError<NodeId>>
    where
        NodeId: Clone + Eq + Hash,
    {
        match tree {
            CausalityTree::Branch { children, meta } => {
                let CausalityMeta {
                    id,
                    requires,
                    required_by,
                } = meta;

                let requires_len = ancestor_requires.len();
                ancestor_requires.extend(requires);

                let required_by_len = ancestor_required_by.len();
                ancestor_required_by.extend(required_by);

                let pushed_branch_id = if let Some(branch_id) = id {
                    if !seen_ids.insert(branch_id.clone()) {
                        return Err(EpochError::DuplicateId(branch_id));
                    }
                    id_to_leaves.entry(branch_id.clone()).or_default();
                    active_branch_ids.push(branch_id);
                    true
                } else {
                    false
                };

                for child in children {
                    collect_recursive(
                        child,
                        ancestor_requires,
                        ancestor_required_by,
                        active_branch_ids,
                        seen_ids,
                        id_to_leaves,
                        leaves,
                    )?;
                }

                ancestor_requires.truncate(requires_len);
                ancestor_required_by.truncate(required_by_len);
                if pushed_branch_id {
                    active_branch_ids.pop();
                }
                Ok(())
            }
            CausalityTree::Leaf { node, meta } => {
                let CausalityMeta {
                    id,
                    requires,
                    required_by,
                } = meta;

                let mut effective_requires: Vec<NodeId> = Vec::new();
                effective_requires.extend(ancestor_requires.iter().cloned());
                effective_requires.extend(requires);

                let mut effective_required_by: Vec<NodeId> = Vec::new();
                effective_required_by.extend(ancestor_required_by.iter().cloned());
                effective_required_by.extend(required_by);

                let index = leaves.len();
                leaves.push(CollectedLeaf {
                    node,
                    requires: effective_requires,
                    required_by: effective_required_by,
                });

                for branch_id in active_branch_ids.iter() {
                    if let Some(v) = id_to_leaves.get_mut(branch_id) {
                        v.push(index);
                    }
                }

                if let Some(leaf_id) = id {
                    if !seen_ids.insert(leaf_id.clone()) {
                        return Err(EpochError::DuplicateId(leaf_id));
                    }
                    id_to_leaves.insert(leaf_id, vec![index]);
                }
                Ok(())
            }
        }
    }

    let mut ancestor_requires: Vec<NodeId> = Vec::new();
    let mut ancestor_required_by: Vec<NodeId> = Vec::new();
    let mut active_branch_ids: Vec<NodeId> = Vec::new();

    collect_recursive(
        tree,
        &mut ancestor_requires,
        &mut ancestor_required_by,
        &mut active_branch_ids,
        &mut seen_ids,
        &mut id_to_leaves,
        &mut leaves,
    )?;

    // Build the DAG. Each `requires` edge points from target → this leaf (so this
    // leaf's indegree goes up); each `required_by` edge points from this leaf →
    // target (so target's indegree goes up). Both directions feed Kahn's algorithm.
    let n = leaves.len();
    let mut outgoing: Vec<Vec<usize>> = vec![Vec::new(); n];
    let mut indegree: Vec<usize> = vec![0; n];

    for (i, leaf) in leaves.iter().enumerate() {
        for id in &leaf.requires {
            let Some(targets) = id_to_leaves.get(id) else {
                return Err(EpochError::UnknownRequiresRef(id.clone()));
            };
            for &j in targets {
                outgoing[j].push(i);
                indegree[i] += 1;
            }
        }
        for id in &leaf.required_by {
            let Some(targets) = id_to_leaves.get(id) else {
                return Err(EpochError::UnknownRequiredByRef(id.clone()));
            };
            for &j in targets {
                outgoing[i].push(j);
                indegree[j] += 1;
            }
        }
    }

    let mut queue: VecDeque<usize> = indegree
        .iter()
        .enumerate()
        .filter_map(|(i, &d)| (d == 0).then_some(i))
        .collect();

    let mut seen = 0usize;
    let mut epochs: Vec<Vec<Node>> = Vec::new();
    let mut indegree_mut = indegree;

    while !queue.is_empty() {
        let current_wave: Vec<usize> = queue.drain(..).collect();
        seen += current_wave.len();

        let mut specs: Vec<Node> = Vec::new();
        for i in current_wave.iter().copied() {
            if let Some(node) = leaves[i].node.as_ref() {
                specs.push(node.clone());
            }
        }
        if !specs.is_empty() {
            epochs.push(specs);
        }

        let mut next_wave: Vec<usize> = Vec::new();
        for i in current_wave {
            for &j in &outgoing[i] {
                indegree_mut[j] -= 1;
                if indegree_mut[j] == 0 {
                    next_wave.push(j);
                }
            }
        }
        queue.extend(next_wave);
    }

    if seen != n {
        let remaining = n - seen;
        return Err(EpochError::CycleDetected { remaining });
    }

    Ok(epochs)
}

// Note(cc): branch-level `requires` inflates the edge count - a branch with k leaves
// whose `requires: [X]` resolves to m leaves produces k * m edges. Fine in practice for
// plan-sized inputs, but worth knowing before scaling this to huge trees.
