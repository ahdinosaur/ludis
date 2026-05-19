//! Generic tree data structures: nested [`Tree`] and arena-backed [`FlatTree`].
//! See the crate README for invariants and usage.

use std::future::Future;

use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Recursive nested tree. Either a `Branch` with children or a `Leaf` with a value,
/// each carrying a `Meta` payload.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Tree<Node, Meta> {
    Branch {
        meta: Meta,
        children: Vec<Tree<Node, Meta>>,
    },
    Leaf {
        meta: Meta,
        node: Node,
    },
}

impl<Node, Meta> Tree<Node, Meta> {
    /// Construct a branch node from metadata and an iterable of children.
    pub fn branch(meta: Meta, children: impl IntoIterator<Item = Tree<Node, Meta>>) -> Self {
        Self::Branch {
            meta,
            children: children.into_iter().collect(),
        }
    }

    /// Construct a leaf node from metadata and a value.
    pub fn leaf(meta: Meta, node: Node) -> Self {
        Self::Leaf { meta, node }
    }

    pub fn is_leaf(&self) -> bool {
        matches!(self, Tree::Leaf { .. })
    }

    pub fn is_branch(&self) -> bool {
        matches!(self, Tree::Branch { .. })
    }

    /// A tree is "empty" when it contains no leaves - i.e. every branch contains only
    /// empty branches. A leaf is never empty.
    pub fn is_empty(&self) -> bool {
        match self {
            Tree::Branch { children, .. } => children.iter().all(|child| child.is_empty()),
            Tree::Leaf { .. } => false,
        }
    }

    /// Transform leaf values, preserving structure and metadata.
    pub fn map<NextNode, MapFn>(self, map: MapFn) -> Tree<NextNode, Meta>
    where
        MapFn: Fn(Node) -> NextNode + Copy,
    {
        match self {
            Tree::Branch { meta, children } => Tree::Branch {
                meta,
                children: children
                    .into_iter()
                    .map(|tree| Self::map(tree, map))
                    .collect(),
            },
            Tree::Leaf { meta, node } => Tree::Leaf {
                meta,
                node: map(node),
            },
        }
    }

    /// Transform metadata on all nodes, preserving structure and leaf values.
    pub fn map_meta<NextMeta, MapFn>(self, map: MapFn) -> Tree<Node, NextMeta>
    where
        MapFn: Fn(Meta) -> NextMeta + Copy,
    {
        match self {
            Tree::Branch { meta, children } => Tree::Branch {
                meta: map(meta),
                children: children
                    .into_iter()
                    .map(|tree| Self::map_meta(tree, map))
                    .collect(),
            },
            Tree::Leaf { meta, node } => Tree::Leaf {
                meta: map(meta),
                node,
            },
        }
    }
}

/// A single node in a [`FlatTree`]. Branches store child indices; leaves store values.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FlatTreeNode<Node, Meta> {
    Branch { meta: Meta, children: Vec<usize> },
    Leaf { meta: Meta, node: Node },
}

/// Arena-backed flat tree. Nodes are stored in a `Vec<Option<...>>` where indices serve
/// as node identifiers. `None` slots are tombstones - previously occupied positions that
/// have been cleared (e.g. after a subtree replacement).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlatTree<Node, Meta> {
    nodes: Vec<Option<FlatTreeNode<Node, Meta>>>,
}

#[derive(Debug, Error)]
pub enum FlatTreeError {
    #[error("node at index {0} is None")]
    NodeMissing(usize),

    #[error("index {0} is out of bounds")]
    IndexOutOfBounds(usize),
}

impl<Node, Meta> FlatTree<Node, Meta>
where
    Node: Clone,
    Meta: Clone,
{
    /// Root index is always 0.
    pub const fn root_index() -> usize {
        0
    }

    /// Returns the root node, or `None` if the tree is empty.
    pub fn root(&self) -> Option<&FlatTreeNode<Node, Meta>> {
        self.nodes.first()?.as_ref()
    }

    /// Iterates leaf values in arena order (not traversal order).
    pub fn leaves(&self) -> impl Iterator<Item = &Node> {
        self.nodes.iter().filter_map(|node| match node {
            Some(FlatTreeNode::Branch { .. }) => None,
            Some(FlatTreeNode::Leaf { node, .. }) => Some(node),
            None => None,
        })
    }

    /// Iterates `(arena_index, node)` pairs for every live slot in arena order.
    /// Tombstoned slots are skipped. Useful when downstream needs the arena
    /// index alongside the node (e.g. building a child→parent map).
    pub fn nodes_indexed(&self) -> impl Iterator<Item = (usize, &FlatTreeNode<Node, Meta>)> {
        self.nodes
            .iter()
            .enumerate()
            .filter_map(|(idx, slot)| slot.as_ref().map(|n| (idx, n)))
    }

    pub fn get(&self, index: usize) -> Result<&FlatTreeNode<Node, Meta>, FlatTreeError> {
        let node = self
            .nodes
            .get(index)
            .ok_or(FlatTreeError::IndexOutOfBounds(index))?;
        node.as_ref().ok_or(FlatTreeError::NodeMissing(index))
    }

    pub fn get_mut(
        &mut self,
        index: usize,
    ) -> Result<&mut FlatTreeNode<Node, Meta>, FlatTreeError> {
        let node = self
            .nodes
            .get_mut(index)
            .ok_or(FlatTreeError::IndexOutOfBounds(index))?;
        node.as_mut().ok_or(FlatTreeError::NodeMissing(index))
    }

    /// Append a nested tree to the arena. Returns the index of the tree's root.
    pub fn append_tree(&mut self, tree: Tree<Node, Meta>) -> usize {
        append_tree_nodes(&mut self.nodes, tree)
    }

    /// Transform metadata on every arena node, preserving structure, leaf
    /// values, and indices (including tombstones). Mirrors [`Tree::map_meta`]
    /// so flat-tree consumers don't have to round-trip through nested form
    /// just to swap meta types.
    pub fn map_meta<NextMeta, MapFn>(self, map: MapFn) -> FlatTree<Node, NextMeta>
    where
        MapFn: Fn(Meta) -> NextMeta + Copy,
    {
        let nodes = self
            .nodes
            .into_iter()
            .map(|slot| {
                slot.map(|node| match node {
                    FlatTreeNode::Branch { meta, children } => FlatTreeNode::Branch {
                        meta: map(meta),
                        children,
                    },
                    FlatTreeNode::Leaf { meta, node } => FlatTreeNode::Leaf {
                        meta: map(meta),
                        node,
                    },
                })
            })
            .collect();
        FlatTree { nodes }
    }

    /// Replace the subtree rooted at `root_index`. Existing descendants are cleared
    /// (their slots set to `None`), and the new subtree is installed in place, with
    /// any new children appended to the end of the arena.
    ///
    /// Passing `tree = None` just clears the subtree without installing a replacement.
    pub fn replace_tree(&mut self, tree: Option<Tree<Node, Meta>>, root_index: usize) {
        replace_tree_nodes(&mut self.nodes, tree, root_index)
    }

    /// Depth-first search from the root. Returns indices in post-order
    /// (children before parent). Missing or out-of-bounds children are skipped.
    fn depth_first_search(&self) -> Vec<usize> {
        let mut order = Vec::new();
        if self.root().is_none() {
            return order;
        }

        let mut stack: Vec<(usize, bool)> = Vec::new();
        stack.push((0, false));

        while let Some((index, visited)) = stack.pop() {
            let node = match self.nodes.get(index) {
                Some(Some(node)) => node,
                _ => continue,
            };
            match node {
                FlatTreeNode::Leaf { .. } => order.push(index),
                FlatTreeNode::Branch { children, .. } => {
                    if visited {
                        order.push(index);
                    } else {
                        stack.push((index, true));
                        for &child in children.iter().rev() {
                            if child < self.nodes.len()
                                && self.nodes.get(child).and_then(|n| n.as_ref()).is_some()
                            {
                                stack.push((child, false));
                            }
                        }
                    }
                }
            }
        }

        order
    }
}

impl<Node, Meta> IntoIterator for FlatTree<Node, Meta> {
    type Item = <Vec<Option<FlatTreeNode<Node, Meta>>> as IntoIterator>::Item;
    type IntoIter = <Vec<Option<FlatTreeNode<Node, Meta>>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.nodes.into_iter()
    }
}

impl<Node, Meta> FlatTree<Node, Meta>
where
    Node: Clone,
    Meta: Clone,
{
    /// Transform every leaf synchronously, emitting a `write_update` event per leaf.
    pub async fn map<NextNode, Error, MapFn, WriteUpdateFn, WriteUpdateFut>(
        self,
        map: MapFn,
        write_update: WriteUpdateFn,
    ) -> Result<FlatTree<NextNode, Meta>, Error>
    where
        NextNode: Clone,
        MapFn: Fn(Node) -> NextNode + Copy,
        WriteUpdateFn: Fn(usize, NextNode) -> WriteUpdateFut,
        WriteUpdateFut: Future<Output = Result<(), Error>>,
    {
        let mut next_nodes = vec![None; self.nodes.len()];
        for (index, node) in self.nodes.into_iter().enumerate() {
            match node {
                None => {}
                Some(FlatTreeNode::Branch { meta, children }) => {
                    next_nodes[index] = Some(FlatTreeNode::Branch { meta, children })
                }
                Some(FlatTreeNode::Leaf { meta, node }) => {
                    let next_node = map(node);
                    next_nodes[index] = Some(FlatTreeNode::Leaf {
                        meta,
                        node: next_node.clone(),
                    });
                    write_update(index, next_node).await?;
                }
            }
        }
        Ok(FlatTree { nodes: next_nodes })
    }

    /// Like [`map`](Self::map), but leaves can be dropped by returning `None`.
    /// After mapping, branches that end up with no remaining reachable children are
    /// also dropped (post-order sweep).
    pub async fn map_option<NextNode, Error, MapFn, WriteUpdateFn, WriteUpdateFut>(
        self,
        map: MapFn,
        write_update: WriteUpdateFn,
    ) -> Result<FlatTree<NextNode, Meta>, Error>
    where
        NextNode: Clone,
        MapFn: Fn(Node) -> Option<NextNode> + Copy,
        WriteUpdateFn: Fn(usize, Option<NextNode>) -> WriteUpdateFut,
        WriteUpdateFut: Future<Output = Result<(), Error>>,
    {
        let mut next_nodes = vec![None; self.nodes.len()];
        for (index, node) in self.nodes.into_iter().enumerate() {
            match node {
                None => {}
                Some(FlatTreeNode::Branch { meta, children }) => {
                    next_nodes[index] = Some(FlatTreeNode::Branch { meta, children })
                }
                Some(FlatTreeNode::Leaf { meta, node }) => {
                    let next_node = map(node);
                    next_nodes[index] = next_node
                        .clone()
                        .map(|node| FlatTreeNode::Leaf { meta, node });
                    write_update(index, next_node).await?;
                }
            }
        }

        let mut result = FlatTree { nodes: next_nodes };

        // Drop empty branches (post-order)
        for index in result.depth_first_search() {
            let is_empty_branch = match result.nodes.get(index).and_then(|node| node.as_ref()) {
                Some(FlatTreeNode::Branch { children, .. }) => !children.iter().any(|&child| {
                    child < result.nodes.len()
                        && result
                            .nodes
                            .get(child)
                            .and_then(|node| node.as_ref())
                            .is_some()
                }),
                _ => false,
            };
            if is_empty_branch {
                result.nodes[index] = None;
            }
        }
        Ok(result)
    }

    /// Expand each leaf into a (possibly nested) subtree. The leaf's slot becomes the
    /// new subtree's root, and any extra nodes are appended to the arena.
    pub async fn map_tree<NextNode, Error, MapFn, WriteFut, WriteUpdateFn>(
        self,
        map: MapFn,
        write_update: WriteUpdateFn,
    ) -> Result<FlatTree<NextNode, Meta>, Error>
    where
        NextNode: Clone,
        MapFn: Fn(Node, Meta) -> Tree<NextNode, Meta> + Copy,
        WriteFut: Future<Output = Result<(), Error>>,
        WriteUpdateFn: Fn(usize, Tree<NextNode, Meta>) -> WriteFut,
    {
        let mut next_nodes = vec![None; self.nodes.len()];
        for (index, node) in self.nodes.into_iter().enumerate() {
            match node {
                None => {}
                Some(FlatTreeNode::Branch { meta, children }) => {
                    next_nodes[index] = Some(FlatTreeNode::Branch { meta, children })
                }
                Some(FlatTreeNode::Leaf { meta, node }) => {
                    let next_tree = map(node, meta);
                    replace_tree_nodes(&mut next_nodes, Some(next_tree.clone()), index);
                    write_update(index, next_tree).await?;
                }
            }
        }
        Ok(FlatTree { nodes: next_nodes })
    }

    /// Async, fallible leaf transform with start/update callbacks.
    ///
    /// `write_start` fires before the leaf's `map` future runs (useful for UI: "this
    /// operation is now running"); `write_update` fires after it resolves.
    pub async fn map_result_async<
        NextNode,
        Error,
        MapFn,
        Fut,
        WriteStartFn,
        WriteStartFut,
        WriteUpdateFn,
        WriteUpdateFut,
    >(
        self,
        mut map: MapFn,
        mut write_start: WriteStartFn,
        mut write_update: WriteUpdateFn,
    ) -> Result<FlatTree<NextNode, Meta>, Error>
    where
        NextNode: Clone,
        MapFn: FnMut(Node) -> Fut,
        Fut: Future<Output = Result<NextNode, Error>>,
        WriteStartFn: FnMut(usize) -> WriteStartFut,
        WriteStartFut: Future<Output = Result<(), Error>>,
        WriteUpdateFn: FnMut(usize, NextNode) -> WriteUpdateFut,
        WriteUpdateFut: Future<Output = Result<(), Error>>,
    {
        let mut next_nodes = vec![None; self.nodes.len()];
        for (index, node) in self.nodes.into_iter().enumerate() {
            match node {
                None => {}
                Some(FlatTreeNode::Branch { meta, children }) => {
                    next_nodes[index] = Some(FlatTreeNode::Branch { meta, children })
                }
                Some(FlatTreeNode::Leaf { meta, node }) => {
                    write_start(index).await?;
                    let next_node = map(node).await?;
                    next_nodes[index] = Some(FlatTreeNode::Leaf {
                        meta,
                        node: next_node.clone(),
                    });
                    write_update(index, next_node).await?;
                }
            }
        }
        Ok(FlatTree { nodes: next_nodes })
    }

    /// Async, fallible subtree-expansion variant of [`map_tree`](Self::map_tree).
    /// Each leaf can asynchronously produce a full subtree.
    pub async fn map_tree_result_async<
        NextNode,
        Error,
        MapFn,
        Fut,
        WriteFut,
        WriteStartFn,
        WriteUpdateFn,
    >(
        self,
        map: MapFn,
        write_start: WriteStartFn,
        write_update: WriteUpdateFn,
    ) -> Result<FlatTree<NextNode, Meta>, Error>
    where
        NextNode: Clone,
        MapFn: Fn(Node, Meta) -> Fut + Copy,
        Fut: Future<Output = Result<Tree<NextNode, Meta>, Error>>,
        WriteFut: Future<Output = Result<(), Error>>,
        WriteStartFn: Fn(usize) -> WriteFut,
        WriteUpdateFn: Fn(usize, Tree<NextNode, Meta>) -> WriteFut,
    {
        let mut next_nodes = vec![None; self.nodes.len()];
        for (index, node) in self.nodes.into_iter().enumerate() {
            match node {
                None => {}
                Some(FlatTreeNode::Branch { meta, children }) => {
                    next_nodes[index] = Some(FlatTreeNode::Branch { meta, children })
                }
                Some(FlatTreeNode::Leaf { meta, node }) => {
                    write_start(index).await?;
                    let next_tree = map(node, meta).await?;
                    replace_tree_nodes(&mut next_nodes, Some(next_tree.clone()), index);
                    write_update(index, next_tree).await?;
                }
            }
        }
        Ok(FlatTree { nodes: next_nodes })
    }
}

fn append_tree_nodes<Node, Meta>(
    nodes: &mut Vec<Option<FlatTreeNode<Node, Meta>>>,
    tree: Tree<Node, Meta>,
) -> usize {
    match tree {
        Tree::Leaf { node, meta } => {
            let index = nodes.len();
            nodes.push(Some(FlatTreeNode::Leaf { node, meta }));
            index
        }
        Tree::Branch { mut children, meta } => {
            let index = nodes.len();
            nodes.push(Some(FlatTreeNode::Branch {
                children: Vec::new(),
                meta,
            }));
            let mut child_indices = Vec::with_capacity(children.len());
            for child in children.drain(..) {
                let child_index = append_tree_nodes(nodes, child);
                child_indices.push(child_index);
            }
            if let Some(FlatTreeNode::Branch { children, .. }) = nodes[index].as_mut() {
                *children = child_indices;
            }
            index
        }
    }
}

// Core subtree replacement. First recursively tombstones any existing descendants of
// `root_index` (so they don't dangle), then installs the new content. When the new tree
// is a branch, its children are appended to the end of the arena - meaning the new
// root keeps its slot but descendants live at fresh indices.
//
// Note(cc): the `None` branch below grows the arena just to immediately write `None`
// into the new slot. That's a no-op (default-extended slots are already `None`) but
// harmless; kept explicit so intent is obvious.
fn replace_tree_nodes<Node, Meta>(
    nodes: &mut Vec<Option<FlatTreeNode<Node, Meta>>>,
    tree: Option<Tree<Node, Meta>>,
    root_index: usize,
) where
    Node: Clone,
    Meta: Clone,
{
    if let Some(Some(FlatTreeNode::Branch { meta: _, children })) = nodes.get(root_index) {
        for child in children.clone() {
            replace_tree_nodes(nodes, None, child);
        }
    }

    match tree {
        None => {
            if root_index < nodes.len() {
                nodes[root_index] = None;
            } else {
                nodes.resize(root_index + 1, None);
                nodes[root_index] = None;
            }
        }
        Some(Tree::Leaf { node, meta }) => {
            if root_index >= nodes.len() {
                nodes.resize(root_index + 1, None);
            }
            nodes[root_index] = Some(FlatTreeNode::Leaf { node, meta });
        }
        Some(Tree::Branch { children, meta }) => {
            let mut child_indices = Vec::with_capacity(children.len());
            for child in children {
                let child_index = append_tree_nodes(nodes, child);
                child_indices.push(child_index);
            }
            if root_index >= nodes.len() {
                nodes.resize(root_index + 1, None);
            }
            nodes[root_index] = Some(FlatTreeNode::Branch {
                children: child_indices,
                meta,
            });
        }
    }
}

impl<Node, Meta> From<Tree<Node, Meta>> for FlatTree<Node, Meta> {
    fn from(tree: Tree<Node, Meta>) -> Self {
        let mut nodes = Vec::new();
        // Root will be at index 0 after the first append.
        let _ = append_tree_nodes(&mut nodes, tree);
        FlatTree { nodes }
    }
}

/// Reconstruct a nested tree (lenient):
/// - Missing or invalid children are skipped.
/// - If the root is missing, returns an empty Branch with default meta.
impl<Node, Meta> From<FlatTree<Node, Meta>> for Tree<Node, Meta>
where
    Meta: Default,
{
    fn from(mut flat: FlatTree<Node, Meta>) -> Self {
        fn build<Node, Meta>(
            index: usize,
            nodes: &mut [Option<FlatTreeNode<Node, Meta>>],
        ) -> Option<Tree<Node, Meta>>
        where
            Meta: Default,
        {
            if index >= nodes.len() {
                return None;
            }
            let node = nodes[index].take()?;
            match node {
                FlatTreeNode::Leaf { node, meta } => Some(Tree::Leaf { node, meta }),
                FlatTreeNode::Branch { children, meta } => {
                    let mut built_children = Vec::new();
                    for child_index in children {
                        if let Some(child) = build(child_index, nodes) {
                            built_children.push(child);
                        }
                    }
                    Some(Tree::Branch {
                        children: built_children,
                        meta,
                    })
                }
            }
        }

        build(0, &mut flat.nodes).unwrap_or_else(|| Tree::Branch {
            children: Vec::new(),
            meta: Meta::default(),
        })
    }
}
