//! Generic nested and flat tree representations with mapping helpers.
//!
//! Assumptions for FlatTree:
//! - Root index is always 0.
//! - Nodes are stored in a Vec<Option<Node>>; missing children (None) or
//!   out-of-bounds indices are tolerated by lenient reconstruction.
//! - When replacing a subtree at an index, existing descendants are removed
//!   (recursively set to None), and new children are appended at the end.
//! - Depth-first traversal uses post-order (children before parent).
//!
//! Conversions:
//! - From<Tree> to FlatTree always creates the root at index 0.
//! - From<FlatTree> to Tree is lenient: missing children are skipped; if the
//!   root is missing, returns an empty Branch with default Meta.

use std::future::Future;
use thiserror::Error;

#[derive(Debug, Clone)]
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
    pub fn branch(meta: Meta, children: Vec<Tree<Node, Meta>>) -> Self {
        Self::Branch { children, meta }
    }

    pub fn leaf(meta: Meta, node: Node) -> Self {
        Self::Leaf { node, meta }
    }

    pub fn is_leaf(&self) -> bool {
        matches!(self, Tree::Leaf { .. })
    }

    pub fn is_branch(&self) -> bool {
        matches!(self, Tree::Branch { .. })
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Tree::Branch { children, .. } => children.iter().all(|child| child.is_empty()),
            Tree::Leaf { .. } => false,
        }
    }

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

#[derive(Debug, Clone)]
pub enum FlatTreeNode<Node, Meta> {
    Branch { meta: Meta, children: Vec<usize> },
    Leaf { meta: Meta, node: Node },
}

#[derive(Debug, Clone)]
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

    pub fn root(&self) -> Option<&FlatTreeNode<Node, Meta>> {
        self.nodes.first()?.as_ref()
    }

    pub fn is_empty(&self) -> bool {
        self.root().is_none()
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

    pub fn append_tree(&mut self, tree: Tree<Node, Meta>) -> usize {
        append_tree_nodes(&mut self.nodes, tree)
    }

    pub fn replace_tree(&mut self, tree: Option<Tree<Node, Meta>>, root_index: usize) {
        replace_tree_nodes(&mut self.nodes, tree, root_index)
    }

    /// Depth-first search from the root. Returns indices in post-order
    /// (children before parent). Missing or out-of-bounds children are skipped.
    pub fn depth_first_search(&self) -> Vec<usize> {
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
