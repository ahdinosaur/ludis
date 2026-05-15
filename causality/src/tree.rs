use lusid_tree::Tree;

/// A [`Tree`] whose metadata carries dependency information for epoch scheduling.
pub type CausalityTree<Node, NodeId = String> = Tree<Node, CausalityMeta<NodeId>>;

/// Dependency metadata attached to every node.
///
/// - `id`: an optional label that other nodes can reference via `requires` / `required_by`.
///   Must be unique across the tree.
/// - `requires`: ids this node depends on (this node runs after those).
/// - `required_by`: ids that depend on this node (those run after this one).
///
/// When set on a branch, the dependency applies transitively to every descendant leaf,
/// and the branch id acts as a group reference - requiring a branch id means requiring
/// all leaves within it.
#[derive(Debug, Clone)]
pub struct CausalityMeta<NodeId> {
    pub id: Option<NodeId>,
    pub requires: Vec<NodeId>,
    pub required_by: Vec<NodeId>,
}

impl<NodeId> Default for CausalityMeta<NodeId> {
    fn default() -> Self {
        Self {
            id: None,
            requires: Vec::new(),
            required_by: Vec::new(),
        }
    }
}

impl<NodeId> CausalityMeta<NodeId> {
    pub fn id(id: NodeId) -> Self {
        Self {
            id: Some(id),
            requires: vec![],
            required_by: vec![],
        }
    }

    pub fn requires(requires: Vec<NodeId>) -> Self {
        Self {
            id: None,
            requires,
            required_by: vec![],
        }
    }

    pub fn required_by(required_by: Vec<NodeId>) -> Self {
        Self {
            id: None,
            requires: vec![],
            required_by,
        }
    }
}
