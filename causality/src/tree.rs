use lusid_tree::Tree;

pub type CausalityTree<Node, NodeId = String> = Tree<Node, CausalityMeta<NodeId>>;

#[derive(Debug, Clone)]
pub struct CausalityMeta<NodeId> {
    pub id: Option<NodeId>,
    pub before: Vec<NodeId>,
    pub after: Vec<NodeId>,
}

impl<NodeId> Default for CausalityMeta<NodeId> {
    fn default() -> Self {
        Self {
            id: None,
            before: Vec::new(),
            after: Vec::new(),
        }
    }
}

impl<NodeId> CausalityMeta<NodeId> {
    pub fn id(id: NodeId) -> Self {
        Self {
            id: Some(id),
            before: vec![],
            after: vec![],
        }
    }

    pub fn before(before: Vec<NodeId>) -> Self {
        Self {
            id: None,
            before,
            after: vec![],
        }
    }

    pub fn after(after: Vec<NodeId>) -> Self {
        Self {
            id: None,
            before: vec![],
            after,
        }
    }
}
