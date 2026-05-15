//! Dependency ordering over a [`lusid_tree::Tree`].
//!
//! Each node carries a [`CausalityMeta`] with three pieces of information:
//! - `id`: an optional identifier the node can be referenced by.
//! - `requires`: ids this node depends on - it can only run after those are done.
//! - `required_by`: ids that depend on this node - those run after it.
//!
//! [`compute_epochs`] flattens the tree into topologically-sorted layers ("epochs")
//! using Kahn's algorithm. Each epoch is a set of nodes with no remaining
//! dependencies, so they can be executed in parallel.

mod epoch;
mod tree;

pub use crate::epoch::*;
pub use crate::tree::*;
