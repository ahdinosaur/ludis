//! Renderable, serializable view primitives for the lusid streaming UI.
//!
//! Every domain type that `lusid-apply` emits to the TUI (resource params,
//! resources, states, changes, operations) implements [`Render`] to produce a
//! [`View`]. Views are a small "virtual DOM" over text:
//!
//! - [`Span`] - a run of text (one segment, no line break)
//! - [`Line`] - a row of spans, renders as a single logical line
//! - [`Paragraph`] - a block of lines
//! - [`Fragment`] - zero-or-more views concatenated with no separator
//!
//! Plus [`ViewTree`], a recursive Branch/Leaf nesting of `View`s that
//! `termtree` can render as an indented tree on the terminal.
//!
//! Views are `Serialize`/`Deserialize` so the apply process can stream them
//! over stdout as JSON, and the TUI can reconstruct and render them.

mod render;
mod tree;
mod view;

pub use crate::render::*;
pub use crate::tree::*;
pub use crate::view::*;
