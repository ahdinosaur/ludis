//! Renderable, serializable view primitives for the lusid streaming UI.
//!
//! Every domain type that `lusid-apply` emits to the TUI (resource params,
//! resources, states, changes, operations) implements [`Render`] to produce a
//! [`View`]. Views are a small "virtual DOM" over styled text:
//!
//! - [`Span`] - a run of styled text (one segment, no line break)
//! - [`Line`] - a row of spans, renders as a single logical line
//! - [`Paragraph`] - a block of lines
//! - [`Fragment`] - zero-or-more views concatenated with no separator
//!
//! Plus [`ViewTree`], a recursive Branch/Leaf nesting of `View`s that
//! `termtree` can render as an indented tree on the terminal.
//!
//! The design goal is that views are `Serialize`/`Deserialize` so the apply
//! process can stream them over stdout as JSON, and the TUI can reconstruct
//! and render them. Styling metadata ([`TextStyle`], [`Color`], [`Alignment`])
//! travels with the view.
//!
//! Note(cc): the current TUI ([`lusid/src/tui.rs`]) uses ratatui's own styling
//! and only consumes the text content of views (via `Display`). The style
//! fields here are intentional overhead for a future renderer that honours
//! them, and for non-TUI consumers.

mod render;
mod tree;
mod view;

pub use crate::render::*;
pub use crate::tree::*;
pub use crate::view::*;
