//! Rendering layer for lusid domain types.
//!
//! Domain types in `lusid-resource`, `lusid-operation`, and `lusid-plan` are
//! pure data: they serde-derive cleanly for the wire and have `Display` impls
//! for human text. This crate sits one layer up and produces a
//! [`RenderedNode`]: text annotated with a small [`SemanticTag`] vocabulary
//! that consumers (the TUI, plain-log mode, future Slack/web sinks) lower
//! into their own output format.
//!
//! Two consumer helpers are provided:
//!
//! - [`RenderedNode::to_plain_string`] - matches `Display` for plain-log mode.
//! - [`RenderedNode::to_ratatui_text`] - lowers tagged nodes through a
//!   [`Palette`] mapping each [`SemanticTag`] to a `ratatui::Style`.
//!
//! The [`Render`] trait is the entry point. Most domain types' renderings are
//! `RenderedNode::Plain(self.to_string())`; the [`display_render!`] macro
//! generates those mechanically. Hand-written impls are reserved for cases
//! where structured tagging adds value (diffs, multi-line content).

use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span, Text};
use termtree::Tree as TermTree;

pub mod operations;
pub mod plan_id;
pub mod resources;

/// Produce a [`RenderedNode`] for a domain value. Mirrors [`std::fmt::Display`]
/// but the result is structured so consumers can attach styles or transform
/// the text without re-parsing it.
pub trait Render {
    fn render(&self) -> RenderedNode;
}

/// Semantic role of a piece of rendered text. The [`Palette`] maps these to
/// concrete `ratatui::Style`s; plain-log mode ignores them.
///
/// Kept minimal: every additional tag forces every palette and consumer to
/// answer "what colour?". Add more only when the TUI palette genuinely needs
/// to distinguish them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemanticTag {
    Added,
    Removed,
    Modified,
    Unchanged,
    Error,
}

/// Structured rendering output. `Plain` is the leaf; `Tagged` attaches a
/// [`SemanticTag`] to a subtree; `Tree` builds an indented hierarchy via
/// [`termtree`] for plain output and via successive lines for ratatui.
#[derive(Debug, Clone)]
pub enum RenderedNode {
    Plain(String),
    Tagged {
        tag: SemanticTag,
        content: Vec<RenderedNode>,
    },
    Tree {
        root: Box<RenderedNode>,
        children: Vec<RenderedNode>,
    },
}

impl RenderedNode {
    pub fn plain(text: impl Into<String>) -> Self {
        RenderedNode::Plain(text.into())
    }

    pub fn tagged(tag: SemanticTag, content: Vec<RenderedNode>) -> Self {
        RenderedNode::Tagged { tag, content }
    }

    pub fn tree(root: RenderedNode, children: Vec<RenderedNode>) -> Self {
        RenderedNode::Tree {
            root: Box::new(root),
            children,
        }
    }

    /// Flatten to a plain string. `Tagged` discards its tag; `Tree` is rendered
    /// as an indented ASCII tree via [`termtree`], matching the previous
    /// `ViewTree::Display` output shape.
    pub fn to_plain_string(&self) -> String {
        match self {
            RenderedNode::Plain(text) => text.clone(),
            RenderedNode::Tagged { content, .. } => {
                content.iter().map(RenderedNode::to_plain_string).collect()
            }
            RenderedNode::Tree { root, children } => {
                let mut tree = TermTree::new(root.to_plain_string());
                for child in children {
                    tree.push(node_to_termtree(child));
                }
                tree.to_string()
            }
        }
    }

    /// Lower to a `ratatui::text::Text`, applying `palette` to tagged subtrees.
    ///
    /// `Plain` becomes one line per `\n` in the string. `Tagged` styles its
    /// content lines through the palette. `Tree` is rendered as the same
    /// indented ASCII tree as [`Self::to_plain_string`], then split into
    /// styled lines.
    pub fn to_ratatui_text(&self, palette: &Palette) -> Text<'static> {
        let mut lines: Vec<Line<'static>> = Vec::new();
        append_lines(&mut lines, self, palette, Style::default());
        Text::from(lines)
    }
}

fn node_to_termtree(node: &RenderedNode) -> TermTree<String> {
    match node {
        RenderedNode::Tree { root, children } => {
            let mut tree = TermTree::new(root.to_plain_string());
            for child in children {
                tree.push(node_to_termtree(child));
            }
            tree
        }
        _ => TermTree::new(node.to_plain_string()),
    }
}

fn append_lines(
    lines: &mut Vec<Line<'static>>,
    node: &RenderedNode,
    palette: &Palette,
    base: Style,
) {
    match node {
        RenderedNode::Plain(text) => {
            for (i, segment) in text.split('\n').enumerate() {
                if i == 0
                    && let Some(last) = lines.last_mut()
                {
                    last.spans.push(Span::styled(segment.to_string(), base));
                    continue;
                }
                lines.push(Line::from(Span::styled(segment.to_string(), base)));
            }
        }
        RenderedNode::Tagged { tag, content } => {
            let style = base.patch(palette.style_for(*tag));
            for child in content {
                append_lines(lines, child, palette, style);
            }
        }
        RenderedNode::Tree { .. } => {
            for line in node.to_plain_string().split('\n') {
                lines.push(Line::from(Span::styled(line.to_string(), base)));
            }
        }
    }
}

/// Maps each [`SemanticTag`] to a `ratatui::Style`. Consumers can override the
/// default by constructing their own. Held by reference at render time.
#[derive(Debug, Clone)]
pub struct Palette {
    pub added: Style,
    pub removed: Style,
    pub modified: Style,
    pub unchanged: Style,
    pub error: Style,
}

impl Palette {
    pub fn style_for(&self, tag: SemanticTag) -> Style {
        match tag {
            SemanticTag::Added => self.added,
            SemanticTag::Removed => self.removed,
            SemanticTag::Modified => self.modified,
            SemanticTag::Unchanged => self.unchanged,
            SemanticTag::Error => self.error,
        }
    }
}

impl Default for Palette {
    fn default() -> Self {
        Self {
            added: Style::default().fg(Color::Green),
            removed: Style::default().fg(Color::Red),
            modified: Style::default().fg(Color::Yellow),
            unchanged: Style::default().add_modifier(Modifier::DIM),
            error: Style::default().fg(Color::Red).add_modifier(Modifier::BOLD),
        }
    }
}

/// Bind [`Render`] to any `Display` type by wrapping `to_string()` in a
/// [`RenderedNode::Plain`]. The common case for domain types whose textual
/// form is already authored by their `Display` impl.
#[macro_export]
macro_rules! display_render {
    ($type:ty) => {
        impl $crate::Render for $type {
            fn render(&self) -> $crate::RenderedNode {
                $crate::RenderedNode::Plain(self.to_string())
            }
        }
    };
}

/// `None` renders as an empty `Plain`. Lets optional fields render
/// unconditionally without callers branching.
impl<T> Render for Option<T>
where
    T: Render,
{
    fn render(&self) -> RenderedNode {
        match self {
            Some(inner) => inner.render(),
            None => RenderedNode::Plain(String::new()),
        }
    }
}

display_render!(String);
display_render!(&str);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn plain_renders_to_string() {
        let node = RenderedNode::plain("hello");
        assert_eq!(node.to_plain_string(), "hello");
    }

    #[test]
    fn tagged_drops_tag_in_plain_string() {
        let node = RenderedNode::tagged(SemanticTag::Added, vec![RenderedNode::plain("+ line")]);
        assert_eq!(node.to_plain_string(), "+ line");
    }

    #[test]
    fn tree_uses_termtree_layout() {
        let node = RenderedNode::tree(
            RenderedNode::plain("root"),
            vec![
                RenderedNode::plain("a"),
                RenderedNode::tree(RenderedNode::plain("b"), vec![RenderedNode::plain("b1")]),
            ],
        );
        let plain = node.to_plain_string();
        assert!(plain.contains("root"));
        assert!(plain.contains("a"));
        assert!(plain.contains("b"));
        assert!(plain.contains("b1"));
    }

    #[test]
    fn ratatui_text_applies_palette() {
        let palette = Palette::default();
        let node =
            RenderedNode::tagged(SemanticTag::Removed, vec![RenderedNode::plain("- removed")]);
        let text = node.to_ratatui_text(&palette);
        let line = text.lines.first().expect("one line");
        let span = line.spans.first().expect("one span");
        assert_eq!(span.content, "- removed");
        assert_eq!(span.style, palette.removed);
    }

    #[test]
    fn option_renders_some_and_none() {
        let some: Option<String> = Some("hi".into());
        let none: Option<String> = None;
        assert_eq!(some.render().to_plain_string(), "hi");
        assert_eq!(none.render().to_plain_string(), "");
    }
}
