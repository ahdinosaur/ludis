mod fragment;
mod line;
mod paragraph;
mod span;
mod text;

use std::fmt::Debug;
use std::fmt::Display;

use serde::Deserialize;
use serde::Serialize;

pub use self::fragment::*;
pub use self::line::*;
pub use self::paragraph::*;
pub use self::span::*;
pub use self::text::*;

/// The central view type. Sum of the four primitive shapes; every `Render`
/// implementation produces one of these. `Display` recurses into the chosen
/// variant - the text content is preserved, but the styling metadata on
/// [`Span`]/[`Line`]/[`Paragraph`] is only visible to callers that inspect
/// the structure directly.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum View {
    Fragment(Fragment),
    Span(Span),
    Line(Line),
    Paragraph(Paragraph),
}

impl Display for View {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            View::Span(view) => Display::fmt(view, f),
            View::Fragment(view) => Display::fmt(view, f),
            View::Line(view) => Display::fmt(view, f),
            View::Paragraph(view) => Display::fmt(view, f),
        }
    }
}
