use serde::{Deserialize, Serialize};
use std::fmt::Display;

/// A run of text. The atomic unit of the view system: [`Line`](crate::Line)s
/// are `Vec<Span>`, so a line is a flat sequence of text fragments.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Span {
    pub content: String,
}

impl Span {
    pub fn new<T: Into<String>>(content: T) -> Self {
        Self {
            content: content.into(),
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.content.fmt(f)
    }
}

impl From<&str> for Span {
    fn from(value: &str) -> Self {
        Self::new(value)
    }
}

impl From<String> for Span {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}
