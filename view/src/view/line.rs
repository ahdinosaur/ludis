use std::fmt::{Debug, Display};

use serde::{Deserialize, Serialize};

use crate::{Span, View};

/// A single logical line of text, composed of one or more [`Span`]s.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Line {
    pub spans: Vec<Span>,
}

impl Line {
    pub fn new<S: Into<Vec<Span>>>(spans: S) -> Self {
        Self {
            spans: spans.into(),
        }
    }
}

impl Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for span in self.spans.iter() {
            Display::fmt(span, f)?
        }
        Ok(())
    }
}

impl From<Vec<Span>> for Line {
    fn from(value: Vec<Span>) -> Self {
        Line::new(value)
    }
}

impl From<Span> for Line {
    fn from(value: Span) -> Self {
        Line::new(vec![value])
    }
}

impl From<&str> for Line {
    fn from(value: &str) -> Self {
        Line::new(vec![Span::from(value)])
    }
}

impl From<String> for Line {
    fn from(value: String) -> Self {
        Line::new(vec![Span::from(value)])
    }
}

impl From<Line> for View {
    fn from(value: Line) -> Self {
        View::Line(value)
    }
}
