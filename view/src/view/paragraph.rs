use std::fmt::Display;

use serde::{Deserialize, Serialize};

use crate::{Line, View};

/// A block of [`Line`]s. The `Display` impl emits each line with a trailing
/// newline (via `writeln!`).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Paragraph {
    pub lines: Vec<Line>,
}

impl Paragraph {
    pub fn new<L: Into<Vec<Line>>>(lines: L) -> Self {
        Self {
            lines: lines.into(),
        }
    }
}

impl Display for Paragraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in self.lines.iter() {
            writeln!(f, "{line}")?
        }
        Ok(())
    }
}

impl From<Vec<Line>> for Paragraph {
    fn from(value: Vec<Line>) -> Self {
        Paragraph::new(value)
    }
}

impl From<Vec<&str>> for Paragraph {
    fn from(value: Vec<&str>) -> Self {
        let lines: Vec<Line> = value.into_iter().map(Line::from).collect();
        Paragraph::new(lines)
    }
}

impl From<Paragraph> for View {
    fn from(value: Paragraph) -> Self {
        View::Paragraph(value)
    }
}
