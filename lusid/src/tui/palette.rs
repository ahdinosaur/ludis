//! Status badge palette for the Tree and Epochs pages.
//!
//! [`Badge`] captures the five rollup states the Task 12 spec defines:
//! Planned / Running / Ok (no change) / Changed (pending or applied) / Failed.
//! Glyph + colour come from this module so per-page rendering shares one
//! source of truth.

use ratatui::style::{Color, Modifier, Style};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Badge {
    /// Not started yet.
    Planned,
    /// Probing or applying.
    Running,
    /// Resolved with no change.
    Ok,
    /// Resolved with a pending or applied change.
    Changed,
    /// Failed during apply.
    ///
    /// TODO(cc): wire this in. Phase A/B errors aren't yet mapped back to
    /// the originating atom, so the badge has no construction site. Kept
    /// in the enum so `palette::rollup` precedence is complete the moment
    /// the mapping lands.
    #[allow(dead_code)]
    Failed,
}

impl Badge {
    pub const fn glyph(self) -> &'static str {
        match self {
            Badge::Planned => "▸",
            Badge::Running => "↻",
            Badge::Ok => "✓",
            Badge::Changed => "~",
            Badge::Failed => "✗",
        }
    }

    pub fn style(self) -> Style {
        match self {
            Badge::Planned => Style::default().fg(Color::DarkGray),
            Badge::Running => Style::default().fg(Color::Blue),
            Badge::Ok => Style::default()
                .fg(Color::Green)
                .add_modifier(Modifier::DIM),
            Badge::Changed => Style::default().fg(Color::Yellow),
            Badge::Failed => Style::default().fg(Color::Red).add_modifier(Modifier::BOLD),
        }
    }
}

/// Merge two child badges into a single parent rollup. Precedence:
/// `Failed > Running > Changed > Ok > Planned`. Applied repeatedly across
/// a branch's children to derive the branch badge.
pub fn rollup(a: Badge, b: Badge) -> Badge {
    use Badge::*;
    let rank = |x: Badge| match x {
        Failed => 4,
        Running => 3,
        Changed => 2,
        Ok => 1,
        Planned => 0,
    };
    if rank(b) > rank(a) { b } else { a }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rollup_precedence() {
        assert_eq!(rollup(Badge::Planned, Badge::Ok), Badge::Ok);
        assert_eq!(rollup(Badge::Ok, Badge::Changed), Badge::Changed);
        assert_eq!(rollup(Badge::Changed, Badge::Running), Badge::Running);
        assert_eq!(rollup(Badge::Running, Badge::Failed), Badge::Failed);
        assert_eq!(rollup(Badge::Failed, Badge::Planned), Badge::Failed);
    }

    #[test]
    fn glyphs_are_one_char_wide() {
        for b in [
            Badge::Planned,
            Badge::Running,
            Badge::Ok,
            Badge::Changed,
            Badge::Failed,
        ] {
            assert_eq!(b.glyph().chars().count(), 1, "{b:?}");
        }
    }
}
