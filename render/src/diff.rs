//! Rich diff rendering for [`ResourceChange`] values.
//!
//! The [`Render`] impl that ships with each resource family produces a
//! one-line `Display`-style summary suited to logs and the plan tree label
//! column. The detail pane wants more: side-by-side file content, scalar
//! before/after rows, redacted placeholders for secrets. [`render_change`]
//! is that richer renderer.
//!
//! Output is still a [`RenderedNode`] so the TUI lowers it through the same
//! [`crate::Palette`] as everything else.

use lusid_operation::operations::file::{FileGroup, FileMode, FilePath, FileSource, FileUser};
use lusid_resource::ResourceChange;
use lusid_resource::file::{Content, FileChange};
use similar::{ChangeTag, TextDiff};

use crate::{Render, RenderedNode, SemanticTag};

/// Options for [`render_change`]. `context_lines` controls how many `Equal`
/// lines surround each change block in unified mode. `side_by_side` switches
/// to the paired two-column layout, valid when `width >= 140` per spec.
/// `width` budgets the side-by-side column padding.
#[derive(Debug, Clone, Copy)]
pub struct DiffOptions {
    pub side_by_side: bool,
    pub width: usize,
    pub context_lines: usize,
}

impl Default for DiffOptions {
    fn default() -> Self {
        Self {
            side_by_side: false,
            width: 100,
            context_lines: 3,
        }
    }
}

/// Render a [`ResourceChange`] with diff-aware formatting where the change
/// payload supports it (currently the `@resource/file` family). Other
/// families fall through to their plain [`Render`] impl - those changes
/// don't carry before/after data, so there's nothing to diff.
pub fn render_change(change: &ResourceChange, opts: DiffOptions) -> RenderedNode {
    match change {
        ResourceChange::File(file_change) => render_file_change(file_change, opts),
        other => other.render(),
    }
}

fn render_file_change(change: &FileChange, opts: DiffOptions) -> RenderedNode {
    match change {
        FileChange::Write {
            path,
            source,
            before,
            after,
        } => render_write(path, source, before.as_ref(), after, opts),
        FileChange::ChangeMode { path, mode } => render_mode_change(path, *mode),
        FileChange::ChangeOwner { path, user, group } => {
            render_owner_change(path, user.as_ref(), group.as_ref())
        }
        // CreateSymlink / Remove have no meaningful before/after; the
        // one-line Display summary already says everything.
        other => other.render(),
    }
}

// --------------------------------------------------------------------------
// File content (Write)
// --------------------------------------------------------------------------

fn render_write(
    path: &FilePath,
    source: &FileSource,
    before: Option<&Content>,
    after: &Content,
    opts: DiffOptions,
) -> RenderedNode {
    let mut content = Vec::new();
    push_plain_line(&mut content, format!("path: {path}"));
    push_plain_line(&mut content, format!("source: {}", source_label(source)));
    push_plain_line(&mut content, String::new());

    let body = render_content_diff(before, after, opts);
    content.extend(body);

    RenderedNode::Tagged {
        tag: SemanticTag::Modified,
        content,
    }
}

fn source_label(source: &FileSource) -> String {
    match source {
        FileSource::Contents(bytes) => format!("inline contents ({} bytes)", bytes.len()),
        FileSource::Path(p) => format!("host path {p}"),
        FileSource::Secret(name) => format!("secret {name:?}"),
    }
}

fn render_content_diff(
    before: Option<&Content>,
    after: &Content,
    opts: DiffOptions,
) -> Vec<RenderedNode> {
    match (before, after) {
        // Either side redacted: refuse to diff, render the placeholder both
        // sides so the operator sees that *something* changed without
        // leaking plaintext on the unredacted side either. This is paranoid
        // by design - a partial-secret config is still a secret.
        (Some(b), _) if is_redacted(b) => {
            redacted_diff_placeholder(Some(b), after, opts.side_by_side)
        }
        (_, a) if is_redacted(a) => redacted_diff_placeholder(before, a, opts.side_by_side),
        // New file: render the whole `after` as an addition; before label
        // is `(no file)`.
        (None, Content::Bytes(after_bytes)) => render_new_file_diff(after_bytes, opts),
        (Some(Content::Bytes(before_bytes)), Content::Bytes(after_bytes)) => {
            if is_binary(before_bytes) || is_binary(after_bytes) {
                binary_diff_placeholder(Some(before_bytes), after_bytes, opts.side_by_side)
            } else {
                let before_str = String::from_utf8_lossy(before_bytes);
                let after_str = String::from_utf8_lossy(after_bytes);
                if opts.side_by_side {
                    render_side_by_side(&before_str, &after_str, opts)
                } else {
                    render_unified(&before_str, &after_str, opts.context_lines)
                }
            }
        }
        // Belt-and-braces: Redacted variants are handled above. The match is
        // exhaustive on Content's two variants, but the compiler doesn't
        // recognise the `is_redacted` short-circuit, so this arm is dead.
        _ => unreachable!("redacted content handled by guards above"),
    }
}

fn render_new_file_diff(after: &[u8], opts: DiffOptions) -> Vec<RenderedNode> {
    let mut out = Vec::new();
    push_plain_line(&mut out, "(no file)".to_string());
    push_plain_line(&mut out, "---".to_string());
    if is_binary(after) {
        push_plain_line(&mut out, format!("<binary content, {} bytes>", after.len()));
        return out;
    }
    let text = String::from_utf8_lossy(after);
    if opts.side_by_side {
        for line in split_lines(&text) {
            out.push(tagged_line(
                SemanticTag::Added,
                format!(
                    "{:width$} │ + {}",
                    "",
                    line,
                    width = side_col_width(opts.width)
                ),
            ));
        }
    } else {
        for line in split_lines(&text) {
            out.push(tagged_line(SemanticTag::Added, format!("+ {line}")));
        }
    }
    out
}

fn render_unified(before: &str, after: &str, context: usize) -> Vec<RenderedNode> {
    let diff = TextDiff::from_lines(before, after);
    let mut out = Vec::new();
    push_plain_line(&mut out, "--- before".to_string());
    push_plain_line(&mut out, "+++ after".to_string());
    for group in diff.grouped_ops(context) {
        for op in group {
            for change in diff.iter_changes(&op) {
                let line = strip_trailing_newline(change.value());
                let (tag, marker) = match change.tag() {
                    ChangeTag::Delete => (SemanticTag::Removed, '-'),
                    ChangeTag::Insert => (SemanticTag::Added, '+'),
                    ChangeTag::Equal => (SemanticTag::Unchanged, ' '),
                };
                out.push(tagged_line(tag, format!("{marker} {line}")));
            }
        }
    }
    if out.len() == 2 {
        // No diff groups - before and after were textually identical. The
        // change event still fired (a non-content field flipped); render
        // an explicit "(no textual change)" so the pane isn't blank.
        push_plain_line(&mut out, "(no textual change)".to_string());
    }
    out
}

fn render_side_by_side(before: &str, after: &str, opts: DiffOptions) -> Vec<RenderedNode> {
    let diff = TextDiff::from_lines(before, after);
    let col = side_col_width(opts.width);
    let mut out = Vec::new();
    out.push(plain_line(format!(
        "{:<width$} │ {}",
        "before",
        "after",
        width = col,
    )));
    out.push(plain_line(format!(
        "{:<width$} │ {}",
        "─".repeat(col),
        "─".repeat(col),
        width = col,
    )));

    // Each top-level op carries a single change kind (Equal | Delete |
    // Insert | Replace); within a Replace, `iter_changes` yields all
    // Deletes then all Inserts. Pair Delete/Insert from a Replace op,
    // emit single-side rows for pure Delete or Insert, mirror Equal on
    // both sides.
    for group in diff.grouped_ops(opts.context_lines) {
        for op in group {
            let mut left: Vec<&str> = Vec::new();
            let mut right: Vec<&str> = Vec::new();
            for change in diff.iter_changes(&op) {
                let line = strip_trailing_newline(change.value());
                match change.tag() {
                    ChangeTag::Delete => left.push(line),
                    ChangeTag::Insert => right.push(line),
                    ChangeTag::Equal => out.push(equal_pair(line, col)),
                }
            }
            flush_pairs(&mut out, &mut left, &mut right, col);
        }
    }
    out
}

fn flush_pairs(
    out: &mut Vec<RenderedNode>,
    left: &mut Vec<&str>,
    right: &mut Vec<&str>,
    col: usize,
) {
    let n = left.len().max(right.len());
    for i in 0..n {
        let l_text = left.get(i).copied();
        let r_text = right.get(i).copied();
        match (l_text, r_text) {
            (Some(l), Some(r)) => {
                out.push(modify_pair(l, r, col));
            }
            (Some(l), None) => {
                out.push(left_only(l, col));
            }
            (None, Some(r)) => {
                out.push(right_only(r, col));
            }
            (None, None) => {}
        }
    }
    left.clear();
    right.clear();
}

fn equal_pair(line: &str, col: usize) -> RenderedNode {
    let cell = truncate(&format!("  {line}"), col);
    tagged_line(
        SemanticTag::Unchanged,
        format!("{cell:<col$} │ {cell:<col$}"),
    )
}

fn modify_pair(before: &str, after: &str, col: usize) -> RenderedNode {
    // The whole row is a "modification"; using Modified for the row keeps
    // visual cohesion. Individual sides aren't separately re-tagged here
    // because ratatui Span styles can't vary mid-line in this renderer
    // (we ship one tag per RenderedNode line).
    tagged_line(
        SemanticTag::Modified,
        format!(
            "{:<col$} │ {:<col$}",
            truncate(&format!("- {before}"), col),
            truncate(&format!("+ {after}"), col),
            col = col,
        ),
    )
}

fn left_only(before: &str, col: usize) -> RenderedNode {
    tagged_line(
        SemanticTag::Removed,
        format!(
            "{:<col$} │ {:<col$}",
            truncate(&format!("- {before}"), col),
            "",
            col = col,
        ),
    )
}

fn right_only(after: &str, col: usize) -> RenderedNode {
    tagged_line(
        SemanticTag::Added,
        format!(
            "{:<col$} │ {:<col$}",
            "",
            truncate(&format!("+ {after}"), col),
            col = col,
        ),
    )
}

fn side_col_width(width: usize) -> usize {
    // Reserve 3 cols for ` │ ` separator.
    width.saturating_sub(3) / 2
}

fn truncate(s: &str, width: usize) -> String {
    if s.chars().count() <= width {
        s.to_string()
    } else {
        let mut out = String::with_capacity(width);
        for (i, c) in s.chars().enumerate() {
            if i + 1 >= width {
                out.push('…');
                break;
            }
            out.push(c);
        }
        out
    }
}

fn split_lines(text: &str) -> Vec<&str> {
    if text.is_empty() {
        Vec::new()
    } else {
        text.split_inclusive('\n')
            .map(strip_trailing_newline)
            .collect()
    }
}

fn strip_trailing_newline(line: &str) -> &str {
    line.strip_suffix('\n').unwrap_or(line)
}

fn is_redacted(c: &Content) -> bool {
    matches!(c, Content::Redacted { .. })
}

/// UTF-8 invalidity OR a NUL byte in the first 8 KiB classifies as binary.
/// Either signal is enough to refuse a unified diff that would otherwise
/// dump unreadable bytes into the operator's pane.
fn is_binary(bytes: &[u8]) -> bool {
    let window = &bytes[..bytes.len().min(8192)];
    if window.contains(&0u8) {
        return true;
    }
    std::str::from_utf8(bytes).is_err()
}

fn binary_diff_placeholder(
    before: Option<&[u8]>,
    after: &[u8],
    side_by_side: bool,
) -> Vec<RenderedNode> {
    let before_label = match before {
        Some(b) => format!("<binary content, {} bytes>", b.len()),
        None => "(no file)".to_string(),
    };
    let after_label = format!("<binary content, {} bytes>", after.len());
    placeholder_diff(&before_label, &after_label, side_by_side)
}

fn redacted_diff_placeholder(
    before: Option<&Content>,
    after: &Content,
    side_by_side: bool,
) -> Vec<RenderedNode> {
    // FileChange::Write always carries an `after`; only `before` is
    // optional (None on a fresh create).
    let before_label = match before {
        Some(c) => content_placeholder(c),
        None => "(no file)".to_string(),
    };
    let after_label = content_placeholder(after);
    placeholder_diff(&before_label, &after_label, side_by_side)
}

fn content_placeholder(c: &Content) -> String {
    match c {
        Content::Redacted { len, sha256 } => {
            let prefix = sha256.get(..16).unwrap_or(sha256.as_str());
            format!("<redacted: {len} bytes, sha256:{prefix}>")
        }
        Content::Bytes(b) => format!("<{} bytes>", b.len()),
    }
}

fn placeholder_diff(before: &str, after: &str, side_by_side: bool) -> Vec<RenderedNode> {
    let mut out = Vec::new();
    if side_by_side {
        out.push(plain_line(format!("before: {before}")));
        out.push(plain_line(format!("after:  {after}")));
    } else {
        out.push(tagged_line(SemanticTag::Removed, format!("- {before}")));
        out.push(tagged_line(SemanticTag::Added, format!("+ {after}")));
    }
    out
}

// --------------------------------------------------------------------------
// Scalar (mode, owner)
// --------------------------------------------------------------------------

fn render_mode_change(path: &FilePath, mode: FileMode) -> RenderedNode {
    let mut content = Vec::new();
    push_plain_line(&mut content, format!("path: {path}"));
    push_plain_line(&mut content, String::new());
    content.push(scalar_row("mode", &format!("{:o}", mode.as_u32())));
    RenderedNode::Tagged {
        tag: SemanticTag::Modified,
        content,
    }
}

fn render_owner_change(
    path: &FilePath,
    user: Option<&FileUser>,
    group: Option<&FileGroup>,
) -> RenderedNode {
    let mut content = Vec::new();
    push_plain_line(&mut content, format!("path: {path}"));
    push_plain_line(&mut content, String::new());
    if let Some(user) = user {
        content.push(scalar_row("user", user.as_str()));
    }
    if let Some(group) = group {
        content.push(scalar_row("group", group.as_str()));
    }
    RenderedNode::Tagged {
        tag: SemanticTag::Modified,
        content,
    }
}

fn scalar_row(field: &str, after: &str) -> RenderedNode {
    // before is not preserved on the wire for these change kinds (state
    // probes only return "correct"/"incorrect"). Render only the desired
    // value; the panel header already labels this as a Change section.
    tagged_line(SemanticTag::Modified, format!("{field:<8} → {after}"))
}

// --------------------------------------------------------------------------
// Small RenderedNode helpers
// --------------------------------------------------------------------------

fn plain_line(text: String) -> RenderedNode {
    RenderedNode::Plain(format!("{text}\n"))
}

fn push_plain_line(out: &mut Vec<RenderedNode>, text: String) {
    out.push(plain_line(text));
}

fn tagged_line(tag: SemanticTag, text: String) -> RenderedNode {
    RenderedNode::Tagged {
        tag,
        content: vec![RenderedNode::Plain(format!("{text}\n"))],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lusid_operation::operations::file::{FileMode, FilePath, FileSource};
    use lusid_resource::file::{Content, FileChange};

    fn unified(before: &str, after: &str) -> String {
        let change = FileChange::Write {
            path: FilePath::new("/etc/nginx.conf"),
            source: FileSource::Path(FilePath::new("/host/nginx.conf")),
            before: Some(Content::Bytes(before.as_bytes().to_vec())),
            after: Content::Bytes(after.as_bytes().to_vec()),
        };
        render_file_change(&change, DiffOptions::default()).to_plain_string()
    }

    #[test]
    fn unified_diff_shows_added_and_removed_lines() {
        let s = unified("a\nb\nc\n", "a\nbb\nc\n");
        assert!(s.contains("- b"), "missing removed line: {s}");
        assert!(s.contains("+ bb"), "missing added line: {s}");
    }

    #[test]
    fn unified_diff_for_new_file_marks_all_added() {
        let change = FileChange::Write {
            path: FilePath::new("/etc/new.conf"),
            source: FileSource::Path(FilePath::new("/host/new.conf")),
            before: None,
            after: Content::Bytes(b"alpha\nbeta\n".to_vec()),
        };
        let s = render_file_change(&change, DiffOptions::default()).to_plain_string();
        assert!(s.contains("(no file)"));
        assert!(s.contains("+ alpha"));
        assert!(s.contains("+ beta"));
    }

    #[test]
    fn redacted_content_does_not_leak_plaintext() {
        let plaintext = "the-password-is-hunter2";
        let change = FileChange::Write {
            path: FilePath::new("/etc/secret"),
            source: FileSource::Secret("api-key".into()),
            before: Some(Content::redacted(plaintext.as_bytes())),
            after: Content::redacted(plaintext.as_bytes()),
        };
        let s = render_file_change(&change, DiffOptions::default()).to_plain_string();
        assert!(!s.contains(plaintext), "plaintext leaked: {s}");
        assert!(s.contains("<redacted:"), "no redacted placeholder: {s}");
    }

    #[test]
    fn binary_content_skips_unified_diff() {
        let before = vec![0u8, 1, 2, 3, 4];
        let after = vec![0u8, 1, 2, 3, 5];
        let change = FileChange::Write {
            path: FilePath::new("/usr/bin/app"),
            source: FileSource::Path(FilePath::new("/host/app")),
            before: Some(Content::Bytes(before)),
            after: Content::Bytes(after),
        };
        let s = render_file_change(&change, DiffOptions::default()).to_plain_string();
        assert!(s.contains("<binary content"), "expected placeholder: {s}");
    }

    #[test]
    fn mode_change_renders_octal() {
        let change = FileChange::ChangeMode {
            path: FilePath::new("/etc/nginx.conf"),
            mode: FileMode::new(0o600),
        };
        let s = render_file_change(&change, DiffOptions::default()).to_plain_string();
        assert!(s.contains("mode"));
        assert!(s.contains("600"));
    }

    #[test]
    fn side_by_side_emits_two_column_layout() {
        let change = FileChange::Write {
            path: FilePath::new("/etc/nginx.conf"),
            source: FileSource::Path(FilePath::new("/host/nginx.conf")),
            before: Some(Content::Bytes(b"a\nb\nc\n".to_vec())),
            after: Content::Bytes(b"a\nbb\nc\n".to_vec()),
        };
        let s = render_file_change(
            &change,
            DiffOptions {
                side_by_side: true,
                width: 140,
                context_lines: 3,
            },
        )
        .to_plain_string();
        assert!(s.contains("│"), "no column separator: {s}");
        assert!(s.contains("before"), "no header: {s}");
    }

    #[test]
    fn render_change_falls_through_for_non_file_variants() {
        use lusid_resource::ResourceChange;
        use lusid_resource::apt::AptChange;
        let change = ResourceChange::Apt(AptChange::Install {
            package: "nginx".into(),
        });
        let s = render_change(&change, DiffOptions::default()).to_plain_string();
        assert_eq!(s, change.to_string());
    }

    #[test]
    fn side_by_side_asymmetric_replace_pads_extra_lines() {
        // 2 deletes vs 3 inserts: pair the first two, render the trailing
        // insert with an empty left column.
        let change = FileChange::Write {
            path: FilePath::new("/etc/nginx.conf"),
            source: FileSource::Path(FilePath::new("/host/nginx.conf")),
            before: Some(Content::Bytes(b"a\nb\n".to_vec())),
            after: Content::Bytes(b"x\ny\nz\n".to_vec()),
        };
        let s = render_file_change(
            &change,
            DiffOptions {
                side_by_side: true,
                width: 140,
                context_lines: 3,
            },
        )
        .to_plain_string();
        assert!(s.contains("- a"), "missing '- a' row: {s}");
        assert!(s.contains("+ x"), "missing '+ x' row: {s}");
        // Trailing insert must surface even with no paired delete - the
        // right-only path. Earlier versions silently dropped it.
        assert!(s.contains("+ z"), "missing '+ z' row: {s}");
        // The right-only row pads the left column to whitespace so the
        // separator column stays aligned.
        assert!(
            s.lines().any(|l| l.contains("│") && l.contains("+ z")),
            "expected '+ z' under separator: {s}",
        );
    }

    #[test]
    fn side_by_side_pure_delete_leaves_right_column_blank() {
        let change = FileChange::Write {
            path: FilePath::new("/etc/nginx.conf"),
            source: FileSource::Path(FilePath::new("/host/nginx.conf")),
            before: Some(Content::Bytes(b"a\nb\nc\nd\n".to_vec())),
            after: Content::Bytes(b"a\nc\nd\n".to_vec()),
        };
        let s = render_file_change(
            &change,
            DiffOptions {
                side_by_side: true,
                width: 140,
                context_lines: 3,
            },
        )
        .to_plain_string();
        assert!(s.contains("- b"), "missing '- b' row: {s}");
        // The deleted-only line should have nothing useful on the right.
        let delete_row = s
            .lines()
            .find(|l| l.contains("- b"))
            .expect("delete row exists");
        let (_, right) = delete_row.split_once('│').expect("separator");
        assert!(
            right.trim().is_empty(),
            "expected blank right column on pure delete: {delete_row:?}"
        );
    }

    #[test]
    fn change_owner_renders_user_and_group_rows() {
        use lusid_operation::operations::file::{FileGroup, FileUser};
        let change = FileChange::ChangeOwner {
            path: FilePath::new("/etc/nginx.conf"),
            user: Some(FileUser::new("root")),
            group: Some(FileGroup::new("wheel")),
        };
        let s = render_file_change(&change, DiffOptions::default()).to_plain_string();
        assert!(s.contains("user"), "missing user row: {s}");
        assert!(s.contains("root"), "missing user value: {s}");
        assert!(s.contains("group"), "missing group row: {s}");
        assert!(s.contains("wheel"), "missing group value: {s}");
    }

    #[test]
    fn create_symlink_falls_through_to_display() {
        let change = FileChange::CreateSymlink {
            source: FilePath::new("/host/src"),
            path: FilePath::new("/target/link"),
        };
        let s = render_file_change(&change, DiffOptions::default()).to_plain_string();
        assert_eq!(s, change.to_string());
    }

    #[test]
    fn identical_bytes_render_no_textual_change_marker() {
        // Defensive: a FileChange::Write with before == after shouldn't
        // happen in practice (the change wouldn't be emitted) but the
        // renderer must not blank the pane if it ever does.
        let change = FileChange::Write {
            path: FilePath::new("/etc/nginx.conf"),
            source: FileSource::Path(FilePath::new("/host/nginx.conf")),
            before: Some(Content::Bytes(b"unchanged\n".to_vec())),
            after: Content::Bytes(b"unchanged\n".to_vec()),
        };
        let s = render_file_change(&change, DiffOptions::default()).to_plain_string();
        assert!(s.contains("(no textual change)"), "missing marker: {s}");
    }

    #[test]
    fn side_by_side_at_narrow_width_still_produces_output() {
        // The TUI gates the toggle at 140 cols, but the renderer must
        // tolerate narrower inputs without panicking - bad arithmetic
        // here would have hit `side_col_width` and underflowed.
        let change = FileChange::Write {
            path: FilePath::new("/etc/nginx.conf"),
            source: FileSource::Path(FilePath::new("/host/nginx.conf")),
            before: Some(Content::Bytes(b"a\n".to_vec())),
            after: Content::Bytes(b"b\n".to_vec()),
        };
        let s = render_file_change(
            &change,
            DiffOptions {
                side_by_side: true,
                width: 20,
                context_lines: 3,
            },
        )
        .to_plain_string();
        assert!(!s.is_empty());
    }

    #[test]
    fn binary_detection_picks_up_utf8_invalid_bytes() {
        // Skip NUL bytes (the other binary signal) so this test exercises
        // the UTF-8-validity branch independently.
        let before = vec![1u8, 2, 3];
        let after = vec![0xC3, 0x28]; // 0xC3 0x28 is invalid UTF-8.
        let change = FileChange::Write {
            path: FilePath::new("/usr/bin/x"),
            source: FileSource::Path(FilePath::new("/host/x")),
            before: Some(Content::Bytes(before)),
            after: Content::Bytes(after),
        };
        let s = render_file_change(&change, DiffOptions::default()).to_plain_string();
        assert!(s.contains("<binary content"), "expected placeholder: {s}");
    }

    #[test]
    fn redacted_after_with_bytes_before_still_redacts() {
        // Even when only one side is Redacted, render placeholders both
        // sides - we don't trust ourselves to compute a "safe" diff that
        // mixes plaintext with a secret.
        let change = FileChange::Write {
            path: FilePath::new("/etc/secret"),
            source: FileSource::Secret("api-key".into()),
            before: Some(Content::Bytes(b"plain-before".to_vec())),
            after: Content::redacted(b"secret-after"),
        };
        let s = render_file_change(&change, DiffOptions::default()).to_plain_string();
        assert!(!s.contains("plain-before"), "before plaintext leaked: {s}");
        assert!(!s.contains("secret-after"), "after plaintext leaked: {s}");
        assert!(s.contains("<redacted:"), "no redacted placeholder: {s}");
    }

    #[test]
    fn truncate_respects_char_boundaries_for_unicode() {
        // Wide characters like ✓ are multi-byte. Truncate must not split
        // them; otherwise we'd produce invalid UTF-8 segments in the pane.
        let s = truncate("✓✓✓✓✓✓✓✓✓✓", 5);
        assert!(s.chars().count() <= 5);
        // Must be valid UTF-8 (assert by re-construction).
        let _ = s.as_str();
    }
}
