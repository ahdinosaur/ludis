//! Ratatui-based TUI for the apply pipeline. Two structural pages:
//!
//! - **Tree** (`1`): plan-item tree on the left with per-atom status badges,
//!   detail pane on the right (or stacked vertically below 100 columns).
//!   This is the default page and the surface most operators spend time on.
//! - **Stderr** (`e`): apply stderr scrollback.
//!
//! An **Epochs** (`2`) page is reserved for Task 13 - it shows a placeholder
//! until then.
//!
//! Input: crossterm events are read on a dedicated OS thread (blocking read)
//! and forwarded into a tokio mpsc channel so the main select loop stays
//! responsive. Terminal raw-mode is acquired via `ratatui::init` and
//! restored in the [`TerminalSession`]'s `Drop` so panics don't leave the
//! terminal in a bad state.
//!
//! [`plain`] is the non-interactive sibling: it folds the same `AppUpdate`s
//! into the same [`AppView`] but emits a human digest to stderr instead of
//! drawing. Selected when stdout is not a TTY or `--no-tui` is set.

#![allow(clippy::collapsible_if)]

mod palette;
mod plain;

pub use plain::plain;

use std::collections::HashSet;
use std::future::Future;
use std::io;
use std::io::IsTerminal;
use std::pin::Pin;

use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use lusid_apply_stdio::{AppUpdate, AppView, AppViewError, LeafState, Phase, ResourcesNode};
use lusid_cmd::CommandError;
use lusid_plan::{PlanMeta, PlanNodeId};
use lusid_render::{Palette as RenderPalette, Render, RenderedNode};
use lusid_ssh::SshError;
use ratatui::{
    CompletedFrame, DefaultTerminal, Frame,
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span, Text},
    widgets::{Block, Borders, List, ListItem, ListState, Paragraph, Wrap},
};
use serde_json::Error as SerdeJsonError;
use thiserror::Error;
use tokio::{
    io::{AsyncBufReadExt, AsyncRead, BufReader},
    sync::mpsc::{UnboundedReceiver, unbounded_channel},
};

use crate::tui::palette::{Badge, rollup};

/// True if the current process's stdout is connected to a terminal. The CLI
/// pairs this with `--no-tui` to choose between [`tui`] and [`plain`]: the
/// TUI is selected only when stdout is a TTY *and* the operator has not
/// opted out via `--no-tui`.
pub fn is_tty_stdout() -> bool {
    io::stdout().is_terminal()
}

#[derive(Error, Debug)]
pub enum TuiError {
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error("failed to parse apply stdout as json: {0}")]
    ParseApplyStdout(#[from] SerdeJsonError),

    #[error("failed to read stdout from apply")]
    ReadApplyStdout(#[source] tokio::io::Error),

    #[error("failed to read stderr from apply")]
    ReadApplyStderr(#[source] tokio::io::Error),

    #[error(transparent)]
    AppView(#[from] AppViewError),

    #[error("apply command failed: {0}")]
    Command(#[from] CommandError),

    #[error("ssh failed: {0}")]
    Ssh(#[from] SshError),

    #[error("failed to join task: {0}")]
    TaskJoin(#[from] tokio::task::JoinError),
}

/// Drive the TUI. Reads `stdout` line-by-line as JSON `AppUpdate`s and
/// `stderr` line-by-line as raw text, while racing a `wait` future that
/// resolves when the apply process exits. Returns when the user quits or
/// the wait future resolves; surfaces the apply's exit error if any.
///
/// `subcommand` is the human label shown in the header strip (e.g.
/// `"local apply"`, `"dev parse"`), so the operator can confirm at a glance
/// which command they're watching.
///
/// Generic over the IO and wait types so the same function works for a
/// subprocess (`lusid-cmd`) and an SSH command handle (`lusid-ssh`).
pub async fn tui<Stdout, Stderr, Wait, WaitError>(
    subcommand: &str,
    stdout: Stdout,
    stderr: Stderr,
    wait: Pin<Box<Wait>>,
) -> Result<(), TuiError>
where
    Stdout: AsyncRead + Unpin,
    Stderr: AsyncRead + Unpin,
    Wait: Future<Output = Result<(), WaitError>>,
    WaitError: Into<TuiError>,
{
    let mut terminal = TerminalSession::init();
    let mut app = TuiApp::new(subcommand.to_string());

    let mut stdout_lines = BufReader::new(stdout).lines();
    let mut stderr_lines = BufReader::new(stderr).lines();
    let mut stdout_done = false;
    let mut stderr_done = false;

    let mut events = read_events();

    let mut outcome: Option<Result<(), TuiError>> = None;
    let mut should_quit = false;

    tokio::pin!(wait);

    loop {
        terminal.draw(|frame| draw_ui(frame, &mut app, outcome.as_ref()))?;

        tokio::select! {
            result = &mut wait, if outcome.is_none() => {
                app.child_exited = true;
                outcome = Some(result.map_err(Into::into));
            }

            line = stdout_lines.next_line(), if !stdout_done => {
                match line {
                    Ok(Some(line)) => {
                        if !line.trim().is_empty() {
                            let update: AppUpdate = serde_json::from_str(&line)?;
                            app.apply_update(update)?;
                        }
                    }
                    Ok(None) => stdout_done = true,
                    Err(err) => return Err(err.into()),
                }
            }

            line = stderr_lines.next_line(), if !stderr_done => {
                match line {
                    Ok(Some(line)) => {
                        if !line.trim().is_empty() {
                            app.push_stderr(line)
                        }
                    }
                    Ok(None) => stderr_done = true,
                    Err(err) => return Err(err.into()),
                }
            }

            Some(event) = events.recv() => {
                should_quit = app.handle_event(event)?;
            }
        }

        if should_quit {
            break;
        }
    }

    match outcome {
        None => Ok(()),
        Some(result) => result,
    }
}

struct TerminalSession {
    terminal: DefaultTerminal,
}

impl TerminalSession {
    fn init() -> Self {
        let terminal = ratatui::init();
        Self { terminal }
    }

    pub fn draw<F>(&mut self, render_callback: F) -> Result<CompletedFrame<'_>, TuiError>
    where
        F: FnOnce(&mut Frame),
    {
        Ok(self.terminal.draw(render_callback)?)
    }
}

impl Drop for TerminalSession {
    fn drop(&mut self) {
        ratatui::restore();
    }
}

fn read_events() -> UnboundedReceiver<Event> {
    let (event_tx, event_rx) = unbounded_channel();

    std::thread::spawn(move || {
        loop {
            if let Ok(event) = crossterm::event::read() {
                if event_tx.send(event).is_err() {
                    break;
                }
            }
        }
    });

    event_rx
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UiPage {
    Tree,
    Epochs,
    Stderr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum DetailFocus {
    #[default]
    Tree,
    Detail,
}

/// State for the Tree page. `collapsed` is keyed by arena index in the
/// shipped `ResourcesTree`; `selected` likewise. `filter` dims rows whose
/// label doesn't substring-match (it doesn't remove them so navigation
/// stays predictable). `show_unchanged` toggles whether `Ok` (no-change)
/// leaves remain visible.
#[derive(Debug, Default, Clone)]
struct TreePageState {
    collapsed: HashSet<usize>,
    selected: Option<usize>,
    list_offset: usize,
    detail_focus: DetailFocus,
    detail_scroll: u16,
    show_unchanged: bool,
    filter: String,
    filter_editing: bool,
    /// `gg` chord support: set after the first `g`, reset on any other key.
    awaiting_g: bool,
}

impl TreePageState {
    fn new() -> Self {
        // Default to showing every row so operators see the full atom set
        // on first load. `u` toggles. The spec leaves the apply-time default
        // open; defaulting on keeps `Ok` rows visible until the operator
        // chooses to hide them.
        Self {
            show_unchanged: true,
            ..Self::default()
        }
    }

    fn toggle_collapse(&mut self, arena_index: usize) {
        if self.collapsed.contains(&arena_index) {
            self.collapsed.remove(&arena_index);
        } else {
            self.collapsed.insert(arena_index);
        }
    }

    fn is_expanded(&self, arena_index: usize) -> bool {
        !self.collapsed.contains(&arena_index)
    }

    fn ensure_visible_row(&mut self, selected_row: usize, height: usize) {
        if height == 0 {
            return;
        }
        let bottom = self.list_offset + height.saturating_sub(1);
        if selected_row < self.list_offset {
            self.list_offset = selected_row;
        } else if selected_row > bottom {
            self.list_offset = selected_row.saturating_sub(height.saturating_sub(1));
        }
    }
}

/// Top-level TUI state. Holds the folded `AppView` from the wire plus
/// per-page UI state. The header strip's `subcommand` label is the only
/// thing the operator passes from the CLI; everything else is derived from
/// the wire.
#[derive(Debug, Clone)]
struct TuiApp {
    app_view: AppView,
    subcommand: String,
    page: UiPage,

    tree: TreePageState,

    child_exited: bool,

    // Collect *all* stderr output.
    stderr_buffer: String,
    stderr_lines_count: usize,

    // stderr page UI state.
    stderr_scroll: u16,
    stderr_follow: bool,
    stderr_view_height: u16,
}

impl TuiApp {
    fn new(subcommand: String) -> Self {
        Self {
            app_view: AppView::default(),
            subcommand,
            page: UiPage::Tree,
            tree: TreePageState::new(),
            child_exited: false,
            stderr_buffer: String::new(),
            stderr_lines_count: 0,
            stderr_scroll: 0,
            stderr_follow: true,
            stderr_view_height: 0,
        }
    }

    fn apply_update(&mut self, update: AppUpdate) -> Result<(), TuiError> {
        let current = std::mem::take(&mut self.app_view);
        self.app_view = current.update(update)?;
        Ok(())
    }

    fn handle_event(&mut self, event: Event) -> Result<bool, TuiError> {
        let Event::Key(KeyEvent {
            code, modifiers, ..
        }) = event
        else {
            return Ok(false);
        };
        if modifiers != KeyModifiers::NONE && modifiers != KeyModifiers::SHIFT {
            return Ok(false);
        }

        match self.page {
            UiPage::Tree => Ok(self.handle_event_tree(code)),
            UiPage::Epochs => Ok(self.handle_event_epochs(code)),
            UiPage::Stderr => Ok(self.handle_event_stderr(code)),
        }
    }

    fn handle_event_tree(&mut self, code: KeyCode) -> bool {
        // Filter input mode captures most keys; Enter/Esc end it.
        if self.tree.filter_editing {
            match code {
                KeyCode::Esc => {
                    self.tree.filter_editing = false;
                    self.tree.filter.clear();
                }
                KeyCode::Enter => {
                    self.tree.filter_editing = false;
                }
                KeyCode::Backspace => {
                    self.tree.filter.pop();
                }
                KeyCode::Char(c) => {
                    self.tree.filter.push(c);
                }
                _ => {}
            }
            return false;
        }

        let was_awaiting_g = self.tree.awaiting_g;
        self.tree.awaiting_g = false;

        match code {
            KeyCode::Char('q') => return true,
            KeyCode::Esc => {
                if !self.tree.filter.is_empty() {
                    self.tree.filter.clear();
                } else {
                    return true;
                }
            }

            KeyCode::Char('1') => self.page = UiPage::Tree,
            KeyCode::Char('2') => self.page = UiPage::Epochs,
            KeyCode::Char('e') => {
                self.page = UiPage::Stderr;
                self.stderr_follow = true;
                self.stderr_scroll = u16::MAX;
            }

            KeyCode::Tab => {
                self.tree.detail_focus = match self.tree.detail_focus {
                    DetailFocus::Tree => DetailFocus::Detail,
                    DetailFocus::Detail => DetailFocus::Tree,
                };
            }

            KeyCode::Char('/') => {
                self.tree.filter_editing = true;
                self.tree.filter.clear();
            }

            KeyCode::Char('u') => {
                self.tree.show_unchanged = !self.tree.show_unchanged;
                self.clamp_tree_selection_to_visible();
            }

            KeyCode::Char('g') => {
                if was_awaiting_g {
                    self.tree_jump_first();
                } else {
                    self.tree.awaiting_g = true;
                }
            }
            KeyCode::Char('G') => self.tree_jump_last(),

            KeyCode::Down | KeyCode::Char('j') => self.tree_move_or_scroll(1),
            KeyCode::Up | KeyCode::Char('k') => self.tree_move_or_scroll(-1),

            KeyCode::Right | KeyCode::Char('l') => {
                if let Some(sel) = self.tree.selected
                    && is_branch(&self.app_view, sel)
                {
                    self.tree.collapsed.remove(&sel);
                }
            }

            KeyCode::Left | KeyCode::Char('h') => {
                if let Some(sel) = self.tree.selected
                    && is_branch(&self.app_view, sel)
                {
                    self.tree.collapsed.insert(sel);
                }
                // Collapsing a branch never hides its own row, but if the
                // selection points at a descendant of a newly-collapsed
                // branch the next clamp call cleans it up.
                self.clamp_tree_selection_to_visible();
            }

            KeyCode::Char(' ') => {
                if let Some(sel) = self.tree.selected
                    && is_branch(&self.app_view, sel)
                {
                    self.tree.toggle_collapse(sel);
                }
                self.clamp_tree_selection_to_visible();
            }

            _ => {}
        }

        false
    }

    fn handle_event_epochs(&mut self, code: KeyCode) -> bool {
        // Page-switch keys; everything else is a no-op until Task 13.
        // Esc returns to Tree to match standard TUI conventions; `q` is the
        // only way to quit, so the operator can't accidentally exit by
        // tapping Esc to dismiss the page.
        match code {
            KeyCode::Char('q') => return true,
            KeyCode::Esc => self.page = UiPage::Tree,
            KeyCode::Char('1') => self.page = UiPage::Tree,
            KeyCode::Char('2') => self.page = UiPage::Epochs,
            KeyCode::Char('e') => {
                self.page = UiPage::Stderr;
                self.stderr_follow = true;
                self.stderr_scroll = u16::MAX;
            }
            _ => {}
        }
        false
    }

    fn handle_event_stderr(&mut self, code: KeyCode) -> bool {
        match code {
            KeyCode::Char('q') => return true,
            KeyCode::Esc => self.page = UiPage::Tree,

            KeyCode::Char('1') => self.page = UiPage::Tree,
            KeyCode::Char('2') => self.page = UiPage::Epochs,
            KeyCode::Char('e') => self.page = UiPage::Tree,

            KeyCode::Up | KeyCode::Char('k') => self.stderr_scroll_up(1),
            KeyCode::Down | KeyCode::Char('j') => self.stderr_scroll_down(1),

            KeyCode::PageUp => {
                let step = self.stderr_view_height.max(1);
                self.stderr_scroll_up(step);
            }
            KeyCode::PageDown => {
                let step = self.stderr_view_height.max(1);
                self.stderr_scroll_down(step);
            }

            KeyCode::Home | KeyCode::Char('g') => {
                self.stderr_follow = false;
                self.stderr_scroll = 0;
            }
            KeyCode::End | KeyCode::Char('G') => {
                self.stderr_follow = true;
                self.stderr_scroll = u16::MAX;
            }

            _ => {}
        }
        false
    }

    fn tree_move_or_scroll(&mut self, delta: i32) {
        if self.tree.detail_focus == DetailFocus::Detail {
            if delta > 0 {
                self.tree.detail_scroll = self.tree.detail_scroll.saturating_add(delta as u16);
            } else {
                self.tree.detail_scroll = self.tree.detail_scroll.saturating_sub((-delta) as u16);
            }
        } else {
            self.tree_move(delta);
        }
    }

    fn tree_move(&mut self, delta: i32) {
        let rows = build_visible_rows(&self.app_view, &self.tree);
        if rows.is_empty() {
            self.tree.selected = None;
            self.tree.list_offset = 0;
            return;
        }
        let current = rows
            .iter()
            .position(|r| Some(r.arena_index) == self.tree.selected)
            .unwrap_or(0);
        let next = if delta >= 0 {
            (current + delta as usize).min(rows.len() - 1)
        } else {
            current.saturating_sub((-delta) as usize)
        };
        self.tree.selected = Some(rows[next].arena_index);
        self.tree.detail_scroll = 0;
    }

    fn tree_jump_first(&mut self) {
        let rows = build_visible_rows(&self.app_view, &self.tree);
        if let Some(first) = rows.first() {
            self.tree.selected = Some(first.arena_index);
        }
    }

    fn tree_jump_last(&mut self) {
        let rows = build_visible_rows(&self.app_view, &self.tree);
        if let Some(last) = rows.last() {
            self.tree.selected = Some(last.arena_index);
        }
    }

    /// After collapsing or hiding-Ok-leaves, the previously-selected arena
    /// index may no longer appear in `build_visible_rows`. Move the
    /// selection to the nearest still-visible row so the detail pane and
    /// future j/k presses operate on something the operator can see.
    ///
    /// "Nearest" is defined as "largest visible arena index that is
    /// `<= prev`". The resources tree ships in pre-order, so this lands on
    /// the parent branch when a descendant is hidden, and on the previous
    /// sibling otherwise — close to where the operator's eyes were.
    fn clamp_tree_selection_to_visible(&mut self) {
        let rows = build_visible_rows(&self.app_view, &self.tree);
        if rows.is_empty() {
            self.tree.selected = None;
            return;
        }
        let Some(prev) = self.tree.selected else {
            self.tree.selected = rows.first().map(|r| r.arena_index);
            return;
        };
        if rows.iter().any(|r| r.arena_index == prev) {
            return;
        }
        let fallback = rows
            .iter()
            .filter(|r| r.arena_index <= prev)
            .max_by_key(|r| r.arena_index)
            .or_else(|| rows.first());
        self.tree.selected = fallback.map(|r| r.arena_index);
    }

    fn push_stderr(&mut self, line: String) {
        if !self.stderr_buffer.is_empty() {
            self.stderr_buffer.push('\n');
        }
        self.stderr_buffer.push_str(&line);
        self.stderr_lines_count = self.stderr_lines_count.saturating_add(1);

        if self.page == UiPage::Stderr && self.stderr_follow {
            self.stderr_scroll = u16::MAX;
        }
    }

    fn stderr_scroll_up(&mut self, lines: u16) {
        self.stderr_follow = false;
        self.stderr_scroll = self.stderr_scroll.saturating_sub(lines);
    }

    fn stderr_scroll_down(&mut self, lines: u16) {
        self.stderr_follow = false;
        self.stderr_scroll = self.stderr_scroll.saturating_add(lines);
    }
}

// --------------------------------------------------------------------------
// Drawing
// --------------------------------------------------------------------------

fn draw_ui(frame: &mut ratatui::Frame, app: &mut TuiApp, outcome: Option<&Result<(), TuiError>>) {
    let layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints(
            [
                Constraint::Length(1), // header strip
                Constraint::Min(3),    // body
                Constraint::Length(1), // footer hints / filter prompt
            ]
            .as_ref(),
        )
        .split(frame.area());

    draw_header(frame, layout[0], app, outcome);
    match app.page {
        UiPage::Tree => draw_tree_page(frame, layout[1], app),
        UiPage::Epochs => draw_epochs_page(frame, layout[1], app),
        UiPage::Stderr => draw_stderr_page(frame, layout[1], app),
    }
    draw_footer(frame, layout[2], app);
}

fn draw_header(
    frame: &mut ratatui::Frame,
    area: Rect,
    app: &TuiApp,
    outcome: Option<&Result<(), TuiError>>,
) {
    let (status, status_style) = status_summary(app, outcome);
    let spans: Vec<Span> = vec![
        Span::styled(
            "lusid",
            Style::default()
                .fg(Color::Cyan)
                .add_modifier(Modifier::BOLD),
        ),
        Span::raw(" · "),
        Span::raw(app.subcommand.clone()),
        Span::raw(" · "),
        Span::raw(epoch_label(&app.app_view)),
        Span::raw(" · "),
        Span::styled(status, status_style),
    ];

    let widget = Paragraph::new(Line::from(spans)).alignment(Alignment::Left);
    frame.render_widget(widget, area);
}

/// 1-based "epoch K/N" indicator. `?/?` until `PipelineInfo` arrives so the
/// strip doesn't bake a `0/?` placeholder into the operator's first frame.
/// During apply, `K` is the resource epoch the most recently emitted op
/// epoch belongs to (Phase A and B both report the same K); after
/// `ApplyComplete`, `K = N`.
fn epoch_label(view: &AppView) -> String {
    let Some(total) = view.resource_epochs_total() else {
        return "epoch ?/?".to_string();
    };
    let current = if view.done {
        total
    } else if let Some(last) = view.operation_epoch_meta.last() {
        last.resource_epoch + 1
    } else {
        0
    };
    format!("epoch {current}/{total}")
}

fn status_summary(app: &TuiApp, outcome: Option<&Result<(), TuiError>>) -> (String, Style) {
    if let Some(Err(err)) = outcome {
        return (
            format!("process error: {err}"),
            Style::default().fg(Color::Red).add_modifier(Modifier::BOLD),
        );
    }
    let view = &app.app_view;
    if view.done {
        if !view.had_changes {
            ("no changes".to_string(), Style::default().fg(Color::Green))
        } else if app.child_exited {
            ("complete".to_string(), Style::default().fg(Color::Green))
        } else {
            (
                "complete (waiting for process)".to_string(),
                Style::default().fg(Color::Yellow),
            )
        }
    } else if !view.operations_epochs.is_empty() {
        ("applying".to_string(), Style::default().fg(Color::Blue))
    } else if view.resources.is_some() {
        ("planning".to_string(), Style::default().fg(Color::Yellow))
    } else if view.resource_params.is_some() {
        (
            "expanding resources".to_string(),
            Style::default().fg(Color::Yellow),
        )
    } else {
        (
            "waiting for plan".to_string(),
            Style::default().fg(Color::DarkGray),
        )
    }
}

fn draw_footer(frame: &mut ratatui::Frame, area: Rect, app: &TuiApp) {
    let line = if app.page == UiPage::Tree && app.tree.filter_editing {
        Line::from(vec![
            Span::styled("/", Style::default().fg(Color::Yellow)),
            Span::raw(app.tree.filter.clone()),
            Span::styled(
                "  (Enter to apply, Esc to clear)",
                Style::default().fg(Color::DarkGray),
            ),
        ])
    } else {
        let hint = footer_hint(app);
        Line::from(Span::styled(hint, Style::default().fg(Color::DarkGray)))
    };
    let widget = Paragraph::new(line).alignment(Alignment::Left);
    frame.render_widget(widget, area);
}

fn footer_hint(app: &TuiApp) -> String {
    match app.page {
        UiPage::Tree => "j/k move  h/l collapse/expand  Tab focus  / filter  u show-unchanged  \
             gg/G first/last  1/2/e pages  q quit"
            .to_string(),
        UiPage::Epochs => "1/2/e pages  q quit  (Task 13 will fill this in)".to_string(),
        UiPage::Stderr => {
            "j/k scroll  PgUp/PgDn page  g/G top/bottom  1/2/e pages  q quit".to_string()
        }
    }
}

// --------------------------------------------------------------------------
// Tree page
// --------------------------------------------------------------------------

fn draw_tree_page(frame: &mut ratatui::Frame, area: Rect, app: &mut TuiApp) {
    let resources = app.app_view.resources.as_ref();
    if resources.is_none() {
        draw_placeholder(frame, area, "Waiting for resources tree...");
        return;
    }

    // Lazy-default the selection to the first visible row.
    if app.tree.selected.is_none() {
        let rows = build_visible_rows(&app.app_view, &app.tree);
        app.tree.selected = rows.first().map(|r| r.arena_index);
    }

    let layout = if area.width >= 100 {
        Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)].as_ref())
            .split(area)
    } else {
        Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Percentage(60), Constraint::Percentage(40)].as_ref())
            .split(area)
    };

    draw_tree_list(frame, layout[0], app);
    draw_detail_pane(frame, layout[1], app);
}

#[derive(Debug, Clone)]
struct TreeRow {
    arena_index: usize,
    depth: usize,
    is_branch: bool,
    badge: Badge,
    label: String,
    dim: bool,
}

/// Atoms tree root is at arena index 0 by convention (see
/// `lusid_apply_stdio::ResourcesNode`).
const ROOT_ARENA_INDEX: usize = 0;

fn build_visible_rows(view: &AppView, state: &TreePageState) -> Vec<TreeRow> {
    let mut out = Vec::new();
    let Some(resources) = view.resources.as_ref() else {
        return out;
    };
    let filter = if state.filter.is_empty() {
        None
    } else {
        Some(state.filter.as_str())
    };
    walk_for_rows(
        resources,
        ROOT_ARENA_INDEX,
        0,
        state,
        filter,
        &mut out,
        &mut HashSet::new(),
    );
    out
}

fn walk_for_rows(
    resources: &lusid_apply_stdio::ResourcesTree,
    arena_index: usize,
    depth: usize,
    state: &TreePageState,
    filter: Option<&str>,
    out: &mut Vec<TreeRow>,
    visited: &mut HashSet<usize>,
) {
    if !visited.insert(arena_index) {
        return;
    }
    let Some(slot) = resources.nodes.get(arena_index).and_then(Option::as_ref) else {
        return;
    };
    match slot {
        ResourcesNode::Branch { meta, children } => {
            let label = plan_meta_short_label(meta);
            let badge = rollup_for_branch(resources, arena_index);
            let dim = match filter {
                Some(f) => !label.to_lowercase().contains(&f.to_lowercase()),
                None => false,
            };
            out.push(TreeRow {
                arena_index,
                depth,
                is_branch: true,
                badge,
                label,
                dim,
            });
            if state.is_expanded(arena_index) {
                for &child in children {
                    walk_for_rows(resources, child, depth + 1, state, filter, out, visited);
                }
            }
        }
        ResourcesNode::Leaf { state: leaf_state } => {
            let badge = badge_for_leaf(leaf_state);
            if !state.show_unchanged && badge == Badge::Ok {
                return;
            }
            let label = leaf_state.resource().render().to_plain_string();
            let dim = match filter {
                Some(f) => !label.to_lowercase().contains(&f.to_lowercase()),
                None => false,
            };
            out.push(TreeRow {
                arena_index,
                depth,
                is_branch: false,
                badge,
                label,
                dim,
            });
        }
    }
}

fn draw_tree_list(frame: &mut ratatui::Frame, area: Rect, app: &mut TuiApp) {
    let rows = build_visible_rows(&app.app_view, &app.tree);

    let selected_row = rows
        .iter()
        .position(|r| Some(r.arena_index) == app.tree.selected);

    let items: Vec<ListItem> = rows
        .iter()
        .map(|row| {
            let mut spans: Vec<Span> = Vec::new();
            spans.push(Span::raw("  ".repeat(row.depth)));
            let badge_style = if row.dim {
                row.badge.style().add_modifier(Modifier::DIM)
            } else {
                row.badge.style()
            };
            spans.push(Span::styled(format!("{} ", row.badge.glyph()), badge_style));
            if row.is_branch {
                spans.push(Span::styled(
                    if app.tree.is_expanded(row.arena_index) {
                        "▼ "
                    } else {
                        "▶ "
                    },
                    Style::default().fg(Color::DarkGray),
                ));
            }
            let label_style = if row.dim {
                Style::default().add_modifier(Modifier::DIM)
            } else {
                Style::default()
            };
            spans.push(Span::styled(row.label.clone(), label_style));
            ListItem::new(Line::from(spans))
        })
        .collect();

    let mut list_state = ListState::default();
    list_state.select(selected_row);
    *list_state.offset_mut() = app.tree.list_offset;

    let inner_height = area.height.saturating_sub(2) as usize;
    if let Some(row) = selected_row {
        app.tree.ensure_visible_row(row, inner_height);
        *list_state.offset_mut() = app.tree.list_offset;
    }

    let title = if app.tree.detail_focus == DetailFocus::Tree {
        "tree (focused)"
    } else {
        "tree"
    };
    let widget = List::new(items)
        .block(Block::default().borders(Borders::ALL).title(title))
        .highlight_style(
            Style::default()
                .fg(Color::Cyan)
                .add_modifier(Modifier::BOLD),
        );

    frame.render_stateful_widget(widget, area, &mut list_state);
}

fn draw_detail_pane(frame: &mut ratatui::Frame, area: Rect, app: &TuiApp) {
    let render_palette = RenderPalette::default();
    let text = match app.tree.selected {
        Some(arena_index) => detail_for_node(&app.app_view, arena_index, &render_palette),
        None => Text::from("(no selection)"),
    };

    let title = if app.tree.detail_focus == DetailFocus::Detail {
        "detail (focused)"
    } else {
        "detail"
    };
    let widget = Paragraph::new(text)
        .block(Block::default().borders(Borders::ALL).title(title))
        .wrap(Wrap { trim: false })
        .scroll((app.tree.detail_scroll, 0));
    frame.render_widget(widget, area);
}

/// Build the detail content for a given arena index. Branches get
/// plan-item metadata; leaves get the per-lifecycle content table.
fn detail_for_node(view: &AppView, arena_index: usize, palette: &RenderPalette) -> Text<'static> {
    let Some(resources) = view.resources.as_ref() else {
        return Text::from("(no resources)");
    };
    let Some(slot) = resources.nodes.get(arena_index).and_then(Option::as_ref) else {
        return Text::from("(missing slot)");
    };
    match slot {
        ResourcesNode::Branch { meta, children } => detail_for_branch(meta, children, palette),
        ResourcesNode::Leaf { state } => detail_for_leaf(state, palette),
    }
}

fn detail_for_branch(
    meta: &PlanMeta,
    children: &[usize],
    palette: &RenderPalette,
) -> Text<'static> {
    let mut lines: Vec<Line<'static>> = Vec::new();
    lines.push(section_header("Plan item"));
    let id_text = meta
        .id
        .as_ref()
        .map(|id| id.render().to_plain_string())
        .unwrap_or_else(|| "(anonymous)".to_string());
    lines.push(field_line("id", &id_text));
    lines.push(field_line(
        "children",
        &format!("{} atom(s)", children.len()),
    ));
    if !meta.requires.is_empty() {
        lines.push(blank_line());
        lines.push(section_header("Requires"));
        for r in &meta.requires {
            lines.push(bullet_line(&r.render().to_plain_string()));
        }
    }
    if !meta.required_by.is_empty() {
        lines.push(blank_line());
        lines.push(section_header("Required by"));
        for r in &meta.required_by {
            lines.push(bullet_line(&r.render().to_plain_string()));
        }
    }
    if !meta.handlers.is_empty() {
        lines.push(blank_line());
        lines.push(section_header("on_change handlers"));
        for h in &meta.handlers {
            extend_lines(&mut lines, &h.render(), palette);
        }
    }
    Text::from(lines)
}

fn detail_for_leaf(state: &LeafState, palette: &RenderPalette) -> Text<'static> {
    let mut lines: Vec<Line<'static>> = Vec::new();
    lines.push(section_header("Resource"));
    extend_lines(&mut lines, &state.resource().render(), palette);

    match state {
        LeafState::Planned { .. } => {
            lines.push(blank_line());
            lines.push(status_line("Planned (not started)"));
        }
        LeafState::Probing { .. } => {
            lines.push(blank_line());
            lines.push(status_line("Probing current state..."));
        }
        LeafState::Probed {
            state: probed_state,
            ..
        } => {
            lines.push(blank_line());
            lines.push(section_header("Current state"));
            extend_lines(&mut lines, &probed_state.render(), palette);
        }
        LeafState::NoChange {
            state: probed_state,
            ..
        } => {
            lines.push(blank_line());
            lines.push(section_header("Current state"));
            extend_lines(&mut lines, &probed_state.render(), palette);
            lines.push(blank_line());
            lines.push(status_line("No change"));
        }
        LeafState::Changed {
            state: probed_state,
            change,
            ops,
            ..
        } => {
            lines.push(blank_line());
            lines.push(section_header("Current state"));
            extend_lines(&mut lines, &probed_state.render(), palette);
            lines.push(blank_line());
            lines.push(section_header("Change"));
            extend_lines(&mut lines, &change.render(), palette);
            if let Some((ops_tree, _)) = ops {
                lines.push(blank_line());
                lines.push(section_header("Operations"));
                for_each_plan_leaf(ops_tree, &mut |op| {
                    extend_lines(&mut lines, &op.render(), palette);
                });
            }
        }
    }

    Text::from(lines)
}

/// Walk a `PlanTree<T>` and call `visit` on every leaf, in arena order.
/// Generic because `lusid-operation` is only a dev-dep here, so the
/// caller passes the inferred type rather than naming it.
fn for_each_plan_leaf<T, F: FnMut(&T)>(tree: &lusid_plan::PlanTree<T>, visit: &mut F) {
    match tree {
        lusid_plan::PlanTree::Leaf { node, .. } => visit(node),
        lusid_plan::PlanTree::Branch { children, .. } => {
            for child in children {
                for_each_plan_leaf(child, visit);
            }
        }
    }
}

fn section_header(text: &str) -> Line<'static> {
    Line::from(Span::styled(
        text.to_string(),
        Style::default()
            .fg(Color::Cyan)
            .add_modifier(Modifier::BOLD),
    ))
}

fn field_line(name: &str, value: &str) -> Line<'static> {
    Line::from(vec![
        Span::styled(format!("{name}: "), Style::default().fg(Color::DarkGray)),
        Span::raw(value.to_string()),
    ])
}

fn bullet_line(text: &str) -> Line<'static> {
    Line::from(vec![
        Span::styled("  • ", Style::default().fg(Color::DarkGray)),
        Span::raw(text.to_string()),
    ])
}

fn status_line(text: &str) -> Line<'static> {
    Line::from(Span::styled(
        text.to_string(),
        Style::default()
            .fg(Color::Yellow)
            .add_modifier(Modifier::ITALIC),
    ))
}

fn blank_line() -> Line<'static> {
    Line::from(Span::raw(""))
}

/// Lower a `RenderedNode` into the existing `lines` buffer with the
/// supplied render palette. Multi-line content from `to_ratatui_text` is
/// flattened in order.
fn extend_lines(lines: &mut Vec<Line<'static>>, node: &RenderedNode, palette: &RenderPalette) {
    for line in node.to_ratatui_text(palette).lines {
        lines.push(line);
    }
}

// --------------------------------------------------------------------------
// Epochs page (placeholder for Task 13)
// --------------------------------------------------------------------------

fn draw_epochs_page(frame: &mut ratatui::Frame, area: Rect, app: &TuiApp) {
    let view = &app.app_view;
    let mut lines: Vec<Line<'static>> = Vec::new();
    let phase_a_count = view
        .operation_epoch_meta
        .iter()
        .filter(|m| m.phase == Phase::A)
        .count();
    let phase_b_count = view.operation_epoch_meta.len() - phase_a_count;
    let total = view
        .resource_epochs_total()
        .map(|n| n.to_string())
        .unwrap_or_else(|| "?".to_string());
    lines.push(Line::from(Span::raw(format!(
        "{total} resource epoch(s), {phase_a_count} Phase A, {phase_b_count} Phase B logged",
    ))));
    lines.push(blank_line());
    lines.push(Line::from(Span::styled(
        "Epochs page is reserved for Task 13. Use `1` to return to Tree.",
        Style::default().fg(Color::DarkGray),
    )));
    let widget = Paragraph::new(Text::from(lines))
        .block(Block::default().borders(Borders::ALL).title("epochs"))
        .wrap(Wrap { trim: false });
    frame.render_widget(widget, area);
}

// --------------------------------------------------------------------------
// Stderr page
// --------------------------------------------------------------------------

fn draw_stderr_page(frame: &mut ratatui::Frame, area: Rect, app: &mut TuiApp) {
    let inner_height = area.height.saturating_sub(2) as usize;
    app.stderr_view_height = inner_height as u16;

    let total_lines = app.stderr_lines_count.max(1);
    let max_scroll = total_lines.saturating_sub(inner_height) as u16;

    if app.stderr_follow || app.stderr_scroll > max_scroll {
        app.stderr_scroll = max_scroll;
    }

    let title = if app.stderr_follow {
        "stderr (following)"
    } else {
        "stderr"
    };

    let widget = if app.stderr_buffer.is_empty() {
        Paragraph::new("<no stderr output>")
            .block(Block::default().borders(Borders::ALL).title(title))
            .alignment(Alignment::Left)
            .wrap(Wrap { trim: false })
            .style(Style::default().fg(Color::DarkGray))
    } else {
        Paragraph::new(app.stderr_buffer.as_str())
            .block(Block::default().borders(Borders::ALL).title(title))
            .alignment(Alignment::Left)
            .wrap(Wrap { trim: false })
            .scroll((app.stderr_scroll, 0))
            .style(Style::default().fg(Color::Red))
    };

    frame.render_widget(widget, area);
}

fn draw_placeholder(frame: &mut ratatui::Frame, area: Rect, text: &str) {
    let widget = Paragraph::new(Text::from(text))
        .block(Block::default().borders(Borders::ALL))
        .alignment(Alignment::Center);
    frame.render_widget(widget, area);
}

// --------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------

/// Short user-friendly label for a `PlanNodeId`, suitable for tree rows.
/// Unlike `PlanNodeId::Display` (which spells out the full plan path) this
/// returns just the user-authored item id, falling back to `.` for the
/// anonymous root.
fn plan_node_short_label(id: &PlanNodeId) -> String {
    match id {
        PlanNodeId::PlanItem { item_id, .. } | PlanNodeId::SubItem { item_id, .. } => {
            item_id.clone()
        }
        PlanNodeId::Plan(plan_id) => plan_id.to_string(),
    }
}

fn plan_meta_short_label(meta: &PlanMeta) -> String {
    meta.id
        .as_ref()
        .map(plan_node_short_label)
        .unwrap_or_else(|| ".".to_string())
}

fn is_branch(view: &AppView, arena_index: usize) -> bool {
    matches!(
        view.resources
            .as_ref()
            .and_then(|t| t.nodes.get(arena_index).and_then(Option::as_ref)),
        Some(ResourcesNode::Branch { .. })
    )
}

fn badge_for_leaf(state: &LeafState) -> Badge {
    match state {
        LeafState::Planned { .. } => Badge::Planned,
        LeafState::Probing { .. } | LeafState::Probed { .. } => Badge::Running,
        LeafState::NoChange { .. } => Badge::Ok,
        LeafState::Changed { .. } => Badge::Changed,
    }
}

/// Roll up the badges of every descendant atom under the branch at
/// `arena_index`. Empty branches (no descendant atoms) report `Ok`; the
/// rollup precedence in [`palette::rollup`] then composes children.
fn rollup_for_branch(resources: &lusid_apply_stdio::ResourcesTree, arena_index: usize) -> Badge {
    let mut acc = Badge::Ok;
    let mut saw_any = false;
    walk_atoms(resources, arena_index, &mut HashSet::new(), &mut |b| {
        acc = if saw_any { rollup(acc, b) } else { b };
        saw_any = true;
    });
    if saw_any { acc } else { Badge::Ok }
}

fn walk_atoms<F: FnMut(Badge)>(
    resources: &lusid_apply_stdio::ResourcesTree,
    arena_index: usize,
    visited: &mut HashSet<usize>,
    visit: &mut F,
) {
    if !visited.insert(arena_index) {
        return;
    }
    let Some(slot) = resources.nodes.get(arena_index).and_then(Option::as_ref) else {
        return;
    };
    match slot {
        ResourcesNode::Branch { children, .. } => {
            for &child in children {
                walk_atoms(resources, child, visited, visit);
            }
        }
        ResourcesNode::Leaf { state } => visit(badge_for_leaf(state)),
    }
}

// --------------------------------------------------------------------------
// Tests
// --------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use lusid_apply_stdio::AppUpdate;
    use lusid_operation::operations::file::FilePath;
    use lusid_plan::{PlanId, PlanMeta, PlanNodeId, PlanTree};
    use lusid_resource::{
        Resource, ResourceState,
        file::{FileResource, FileState},
    };
    use std::path::PathBuf;

    fn pi_id(item: &str) -> PlanNodeId {
        PlanNodeId::PlanItem {
            plan_id: PlanId::Path(PathBuf::from("plan.lusid")),
            item_id: item.into(),
        }
    }

    fn resource_leaf(path: &str) -> PlanTree<Resource> {
        PlanTree::Leaf {
            meta: PlanMeta::default(),
            node: Resource::File(FileResource::Present {
                path: FilePath::new(path),
            }),
        }
    }

    fn view_with_two_branches() -> AppView {
        let alpha = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(pi_id("alpha")),
                ..PlanMeta::default()
            },
            children: vec![resource_leaf("/a/1"), resource_leaf("/a/2")],
        };
        let beta = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(pi_id("beta")),
                ..PlanMeta::default()
            },
            children: vec![resource_leaf("/b/1")],
        };
        let root = PlanTree::Branch {
            meta: PlanMeta::default(),
            children: vec![alpha, beta],
        };
        AppView::default()
            .update(AppUpdate::ResourcesStart)
            .unwrap()
            .update(AppUpdate::ResourcesNode {
                index: 0,
                tree: root,
            })
            .unwrap()
    }

    #[test]
    fn short_label_drops_path_noise() {
        let id = pi_id("nginx-config");
        assert_eq!(plan_node_short_label(&id), "nginx-config");
        let sub = PlanNodeId::SubItem {
            scope_id: "scope".into(),
            item_id: "file".into(),
        };
        assert_eq!(plan_node_short_label(&sub), "file");
    }

    #[test]
    fn anonymous_branch_short_label_is_dot() {
        let meta = PlanMeta::default();
        assert_eq!(plan_meta_short_label(&meta), ".");
    }

    #[test]
    fn rollup_combines_child_badges() {
        // Two planned leaves -> branch is Planned.
        let view = view_with_two_branches();
        let resources = view.resources.as_ref().unwrap();
        // Arena: 0=root, 1=alpha, 2=/a/1, 3=/a/2, 4=beta, 5=/b/1
        assert_eq!(rollup_for_branch(resources, 1), Badge::Planned);
        assert_eq!(rollup_for_branch(resources, 0), Badge::Planned);
    }

    #[test]
    fn rollup_escalates_on_running_child() {
        let view = view_with_two_branches()
            .update(AppUpdate::ResourceStatesNodeStart { index: 2 })
            .unwrap();
        let resources = view.resources.as_ref().unwrap();
        assert_eq!(rollup_for_branch(resources, 1), Badge::Running);
        // Sibling branch beta untouched.
        assert_eq!(rollup_for_branch(resources, 4), Badge::Planned);
        // Root inherits Running from alpha.
        assert_eq!(rollup_for_branch(resources, 0), Badge::Running);
    }

    #[test]
    fn rollup_changed_beats_ok() {
        let view = view_with_two_branches()
            .update(AppUpdate::ResourceStatesNodeStart { index: 2 })
            .unwrap()
            .update(AppUpdate::ResourceStatesNodeComplete {
                index: 2,
                state: ResourceState::File(FileState::Absent),
            })
            .unwrap()
            .update(AppUpdate::ResourceChangesNode {
                index: 2,
                change: None,
            })
            .unwrap()
            .update(AppUpdate::ResourceStatesNodeStart { index: 3 })
            .unwrap()
            .update(AppUpdate::ResourceStatesNodeComplete {
                index: 3,
                state: ResourceState::File(FileState::Absent),
            })
            .unwrap()
            .update(AppUpdate::ResourceChangesNode {
                index: 3,
                change: Some(lusid_resource::ResourceChange::Apt(
                    lusid_resource::apt::AptChange::Install {
                        package: "nginx".into(),
                    },
                )),
            })
            .unwrap();
        let resources = view.resources.as_ref().unwrap();
        assert_eq!(rollup_for_branch(resources, 1), Badge::Changed);
    }

    #[test]
    fn filter_dims_non_matches_without_removing() {
        let view = view_with_two_branches();
        let mut state = TreePageState::new();
        state.filter = "beta".into();
        let rows = build_visible_rows(&view, &state);
        let labels: Vec<(String, bool)> = rows.iter().map(|r| (r.label.clone(), r.dim)).collect();
        // Every branch + leaf is present; only "beta" branch and its leaf are
        // un-dimmed (substring match).
        assert!(labels.iter().any(|(l, d)| l == "beta" && !*d));
        assert!(labels.iter().any(|(l, d)| l == "alpha" && *d));
    }

    #[test]
    fn show_unchanged_false_hides_ok_leaves() {
        let view = view_with_two_branches()
            .update(AppUpdate::ResourceStatesNodeStart { index: 2 })
            .unwrap()
            .update(AppUpdate::ResourceStatesNodeComplete {
                index: 2,
                state: ResourceState::File(FileState::Absent),
            })
            .unwrap()
            .update(AppUpdate::ResourceChangesNode {
                index: 2,
                change: None,
            })
            .unwrap();
        let mut state = TreePageState::new();
        state.show_unchanged = false;
        let rows = build_visible_rows(&view, &state);
        // The Ok leaf at arena index 2 is hidden; the others remain.
        assert!(rows.iter().all(|r| r.arena_index != 2));
        assert!(rows.iter().any(|r| r.arena_index == 3));
    }

    /// A filter that doesn't match anything still leaves every row visible
    /// (just dimmed). Navigation must keep working — `build_visible_rows`
    /// returns the full set so j/k still finds rows to land on.
    #[test]
    fn filter_with_no_match_dims_all_but_keeps_rows() {
        let view = view_with_two_branches();
        let mut state = TreePageState::new();
        state.filter = "no-such-id".into();
        let rows = build_visible_rows(&view, &state);
        assert!(!rows.is_empty(), "rows must remain navigable");
        assert!(rows.iter().all(|r| r.dim), "every row dimmed");
    }

    /// Hiding Ok leaves while one of them is selected must move the
    /// selection to its parent branch (the nearest still-visible ancestor
    /// in pre-order), not all the way back to the root.
    #[test]
    fn toggling_show_unchanged_clamps_selection_to_parent_branch() {
        let view = view_with_two_branches()
            .update(AppUpdate::ResourceStatesNodeStart { index: 2 })
            .unwrap()
            .update(AppUpdate::ResourceStatesNodeComplete {
                index: 2,
                state: ResourceState::File(FileState::Absent),
            })
            .unwrap()
            .update(AppUpdate::ResourceChangesNode {
                index: 2,
                change: None,
            })
            .unwrap();
        let mut app = TuiApp::new("test".into());
        app.app_view = view;
        // Arena: 0=root, 1=alpha, 2=/a/1 (now NoChange), 3=/a/2, 4=beta, 5=/b/1
        app.tree.selected = Some(2);

        app.handle_event_tree(KeyCode::Char('u'));

        assert!(!app.tree.show_unchanged);
        assert_eq!(
            app.tree.selected,
            Some(1),
            "selection should land on the parent branch (alpha), not bounce to root",
        );
    }
}
