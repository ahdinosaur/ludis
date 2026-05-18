//! Ratatui-based TUI for the apply pipeline. Three structural pages:
//!
//! - **Tree** (`1`): plan-item tree on the left with per-atom status badges,
//!   detail pane on the right (or stacked vertically below 100 columns).
//!   This is the default page and the surface most operators spend time on.
//! - **Epochs** (`2`): one section per resource epoch, each showing the
//!   epoch's atoms, Phase A ops, and Phase B handlers, with a detail pane on
//!   the right (same layout breakpoint as Tree).
//! - **Stderr** (`e`): apply stderr scrollback.
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

use std::collections::{BTreeMap, HashMap, HashSet};
use std::future::Future;
use std::io;
use std::io::IsTerminal;
use std::pin::Pin;

use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use lusid_apply_stdio::{
    AckAction, AppUpdate, AppView, AppViewError, LeafState, OperationView, Phase, ResourcesNode,
};
use lusid_cmd::CommandError;
use lusid_plan::{PlanMeta, PlanNodeId};
use lusid_render::{DiffOptions, Palette as RenderPalette, Render, RenderedNode, render_change};
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
    io::{AsyncBufReadExt, AsyncRead, AsyncWrite, AsyncWriteExt, BufReader},
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

    #[error("failed to write ack to apply stdin: {0}")]
    WriteAck(#[source] tokio::io::Error),

    #[error("failed to serialize ack: {0}")]
    SerializeAck(#[source] SerdeJsonError),
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
/// `stdin` is the apply child's stdin handle, held for the lifetime of the
/// apply so per-epoch confirm acks can be written back.
///
/// Generic over the IO and wait types so the same function works for a
/// subprocess (`lusid-cmd`) and an SSH command handle (`lusid-ssh`).
pub async fn tui<Stdin, Stdout, Stderr, Wait, WaitError>(
    subcommand: &str,
    mut stdin: Stdin,
    stdout: Stdout,
    stderr: Stderr,
    wait: Pin<Box<Wait>>,
) -> Result<(), TuiError>
where
    Stdin: AsyncWrite + Unpin,
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

        if let Some(ack) = app.pending_ack.take() {
            write_ack(&mut stdin, ack).await?;
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

/// Serialize an [`AckAction`] as one JSON line and flush. Called at most
/// once per `EpochReady` after the operator presses an accept/reject key.
async fn write_ack<W>(stdin: &mut W, ack: AckAction) -> Result<(), TuiError>
where
    W: AsyncWrite + Unpin,
{
    let mut bytes = serde_json::to_vec(&ack).map_err(TuiError::SerializeAck)?;
    bytes.push(b'\n');
    stdin.write_all(&bytes).await.map_err(TuiError::WriteAck)?;
    stdin.flush().await.map_err(TuiError::WriteAck)?;
    Ok(())
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
    /// Transient message shown in the footer until the next keypress.
    /// Used today by `n`/`N` when the jump-to-change search finds nothing.
    toast: Option<String>,
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

/// State for the Epochs page. Selection cycles across every visible row -
/// epoch headers, atoms, phase headers, individual ops - via [`EpochsCursor`],
/// which stays stable across data arrivals (collapse/uncollapse, new ops)
/// even as the underlying row list reshuffles. `collapsed` is keyed by the
/// resource epoch index.
#[derive(Debug, Default, Clone)]
struct EpochsPageState {
    collapsed: HashSet<usize>,
    selected: Option<EpochsCursor>,
    list_offset: usize,
    detail_focus: DetailFocus,
    detail_scroll: u16,
    awaiting_g: bool,
}

impl EpochsPageState {
    fn toggle_collapse(&mut self, epoch: usize) {
        if self.collapsed.contains(&epoch) {
            self.collapsed.remove(&epoch);
        } else {
            self.collapsed.insert(epoch);
        }
    }

    fn is_expanded(&self, epoch: usize) -> bool {
        !self.collapsed.contains(&epoch)
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

/// Stable selection key for the Epochs page. Survives row reshuffles when
/// epochs collapse/uncollapse or new ops arrive; the renderer maps it back to
/// the current row index each frame via [`build_epochs_rows`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EpochsCursor {
    EpochHeader { epoch: usize },
    Atom { arena_index: usize },
    PhaseHeader { epoch: usize, phase: Phase },
    Op { epoch_index: usize, op_index: usize },
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
    epochs: EpochsPageState,

    child_exited: bool,

    // Collect *all* stderr output.
    stderr_buffer: String,
    stderr_lines_count: usize,

    // stderr page UI state.
    stderr_scroll: u16,
    stderr_follow: bool,
    stderr_view_height: u16,

    /// `s` toggles side-by-side diff in the detail pane. Honored only when
    /// the detail pane is at least 140 cols wide; the renderer otherwise
    /// falls back to unified. State persists across width changes so the
    /// operator's preference survives a temporary resize below the
    /// breakpoint.
    side_by_side: bool,

    /// Detail-pane width captured during the last frame, used to gate the
    /// `s` toggle hint and the side-by-side fallback. 0 until the first
    /// frame draws.
    last_detail_width: u16,

    /// Set by `handle_event` when the operator answers a per-epoch confirm
    /// prompt. The main loop drains it into the apply child's stdin after
    /// each event so the keypress and the wire write stay in lockstep.
    pending_ack: Option<AckAction>,

    /// `?` toggles a modal help overlay listing every key binding. While on,
    /// the page's per-key handler is bypassed (except for the confirm-prompt
    /// keys, which must always reach the ack channel).
    show_help: bool,
}

impl TuiApp {
    fn new(subcommand: String) -> Self {
        Self {
            app_view: AppView::default(),
            subcommand,
            page: UiPage::Tree,
            tree: TreePageState::new(),
            epochs: EpochsPageState::default(),
            child_exited: false,
            stderr_buffer: String::new(),
            stderr_lines_count: 0,
            stderr_scroll: 0,
            stderr_follow: true,
            stderr_view_height: 0,
            side_by_side: false,
            last_detail_width: 0,
            pending_ack: None,
            show_help: false,
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

        // While a per-epoch confirm is on screen, intercept the y/n/Enter/Esc
        // keys before they reach the page handler. Other keys (j/k, page
        // switches, q, ...) pass through so the operator can scroll the tree
        // / detail pane to inspect what they're about to ack. The help
        // overlay does not block these keys: the operator must never lose
        // the ack channel while help is open.
        if self.app_view.pending_epoch.is_some() {
            match code {
                KeyCode::Char('y') | KeyCode::Enter => {
                    self.pending_ack = Some(AckAction::Apply);
                    self.app_view.pending_epoch = None;
                    return Ok(false);
                }
                KeyCode::Char('n') | KeyCode::Esc => {
                    self.pending_ack = Some(AckAction::Abort);
                    self.app_view.pending_epoch = None;
                    return Ok(false);
                }
                _ => {}
            }
        }

        // Help overlay is modal: while open, only `?`/`Esc` close it and
        // `q` still quits. Everything else is swallowed so accidental page
        // navigation doesn't happen behind the overlay.
        if self.show_help {
            match code {
                KeyCode::Char('?') | KeyCode::Esc => {
                    self.show_help = false;
                }
                KeyCode::Char('q') => return Ok(true),
                _ => {}
            }
            return Ok(false);
        }

        // `?` opens the overlay from any page. Cheap to intercept here so
        // the per-page handlers don't each need to know about it.
        if code == KeyCode::Char('?') {
            self.show_help = true;
            return Ok(false);
        }

        match self.page {
            UiPage::Tree => Ok(self.handle_event_tree(code)),
            UiPage::Epochs => Ok(self.handle_event_epochs(code)),
            UiPage::Stderr => Ok(self.handle_event_stderr(code)),
        }
    }

    fn handle_event_tree(&mut self, code: KeyCode) -> bool {
        // Any keypress clears the prior `n`/`N` toast. Set again below if
        // the next jump is still a no-op, so repeated `n` presses keep
        // the message visible without sticking around after the operator
        // moves on.
        self.tree.toast = None;

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

            KeyCode::Char('s') => self.side_by_side = !self.side_by_side,

            KeyCode::Char('g') => {
                if was_awaiting_g {
                    self.tree_jump_first();
                } else {
                    self.tree.awaiting_g = true;
                }
            }
            KeyCode::Char('G') => self.tree_jump_last(),

            KeyCode::Char('n') => self.tree_jump_to_change(true),
            KeyCode::Char('N') => self.tree_jump_to_change(false),

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
        let was_awaiting_g = self.epochs.awaiting_g;
        self.epochs.awaiting_g = false;

        match code {
            KeyCode::Char('q') => return true,
            // Esc closes the page (returns to Tree) to match standard TUI
            // conventions; `q` is the only way to quit, so the operator can't
            // accidentally exit by tapping Esc.
            KeyCode::Esc => self.page = UiPage::Tree,

            KeyCode::Char('1') => self.page = UiPage::Tree,
            KeyCode::Char('2') => self.page = UiPage::Epochs,
            KeyCode::Char('e') => {
                self.page = UiPage::Stderr;
                self.stderr_follow = true;
                self.stderr_scroll = u16::MAX;
            }

            KeyCode::Tab => {
                self.epochs.detail_focus = match self.epochs.detail_focus {
                    DetailFocus::Tree => DetailFocus::Detail,
                    DetailFocus::Detail => DetailFocus::Tree,
                };
            }

            KeyCode::Char('s') => self.side_by_side = !self.side_by_side,

            KeyCode::Char('g') => {
                if was_awaiting_g {
                    self.epochs_jump_first();
                } else {
                    self.epochs.awaiting_g = true;
                }
            }
            KeyCode::Char('G') => self.epochs_jump_last(),

            KeyCode::Down | KeyCode::Char('j') => self.epochs_move_or_scroll(1),
            KeyCode::Up | KeyCode::Char('k') => self.epochs_move_or_scroll(-1),

            KeyCode::Right | KeyCode::Char('l') => {
                if let Some(EpochsCursor::EpochHeader { epoch }) = self.epochs.selected {
                    self.epochs.collapsed.remove(&epoch);
                }
            }

            KeyCode::Left | KeyCode::Char('h') => {
                if let Some(EpochsCursor::EpochHeader { epoch }) = self.epochs.selected {
                    self.epochs.collapsed.insert(epoch);
                    self.clamp_epochs_selection_to_visible();
                }
            }

            KeyCode::Char(' ') => {
                if let Some(EpochsCursor::EpochHeader { epoch }) = self.epochs.selected {
                    self.epochs.toggle_collapse(epoch);
                    self.clamp_epochs_selection_to_visible();
                }
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

    /// Move the selection to the next (or, for `forward = false`, previous)
    /// row whose badge represents a change worth investigating - `Changed`
    /// or `Failed`. Branch rollups are included so the operator can land on
    /// a collapsed plan-item branch and drill in. `Ok`, `Planned`, and
    /// `Running` are skipped: the operator is hunting for resolved-with-
    /// difference rows. No wrap-around; if no target exists in the chosen
    /// direction, the selection stays put and a one-shot footer toast
    /// surfaces "no more changes".
    fn tree_jump_to_change(&mut self, forward: bool) {
        let rows = build_visible_rows(&self.app_view, &self.tree);
        if rows.is_empty() {
            self.tree.toast = Some("no more changes".into());
            return;
        }
        let current = rows
            .iter()
            .position(|r| Some(r.arena_index) == self.tree.selected);
        let target = if forward {
            let start = current.map(|i| i + 1).unwrap_or(0);
            rows[start..]
                .iter()
                .position(is_change_target)
                .map(|i| start + i)
        } else {
            let end = current.unwrap_or(rows.len());
            rows[..end].iter().rposition(is_change_target)
        };
        match target {
            Some(idx) => {
                self.tree.selected = Some(rows[idx].arena_index);
                self.tree.detail_scroll = 0;
            }
            None => {
                self.tree.toast = Some("no more changes".into());
            }
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

    fn epochs_move_or_scroll(&mut self, delta: i32) {
        if self.epochs.detail_focus == DetailFocus::Detail {
            if delta > 0 {
                self.epochs.detail_scroll = self.epochs.detail_scroll.saturating_add(delta as u16);
            } else {
                self.epochs.detail_scroll =
                    self.epochs.detail_scroll.saturating_sub((-delta) as u16);
            }
        } else {
            self.epochs_move(delta);
        }
    }

    fn epochs_move(&mut self, delta: i32) {
        let rows = build_epochs_rows(&self.app_view, &self.epochs);
        if rows.is_empty() {
            self.epochs.selected = None;
            self.epochs.list_offset = 0;
            return;
        }
        let current = rows
            .iter()
            .position(|r| Some(r.cursor) == self.epochs.selected)
            .unwrap_or(0);
        let next = if delta >= 0 {
            (current + delta as usize).min(rows.len() - 1)
        } else {
            current.saturating_sub((-delta) as usize)
        };
        self.epochs.selected = Some(rows[next].cursor);
        self.epochs.detail_scroll = 0;
    }

    fn epochs_jump_first(&mut self) {
        let rows = build_epochs_rows(&self.app_view, &self.epochs);
        if let Some(first) = rows.first() {
            self.epochs.selected = Some(first.cursor);
        }
    }

    fn epochs_jump_last(&mut self) {
        let rows = build_epochs_rows(&self.app_view, &self.epochs);
        if let Some(last) = rows.last() {
            self.epochs.selected = Some(last.cursor);
        }
    }

    /// After collapsing, the previously-selected cursor may no longer appear
    /// in the visible rows. Move the selection to the still-visible header of
    /// the same epoch (every row carries one), falling back to the first row.
    /// Mirrors [`TuiApp::clamp_tree_selection_to_visible`].
    fn clamp_epochs_selection_to_visible(&mut self) {
        let rows = build_epochs_rows(&self.app_view, &self.epochs);
        if rows.is_empty() {
            self.epochs.selected = None;
            return;
        }
        let Some(prev) = self.epochs.selected else {
            self.epochs.selected = rows.first().map(|r| r.cursor);
            return;
        };
        if rows.iter().any(|r| r.cursor == prev) {
            return;
        }
        let prev_epoch = epochs_cursor_epoch(&self.app_view, prev);
        let fallback = rows
            .iter()
            .rfind(|r| matches!(r.cursor, EpochsCursor::EpochHeader { epoch } if Some(epoch) == prev_epoch))
            .or_else(|| rows.first());
        self.epochs.selected = fallback.map(|r| r.cursor);
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
                Constraint::Length(1), // tab strip
                Constraint::Min(3),    // body
                Constraint::Length(1), // footer hints / filter prompt
            ]
            .as_ref(),
        )
        .split(frame.area());

    draw_header(frame, layout[0], app, outcome);
    draw_tab_strip(frame, layout[1], app);
    let body = layout[2];
    match app.page {
        UiPage::Tree => draw_tree_page(frame, body, app),
        UiPage::Epochs => draw_epochs_page(frame, body, app),
        UiPage::Stderr => draw_stderr_page(frame, body, app),
    }
    draw_footer(frame, layout[3], app);
    if app.show_help {
        draw_help_overlay(frame, body, app);
    }
}

/// One-line tab strip showing the three pages with their selector key. The
/// active tab is bold + cyan + underlined; inactives are dimmed. Compact form
/// (drops the labels) kicks in below 60 cols so the strip stays on one row on
/// narrow terminals.
fn draw_tab_strip(frame: &mut Frame, area: Rect, app: &TuiApp) {
    let compact = area.width < 60;
    let tabs: [(UiPage, &str, &str); 3] = [
        (UiPage::Tree, "[1 Tree]", "[1]"),
        (UiPage::Epochs, "[2 Epochs]", "[2]"),
        (UiPage::Stderr, "[e Stderr]", "[e]"),
    ];
    let mut spans: Vec<Span> = Vec::new();
    for (i, (page, full, short)) in tabs.iter().enumerate() {
        if i > 0 {
            spans.push(Span::raw(" "));
        }
        let label = if compact { *short } else { *full };
        let style = if *page == app.page {
            Style::default()
                .fg(Color::Cyan)
                .add_modifier(Modifier::BOLD)
                .add_modifier(Modifier::UNDERLINED)
        } else {
            Style::default().fg(Color::DarkGray)
        };
        spans.push(Span::styled(label.to_string(), style));
    }
    let widget = Paragraph::new(Line::from(spans)).alignment(Alignment::Left);
    frame.render_widget(widget, area);
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
    } else if let Some((epoch, summary)) = app.app_view.pending_epoch.as_ref() {
        let total = app
            .app_view
            .resource_epochs_total()
            .map(|n| n.to_string())
            .unwrap_or_else(|| "?".into());
        let head = format!(
            "Epoch {}/{total} · {} atoms, {} handlers · ",
            epoch + 1,
            summary.atoms_changed,
            summary.handlers_pending,
        );
        Line::from(vec![
            Span::styled(head, Style::default().fg(Color::Yellow)),
            Span::styled(
                "↵/y apply",
                Style::default()
                    .fg(Color::Green)
                    .add_modifier(Modifier::BOLD),
            ),
            Span::styled("  ", Style::default()),
            Span::styled(
                "n/Esc abort",
                Style::default().fg(Color::Red).add_modifier(Modifier::BOLD),
            ),
            Span::styled("  ? help", Style::default().fg(Color::DarkGray)),
        ])
    } else if app.page == UiPage::Tree
        && let Some(toast) = app.tree.toast.as_ref()
    {
        Line::from(Span::styled(
            toast.clone(),
            Style::default().fg(Color::Yellow),
        ))
    } else {
        let hint = footer_hint(app);
        Line::from(Span::styled(hint, Style::default().fg(Color::DarkGray)))
    };
    let widget = Paragraph::new(line).alignment(Alignment::Left);
    frame.render_widget(widget, area);
}

fn footer_hint(_app: &TuiApp) -> String {
    "1/2/e pages · Tab focus · f follow · ? help · q quit".to_string()
}

/// Render the modal help overlay centred inside the body rect. Anchored to
/// `body` (not the frame) so other top-level layout changes don't move the
/// overlay around the operator's view.
fn draw_help_overlay(frame: &mut Frame, body: Rect, _app: &TuiApp) {
    let groups: &[(&str, &[&str])] = &[
        (
            "Navigation",
            &[
                "j/k or ↓/↑   move",
                "h/l or ←/→   collapse/expand",
                "Space        toggle collapse",
                "gg / G       first / last",
                "n / N        next / prev change",
                "PgUp/PgDn    page up/down",
            ],
        ),
        (
            "View",
            &[
                "Tab          focus tree / detail",
                "/ filter     (Enter apply, Esc clear)",
                "u            show/hide unchanged",
                "s            side-by-side diff (≥140 cols)",
                "f            follow latest activity",
            ],
        ),
        (
            "Pages",
            &[
                "1            Tree",
                "2            Epochs",
                "e            Stderr",
            ],
        ),
        (
            "Confirm",
            &["Enter / y    apply this epoch", "n / Esc      abort"],
        ),
        ("Quit", &["q            quit lusid"]),
    ];

    let mut lines: Vec<Line<'static>> = Vec::new();
    for (i, (title, entries)) in groups.iter().enumerate() {
        if i > 0 {
            lines.push(blank_line());
        }
        lines.push(Line::from(Span::styled(
            title.to_string(),
            Style::default()
                .fg(Color::Cyan)
                .add_modifier(Modifier::BOLD),
        )));
        for entry in *entries {
            lines.push(Line::from(Span::raw(format!("  {entry}"))));
        }
    }
    lines.push(blank_line());
    lines.push(Line::from(Span::styled(
        "? or Esc to close",
        Style::default().fg(Color::DarkGray),
    )));

    let content_height = lines.len() as u16;
    let width = 60u16.min(body.width.saturating_sub(4));
    let height = (content_height + 2).min(body.height.saturating_sub(2));
    let x = body.x + body.width.saturating_sub(width) / 2;
    let y = body.y + body.height.saturating_sub(height) / 2;
    let area = Rect {
        x,
        y,
        width,
        height,
    };

    let widget = Paragraph::new(Text::from(lines))
        .block(Block::default().borders(Borders::ALL).title("Help"))
        .wrap(Wrap { trim: false });
    frame.render_widget(ratatui::widgets::Clear, area);
    frame.render_widget(widget, area);
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

    // Body area spans the full terminal width, so this is the threshold the
    // spec calls out for `(epoch K)` tags. Hidden below 80 cols so labels
    // keep their column budget on narrow terminals.
    let show_epoch_tag = area.width >= 80;

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

    draw_tree_list(frame, layout[0], app, show_epoch_tag);
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
    /// Latest 0-based resource epoch any descendant atom lands in, when the
    /// wire's epoch mapping is known. Branches only - leaves leave it `None`
    /// because the spec reserves the tag for plan-item rows. Rendered as
    /// `(epoch K)` (1-based) at terminal widths >= 80 cols.
    epoch: Option<usize>,
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
    // Branch -> latest descendant atom epoch. Empty when `PipelineInfo`
    // hasn't arrived yet (no atom_epoch entries); branches then render
    // without an epoch tag.
    let parent_of = build_parent_of_resources(resources);
    let latest_epoch_by_branch = build_latest_epoch_by_branch(resources, view, &parent_of);
    let ctx = RowWalkCtx {
        resources,
        state,
        filter,
        latest_epoch_by_branch: &latest_epoch_by_branch,
    };
    walk_for_rows(&ctx, ROOT_ARENA_INDEX, 0, &mut out, &mut HashSet::new());
    out
}

/// Shared inputs threaded through the recursive [`walk_for_rows`]. Kept as
/// a borrowed bundle so each recursive call only mutates `out` / `visited`.
struct RowWalkCtx<'a> {
    resources: &'a lusid_apply_stdio::ResourcesTree,
    state: &'a TreePageState,
    filter: Option<&'a str>,
    latest_epoch_by_branch: &'a HashMap<usize, usize>,
}

fn walk_for_rows(
    ctx: &RowWalkCtx<'_>,
    arena_index: usize,
    depth: usize,
    out: &mut Vec<TreeRow>,
    visited: &mut HashSet<usize>,
) {
    if !visited.insert(arena_index) {
        return;
    }
    let Some(slot) = ctx
        .resources
        .nodes
        .get(arena_index)
        .and_then(Option::as_ref)
    else {
        return;
    };
    match slot {
        ResourcesNode::Branch { meta, children } => {
            let label = plan_meta_short_label(meta);
            let badge = rollup_for_branch(ctx.resources, arena_index);
            let dim = match ctx.filter {
                Some(f) => !label.to_lowercase().contains(&f.to_lowercase()),
                None => false,
            };
            // Reserve the epoch tag for named plan items so the anonymous
            // root - whose latest epoch always equals the global total -
            // doesn't duplicate the header strip's `epoch K/N`.
            let epoch = meta
                .id
                .as_ref()
                .and_then(|_| ctx.latest_epoch_by_branch.get(&arena_index).copied());
            out.push(TreeRow {
                arena_index,
                depth,
                is_branch: true,
                badge,
                label,
                dim,
                epoch,
            });
            if ctx.state.is_expanded(arena_index) {
                for &child in children {
                    walk_for_rows(ctx, child, depth + 1, out, visited);
                }
            }
        }
        ResourcesNode::Leaf { state: leaf_state } => {
            let badge = badge_for_leaf(leaf_state);
            if !ctx.state.show_unchanged && badge == Badge::Ok {
                return;
            }
            let label = leaf_state.resource().render().to_plain_string();
            let dim = match ctx.filter {
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
                epoch: None,
            });
        }
    }
}

fn draw_tree_list(frame: &mut ratatui::Frame, area: Rect, app: &mut TuiApp, show_epoch_tag: bool) {
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
            if show_epoch_tag && let Some(epoch) = row.epoch {
                spans.push(Span::styled(
                    format!("  (epoch {})", epoch + 1),
                    Style::default().fg(Color::DarkGray),
                ));
            }
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

fn draw_detail_pane(frame: &mut ratatui::Frame, area: Rect, app: &mut TuiApp) {
    let render_palette = RenderPalette::default();
    app.last_detail_width = area.width;
    let diff_opts = diff_opts_for_pane(area.width, app.side_by_side);
    let text = match app.tree.selected {
        Some(arena_index) => {
            detail_for_node(&app.app_view, arena_index, &render_palette, diff_opts)
        }
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

/// Translate detail-pane width + the operator's `s` toggle into renderer
/// options. Side-by-side is gated at 140 cols: above the threshold the
/// toggle takes effect; below, the renderer silently falls back to
/// unified. Width fed to the renderer is the pane width minus 2 for the
/// surrounding border so columns don't run past the frame.
fn diff_opts_for_pane(pane_width: u16, side_by_side: bool) -> DiffOptions {
    let inner_width = pane_width.saturating_sub(2) as usize;
    DiffOptions {
        side_by_side: side_by_side && pane_width >= 140,
        width: inner_width.max(40),
        context_lines: 3,
    }
}

/// Build the detail content for a given arena index. Branches get
/// plan-item metadata; leaves get the per-lifecycle content table.
fn detail_for_node(
    view: &AppView,
    arena_index: usize,
    palette: &RenderPalette,
    diff_opts: DiffOptions,
) -> Text<'static> {
    let Some(resources) = view.resources.as_ref() else {
        return Text::from("(no resources)");
    };
    let Some(slot) = resources.nodes.get(arena_index).and_then(Option::as_ref) else {
        return Text::from("(missing slot)");
    };
    match slot {
        ResourcesNode::Branch { meta, children } => detail_for_branch(meta, children, palette),
        ResourcesNode::Leaf { state } => detail_for_leaf(state, palette, diff_opts),
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

fn detail_for_leaf(
    state: &LeafState,
    palette: &RenderPalette,
    diff_opts: DiffOptions,
) -> Text<'static> {
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
            extend_lines(&mut lines, &render_change(change, diff_opts), palette);
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
// Epochs page
// --------------------------------------------------------------------------

fn draw_epochs_page(frame: &mut ratatui::Frame, area: Rect, app: &mut TuiApp) {
    if app.app_view.resource_epochs_total().is_none() {
        draw_placeholder(frame, area, "Waiting for pipeline info...");
        return;
    }

    let rows = build_epochs_rows(&app.app_view, &app.epochs);

    // Lazy-default the selection to the first visible row (an epoch header).
    if app.epochs.selected.is_none() {
        app.epochs.selected = rows.first().map(|r| r.cursor);
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

    draw_epochs_list(frame, layout[0], app, &rows);
    draw_epochs_detail(frame, layout[1], app);
}

/// One renderable row on the Epochs page. The flat list mirrors `EpochsCursor`
/// (the stable selection key) plus the small set of derived facts the row
/// needs at draw time so the renderer doesn't re-walk the tree for each row.
#[derive(Debug, Clone)]
struct EpochsRow {
    cursor: EpochsCursor,
    depth: usize,
    badge: Option<Badge>,
    label: String,
    /// `~ requires: <id>` / `(epoch K)` annotation. Per-atom only; the
    /// detail pane shows full per-branch dependency info.
    annotation: Option<String>,
}

/// Visible-row list for the Epochs page. Sections appear in resource-epoch
/// order; within each section: epoch header → atoms → Phase A header → Phase A
/// ops → Phase B header → Phase B ops. Collapsed sections render only their
/// header. Atoms are sorted by arena index so navigation is deterministic.
fn build_epochs_rows(view: &AppView, state: &EpochsPageState) -> Vec<EpochsRow> {
    let mut rows = Vec::new();
    let Some(total) = view.resource_epochs_total() else {
        return rows;
    };

    // atoms grouped by resource epoch.
    let mut atoms_by_epoch: BTreeMap<usize, Vec<usize>> = BTreeMap::new();
    for (atom_idx, epoch) in &view.atom_epoch {
        atoms_by_epoch.entry(*epoch).or_default().push(*atom_idx);
    }
    for v in atoms_by_epoch.values_mut() {
        v.sort_unstable();
    }

    // op-epochs grouped by (resource_epoch, phase). The inner Vec is in
    // arrival order, which mirrors apply order, which is what operators
    // expect to read top-down.
    let mut ops_by_epoch_phase: HashMap<(usize, Phase), Vec<usize>> = HashMap::new();
    for (op_epoch_index, meta) in view.operation_epoch_meta.iter().enumerate() {
        debug_assert!(
            meta.resource_epoch < total,
            "op epoch {op_epoch_index} carries resource_epoch={} \
             but PipelineInfo says total={total}; ops would be invisible",
            meta.resource_epoch,
        );
        ops_by_epoch_phase
            .entry((meta.resource_epoch, meta.phase))
            .or_default()
            .push(op_epoch_index);
    }

    let resources = view.resources.as_ref();
    let parent_of = resources.map(build_parent_of_resources).unwrap_or_default();
    let plan_item_index = resources.map(build_plan_item_index).unwrap_or_default();
    let latest_epoch_by_branch = resources
        .map(|r| build_latest_epoch_by_branch(r, view, &parent_of))
        .unwrap_or_default();

    for epoch in 0..total {
        let atoms = atoms_by_epoch.get(&epoch).cloned().unwrap_or_default();
        let phase_a = ops_by_epoch_phase
            .get(&(epoch, Phase::A))
            .cloned()
            .unwrap_or_default();
        let phase_b = ops_by_epoch_phase
            .get(&(epoch, Phase::B))
            .cloned()
            .unwrap_or_default();

        // Header.
        rows.push(EpochsRow {
            cursor: EpochsCursor::EpochHeader { epoch },
            depth: 0,
            badge: Some(rollup_for_atoms(resources, &atoms)),
            label: format_epoch_header(view, epoch, &atoms, &phase_b),
            annotation: None,
        });

        if !state.is_expanded(epoch) {
            continue;
        }

        // Atoms.
        for arena_index in &atoms {
            let (badge, label) = atom_badge_and_label(resources, *arena_index);
            let annotation = requires_annotation_for_atom(
                view,
                *arena_index,
                &parent_of,
                &plan_item_index,
                &latest_epoch_by_branch,
            );
            rows.push(EpochsRow {
                cursor: EpochsCursor::Atom {
                    arena_index: *arena_index,
                },
                depth: 1,
                badge: Some(badge),
                label,
                annotation,
            });
        }

        // Phase A.
        rows.push(EpochsRow {
            cursor: EpochsCursor::PhaseHeader {
                epoch,
                phase: Phase::A,
            },
            depth: 1,
            badge: None,
            label: format!("Phase A · {} op event(s)", phase_a.len()),
            annotation: None,
        });
        for &epoch_index in &phase_a {
            push_op_rows(&mut rows, view, epoch_index);
        }

        // Phase B.
        rows.push(EpochsRow {
            cursor: EpochsCursor::PhaseHeader {
                epoch,
                phase: Phase::B,
            },
            depth: 1,
            badge: None,
            label: if phase_b.is_empty() {
                "Phase B · (no on_change handlers fired)".to_string()
            } else {
                format!("Phase B · {} op event(s)", phase_b.len())
            },
            annotation: None,
        });
        for &epoch_index in &phase_b {
            push_op_rows(&mut rows, view, epoch_index);
        }
    }

    rows
}

/// Append one OpRow per operation in the given op-epoch's apply pane.
/// Lifts the badge from `OperationView` so callers don't need to.
fn push_op_rows(rows: &mut Vec<EpochsRow>, view: &AppView, epoch_index: usize) {
    let Some(ops) = view.operations_epochs.get(epoch_index) else {
        return;
    };
    for (op_index, op) in ops.iter().enumerate() {
        rows.push(EpochsRow {
            cursor: EpochsCursor::Op {
                epoch_index,
                op_index,
            },
            depth: 2,
            badge: Some(badge_for_op(op)),
            label: op.label.render().to_plain_string(),
            annotation: None,
        });
    }
}

fn draw_epochs_list(frame: &mut ratatui::Frame, area: Rect, app: &mut TuiApp, rows: &[EpochsRow]) {
    let selected_row = rows
        .iter()
        .position(|r| Some(r.cursor) == app.epochs.selected);

    let items: Vec<ListItem> = rows
        .iter()
        .map(|row| {
            let mut spans: Vec<Span> = Vec::new();
            spans.push(Span::raw("  ".repeat(row.depth)));
            if let Some(badge) = row.badge {
                spans.push(Span::styled(format!("{} ", badge.glyph()), badge.style()));
            }
            // Header rows render the collapse glyph so operators see the toggle
            // affordance; ops/atoms don't have descendants of their own here.
            if let EpochsCursor::EpochHeader { epoch } = row.cursor {
                spans.push(Span::styled(
                    if app.epochs.is_expanded(epoch) {
                        "▼ "
                    } else {
                        "▶ "
                    },
                    Style::default().fg(Color::DarkGray),
                ));
            }
            let label_style = match row.cursor {
                EpochsCursor::EpochHeader { .. } => Style::default().add_modifier(Modifier::BOLD),
                EpochsCursor::PhaseHeader { .. } => Style::default().fg(Color::DarkGray),
                _ => Style::default(),
            };
            spans.push(Span::styled(row.label.clone(), label_style));
            if let Some(ann) = &row.annotation {
                spans.push(Span::styled(
                    format!("  {ann}"),
                    Style::default().fg(Color::DarkGray),
                ));
            }
            ListItem::new(Line::from(spans))
        })
        .collect();

    let mut list_state = ListState::default();
    list_state.select(selected_row);

    let inner_height = area.height.saturating_sub(2) as usize;
    if let Some(row) = selected_row {
        app.epochs.ensure_visible_row(row, inner_height);
    }
    *list_state.offset_mut() = app.epochs.list_offset;

    let title = if app.epochs.detail_focus == DetailFocus::Tree {
        "epochs (focused)"
    } else {
        "epochs"
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

fn draw_epochs_detail(frame: &mut ratatui::Frame, area: Rect, app: &mut TuiApp) {
    let render_palette = RenderPalette::default();
    app.last_detail_width = area.width;
    let diff_opts = diff_opts_for_pane(area.width, app.side_by_side);
    let text = match app.epochs.selected {
        Some(cursor) => detail_for_epochs_cursor(&app.app_view, cursor, &render_palette, diff_opts),
        None => Text::from("(no selection)"),
    };

    let title = if app.epochs.detail_focus == DetailFocus::Detail {
        "detail (focused)"
    } else {
        "detail"
    };
    let widget = Paragraph::new(text)
        .block(Block::default().borders(Borders::ALL).title(title))
        .wrap(Wrap { trim: false })
        .scroll((app.epochs.detail_scroll, 0));
    frame.render_widget(widget, area);
}

fn detail_for_epochs_cursor(
    view: &AppView,
    cursor: EpochsCursor,
    palette: &RenderPalette,
    diff_opts: DiffOptions,
) -> Text<'static> {
    match cursor {
        EpochsCursor::EpochHeader { epoch } => detail_for_epoch_header(view, epoch),
        EpochsCursor::Atom { arena_index } => {
            let Some(resources) = view.resources.as_ref() else {
                return Text::from("(no resources)");
            };
            match resources.nodes.get(arena_index).and_then(Option::as_ref) {
                Some(ResourcesNode::Leaf { state }) => detail_for_leaf(state, palette, diff_opts),
                _ => Text::from(format!("(no atom at index {arena_index})")),
            }
        }
        EpochsCursor::PhaseHeader { epoch, phase } => detail_for_phase_header(view, epoch, phase),
        EpochsCursor::Op {
            epoch_index,
            op_index,
        } => detail_for_op(view, epoch_index, op_index, palette),
    }
}

fn detail_for_epoch_header(view: &AppView, epoch: usize) -> Text<'static> {
    let mut lines: Vec<Line<'static>> = Vec::new();
    let total = view
        .resource_epochs_total()
        .map(|n| n.to_string())
        .unwrap_or_else(|| "?".into());
    lines.push(section_header(&format!(
        "Resource epoch {}/{total}",
        epoch + 1
    )));

    let atoms: Vec<usize> = view
        .atom_epoch
        .iter()
        .filter_map(|(idx, e)| if *e == epoch { Some(*idx) } else { None })
        .collect();
    let mut atoms = atoms;
    atoms.sort_unstable();

    lines.push(field_line("atoms", &atoms.len().to_string()));
    let changed = atoms
        .iter()
        .filter(|&&idx| matches!(leaf_at(view, idx), Some(LeafState::Changed { .. })))
        .count();
    lines.push(field_line("changed", &changed.to_string()));
    let phase_a = view
        .operation_epoch_meta
        .iter()
        .filter(|m| m.resource_epoch == epoch && m.phase == Phase::A)
        .count();
    let phase_b = view
        .operation_epoch_meta
        .iter()
        .filter(|m| m.resource_epoch == epoch && m.phase == Phase::B)
        .count();
    lines.push(field_line("Phase A op events", &phase_a.to_string()));
    lines.push(field_line("Phase B op events", &phase_b.to_string()));

    Text::from(lines)
}

fn detail_for_phase_header(view: &AppView, epoch: usize, phase: Phase) -> Text<'static> {
    let mut lines: Vec<Line<'static>> = Vec::new();
    let label = match phase {
        Phase::A => "Phase A: change ops produced by this epoch's atoms",
        Phase::B => "Phase B: on_change handlers fired by this epoch's branches",
    };
    lines.push(section_header(label));

    let matching: Vec<(usize, usize)> = view
        .operation_epoch_meta
        .iter()
        .enumerate()
        .filter_map(|(i, m)| {
            if m.resource_epoch == epoch && m.phase == phase {
                Some((i, view.operations_epochs.get(i).map(Vec::len).unwrap_or(0)))
            } else {
                None
            }
        })
        .collect();

    if matching.is_empty() {
        lines.push(blank_line());
        let note = match phase {
            Phase::A => "no op events yet (epoch may not have run, or had no changes)",
            Phase::B => "no on_change handlers fired",
        };
        lines.push(status_line(note));
    } else {
        let total_ops: usize = matching.iter().map(|(_, n)| *n).sum();
        lines.push(field_line("op epochs", &matching.len().to_string()));
        lines.push(field_line("total ops", &total_ops.to_string()));
    }

    Text::from(lines)
}

fn detail_for_op(
    view: &AppView,
    epoch_index: usize,
    op_index: usize,
    palette: &RenderPalette,
) -> Text<'static> {
    let mut lines: Vec<Line<'static>> = Vec::new();
    let Some(op) = view
        .operations_epochs
        .get(epoch_index)
        .and_then(|ops| ops.get(op_index))
    else {
        return Text::from(format!("(no op at {epoch_index}.{op_index})"));
    };

    lines.push(section_header("Operation"));
    extend_lines(&mut lines, &op.label.render(), palette);
    lines.push(blank_line());

    let status = if !op.is_complete {
        "running"
    } else if op.error.is_some() {
        "failed"
    } else {
        "complete"
    };
    lines.push(field_line("status", status));
    if let Some(meta) = view.operation_epoch_meta(epoch_index) {
        let total = view
            .resource_epochs_total()
            .map(|n| n.to_string())
            .unwrap_or_else(|| "?".into());
        lines.push(field_line(
            "resource epoch",
            &format!("{}/{total}", meta.resource_epoch + 1),
        ));
        lines.push(field_line(
            "phase",
            match meta.phase {
                Phase::A => "A",
                Phase::B => "B",
            },
        ));
    }
    if let Some(err) = &op.error {
        lines.push(blank_line());
        lines.push(section_header("Error"));
        lines.push(Line::from(Span::styled(
            err.clone(),
            Style::default().fg(Color::Red),
        )));
    }
    if !op.stdout.is_empty() {
        lines.push(blank_line());
        lines.push(section_header("stdout"));
        for line in op.stdout.lines() {
            lines.push(Line::from(Span::raw(line.to_string())));
        }
    }
    if !op.stderr.is_empty() {
        lines.push(blank_line());
        lines.push(section_header("stderr"));
        for line in op.stderr.lines() {
            lines.push(Line::from(Span::styled(
                line.to_string(),
                Style::default().fg(Color::Red),
            )));
        }
    }

    Text::from(lines)
}

/// Header row text. `handlers` is the count of Phase B *operations* (sum over
/// the relevant op-epochs), matching project terminology where handlers are
/// the `on_change` ops fired in Phase B. Epoch number is 1-based to match
/// the header strip (`epoch K/N`).
fn format_epoch_header(view: &AppView, epoch: usize, atoms: &[usize], phase_b: &[usize]) -> String {
    let total = view
        .resource_epochs_total()
        .map(|n| n.to_string())
        .unwrap_or_else(|| "?".into());
    let changed = atoms
        .iter()
        .filter(|&&idx| matches!(leaf_at(view, idx), Some(LeafState::Changed { .. })))
        .count();
    let handlers: usize = phase_b
        .iter()
        .filter_map(|&i| view.operations_epochs.get(i).map(Vec::len))
        .sum();
    format!(
        "Epoch {one_based}/{total} · {} atoms · {changed} changed · {handlers} handlers",
        atoms.len(),
        one_based = epoch + 1,
    )
}

fn leaf_at(view: &AppView, arena_index: usize) -> Option<&LeafState> {
    match view.resources.as_ref()?.nodes.get(arena_index)?.as_ref()? {
        ResourcesNode::Leaf { state } => Some(state),
        _ => None,
    }
}

fn atom_badge_and_label(
    resources: Option<&lusid_apply_stdio::ResourcesTree>,
    arena_index: usize,
) -> (Badge, String) {
    let Some(resources) = resources else {
        return (Badge::Planned, format!("#{arena_index}"));
    };
    match resources.nodes.get(arena_index).and_then(Option::as_ref) {
        Some(ResourcesNode::Leaf { state }) => (
            badge_for_leaf(state),
            state.resource().render().to_plain_string(),
        ),
        _ => (Badge::Planned, format!("#{arena_index}")),
    }
}

fn rollup_for_atoms(
    resources: Option<&lusid_apply_stdio::ResourcesTree>,
    atoms: &[usize],
) -> Badge {
    let Some(resources) = resources else {
        return Badge::Planned;
    };
    let mut acc = Badge::Ok;
    let mut saw_any = false;
    for &idx in atoms {
        if let Some(ResourcesNode::Leaf { state }) =
            resources.nodes.get(idx).and_then(Option::as_ref)
        {
            let b = badge_for_leaf(state);
            acc = if saw_any { rollup(acc, b) } else { b };
            saw_any = true;
        }
    }
    if saw_any { acc } else { Badge::Planned }
}

fn badge_for_op(op: &OperationView) -> Badge {
    if !op.is_complete {
        Badge::Running
    } else if op.error.is_some() {
        Badge::Failed
    } else {
        Badge::Ok
    }
}

/// `arena_index -> parent_arena_index` for the consumer's view of the atoms
/// tree. Produced once per render frame.
fn build_parent_of_resources(
    resources: &lusid_apply_stdio::ResourcesTree,
) -> HashMap<usize, usize> {
    let mut parent_of: HashMap<usize, usize> = HashMap::new();
    for (idx, slot) in resources.nodes.iter().enumerate() {
        if let Some(ResourcesNode::Branch { children, .. }) = slot {
            for &child in children {
                parent_of.insert(child, idx);
            }
        }
    }
    parent_of
}

/// `PlanNodeId -> branch_arena_index` for every branch whose `meta.id` is set.
/// Lets `requires` ids be resolved back to the branch (and its descendant
/// atoms) without re-walking the tree.
fn build_plan_item_index(
    resources: &lusid_apply_stdio::ResourcesTree,
) -> HashMap<PlanNodeId, usize> {
    let mut out: HashMap<PlanNodeId, usize> = HashMap::new();
    for (idx, slot) in resources.nodes.iter().enumerate() {
        if let Some(ResourcesNode::Branch {
            meta: PlanMeta { id: Some(id), .. },
            ..
        }) = slot
        {
            out.insert(id.clone(), idx);
        }
    }
    out
}

/// `← requires: <id> (epoch K)` annotation for an atom row. Walks up the
/// ancestor chain until it finds a branch with at least one cross-epoch
/// requires (resolves to a known branch whose latest atom epoch is strictly
/// earlier than this atom's epoch). Same-epoch and later edges add noise
/// without explaining where the row landed, so the walk continues past
/// ancestors whose entire requires set is non-crossing.
///
/// `None` when no ancestor declares any cross-epoch requires.
fn requires_annotation_for_atom(
    view: &AppView,
    arena_index: usize,
    parent_of: &HashMap<usize, usize>,
    plan_item_index: &HashMap<PlanNodeId, usize>,
    latest_epoch_by_branch: &HashMap<usize, usize>,
) -> Option<String> {
    let resources = view.resources.as_ref()?;
    let atom_epoch = view.epoch_of_atom(arena_index)?;
    let mut cur = parent_of.get(&arena_index).copied();
    while let Some(idx) = cur {
        if let Some(ResourcesNode::Branch {
            meta: PlanMeta { requires, .. },
            ..
        }) = resources.nodes.get(idx).and_then(Option::as_ref)
            && !requires.is_empty()
        {
            let parts: Vec<String> = requires
                .iter()
                .filter_map(|r| {
                    let branch_idx = plan_item_index.get(r)?;
                    let dep_epoch = latest_epoch_by_branch.get(branch_idx).copied()?;
                    if dep_epoch >= atom_epoch {
                        return None;
                    }
                    Some(format!(
                        "{} (epoch {})",
                        plan_node_short_label(r),
                        dep_epoch + 1,
                    ))
                })
                .collect();
            if !parts.is_empty() {
                return Some(format!("← requires: {}", parts.join(", ")));
            }
        }
        cur = parent_of.get(&idx).copied();
    }
    None
}

/// `branch_arena_index -> max epoch of any descendant atom`. Bottom-up:
/// every leaf contributes its own epoch to its chain of ancestors. One pass
/// over the arena instead of per-row walks; consumed by the requires
/// annotation to resolve `requires: [<id>]` to the epoch the depended-on
/// plan item's atoms ran in.
fn build_latest_epoch_by_branch(
    resources: &lusid_apply_stdio::ResourcesTree,
    view: &AppView,
    parent_of: &HashMap<usize, usize>,
) -> HashMap<usize, usize> {
    let mut out: HashMap<usize, usize> = HashMap::new();
    for (idx, slot) in resources.nodes.iter().enumerate() {
        if !matches!(slot, Some(ResourcesNode::Leaf { .. })) {
            continue;
        }
        let Some(epoch) = view.epoch_of_atom(idx) else {
            continue;
        };
        let mut cur = parent_of.get(&idx).copied();
        while let Some(branch_idx) = cur {
            out.entry(branch_idx)
                .and_modify(|e| *e = (*e).max(epoch))
                .or_insert(epoch);
            cur = parent_of.get(&branch_idx).copied();
        }
    }
    out
}

/// Resource epoch a cursor sits inside, resolving Atom/Op cursors through
/// the AppView's per-atom / per-op-epoch mappings. Used by the clamp routine
/// to find a still-visible header in the same section after a collapse.
fn epochs_cursor_epoch(view: &AppView, cursor: EpochsCursor) -> Option<usize> {
    match cursor {
        EpochsCursor::EpochHeader { epoch } | EpochsCursor::PhaseHeader { epoch, .. } => {
            Some(epoch)
        }
        EpochsCursor::Atom { arena_index } => view.epoch_of_atom(arena_index),
        EpochsCursor::Op { epoch_index, .. } => view
            .operation_epoch_meta(epoch_index)
            .map(|m| m.resource_epoch),
    }
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

/// True when a tree row's badge signals a difference worth jumping to.
/// `n`/`N` use this to skip past `Ok`/`Planned`/`Running` while still
/// stopping on collapsed branches whose rollup contains a change.
fn is_change_target(row: &TreeRow) -> bool {
    matches!(row.badge, Badge::Changed | Badge::Failed)
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

    // ------------------------------------------------------------------
    // Epochs page
    // ------------------------------------------------------------------

    fn command_op(label: &str) -> lusid_operation::Operation {
        use lusid_operation::operations::command::{CommandExecutor, CommandOperation};
        lusid_operation::Operation::Command(CommandOperation {
            command: label.into(),
            executor: CommandExecutor::Shell,
        })
    }

    /// Three-leaf view with `PipelineInfo` populating `atom_epoch`. Leaves at
    /// arena indices 2, 3, 5; epochs 0, 0, 1 respectively (alpha's leaves in
    /// epoch 0, beta's leaf in epoch 1).
    fn epochs_view() -> AppView {
        let view = view_with_two_branches();
        let atom_epoch: HashMap<usize, usize> = [(2, 0), (3, 0), (5, 1)].into_iter().collect();
        view.update(AppUpdate::PipelineInfo {
            resource_epochs_total: 2,
            atom_epoch,
        })
        .unwrap()
    }

    #[test]
    fn epochs_rows_render_a_section_per_resource_epoch() {
        let view = epochs_view();
        let state = EpochsPageState::default();
        let rows = build_epochs_rows(&view, &state);

        let headers: Vec<usize> = rows
            .iter()
            .filter_map(|r| match r.cursor {
                EpochsCursor::EpochHeader { epoch } => Some(epoch),
                _ => None,
            })
            .collect();
        assert_eq!(headers, vec![0, 1]);

        let atoms_epoch_0: Vec<usize> = rows
            .iter()
            .filter_map(|r| match r.cursor {
                EpochsCursor::Atom { arena_index } => Some(arena_index),
                _ => None,
            })
            .filter(|i| matches!(i, 2 | 3))
            .collect();
        assert_eq!(atoms_epoch_0, vec![2, 3]);
    }

    /// Each section must include both phase headers, even when no op events
    /// have arrived. The Phase B header carries the explicit `(no on_change
    /// handlers fired)` annotation so operators can tell empty-by-design from
    /// missing-data.
    #[test]
    fn epochs_rows_include_phase_headers_with_empty_b_annotation() {
        let view = epochs_view();
        let state = EpochsPageState::default();
        let rows = build_epochs_rows(&view, &state);
        let phase_a_rows: Vec<&EpochsRow> = rows
            .iter()
            .filter(|r| {
                matches!(
                    r.cursor,
                    EpochsCursor::PhaseHeader {
                        phase: Phase::A,
                        ..
                    }
                )
            })
            .collect();
        assert_eq!(phase_a_rows.len(), 2, "one Phase A header per epoch");
        let phase_b_rows: Vec<&EpochsRow> = rows
            .iter()
            .filter(|r| {
                matches!(
                    r.cursor,
                    EpochsCursor::PhaseHeader {
                        phase: Phase::B,
                        ..
                    }
                )
            })
            .collect();
        assert_eq!(phase_b_rows.len(), 2, "one Phase B header per epoch");
        for r in phase_b_rows {
            assert!(
                r.label.contains("no on_change handlers fired"),
                "phase B label: {}",
                r.label,
            );
        }
    }

    /// Collapsing an epoch header strips the section's atoms and phase rows
    /// from the visible list - only the header itself remains.
    #[test]
    fn epochs_collapse_hides_section_body() {
        let view = epochs_view();
        let mut state = EpochsPageState::default();
        state.collapsed.insert(0);
        let rows = build_epochs_rows(&view, &state);
        // Epoch 0's body should be gone; epoch 1's body should remain.
        let epoch_0_atoms = rows
            .iter()
            .any(|r| matches!(r.cursor, EpochsCursor::Atom { arena_index: 2 | 3 }));
        assert!(!epoch_0_atoms, "epoch 0's atoms hidden");
        let epoch_1_atoms = rows
            .iter()
            .any(|r| matches!(r.cursor, EpochsCursor::Atom { arena_index: 5 }));
        assert!(epoch_1_atoms, "epoch 1's atoms still visible");
    }

    #[test]
    fn epochs_phase_a_op_rows_appear_under_phase_header() {
        let view = epochs_view()
            .update(AppUpdate::OperationsApplyEpochAdded {
                epoch_index: 0,
                resource_epoch: 0,
                phase: Phase::A,
                operations: vec![command_op("op-a"), command_op("op-b")],
            })
            .unwrap();
        let state = EpochsPageState::default();
        let rows = build_epochs_rows(&view, &state);
        // Phase A op events should appear directly after Phase A's header,
        // before Phase B's header for the same epoch.
        let phase_a_pos = rows
            .iter()
            .position(|r| {
                matches!(
                    r.cursor,
                    EpochsCursor::PhaseHeader {
                        epoch: 0,
                        phase: Phase::A
                    }
                )
            })
            .expect("phase A header");
        let phase_b_pos = rows
            .iter()
            .position(|r| {
                matches!(
                    r.cursor,
                    EpochsCursor::PhaseHeader {
                        epoch: 0,
                        phase: Phase::B
                    }
                )
            })
            .expect("phase B header");
        let ops_between: Vec<&EpochsRow> = rows[phase_a_pos + 1..phase_b_pos]
            .iter()
            .filter(|r| matches!(r.cursor, EpochsCursor::Op { .. }))
            .collect();
        assert_eq!(ops_between.len(), 2);
    }

    /// Phase B handlers must surface under the epoch that scheduled them, not
    /// under some other epoch. The wire ships `resource_epoch` per
    /// `OperationsApplyEpochAdded` event; the page groups by that field.
    #[test]
    fn epochs_phase_b_handlers_land_under_their_resource_epoch() {
        let view = epochs_view()
            .update(AppUpdate::OperationsApplyEpochAdded {
                epoch_index: 0,
                resource_epoch: 1,
                phase: Phase::B,
                operations: vec![command_op("reload-nginx")],
            })
            .unwrap();
        let state = EpochsPageState::default();
        let rows = build_epochs_rows(&view, &state);
        // The handler op should sit under epoch 1's Phase B section, after
        // epoch 1's Phase B header. Epoch 0's Phase B should still show the
        // empty annotation.
        let mut current_epoch = None;
        let mut current_phase = None;
        for row in &rows {
            match row.cursor {
                EpochsCursor::EpochHeader { epoch } => current_epoch = Some(epoch),
                EpochsCursor::PhaseHeader { phase, .. } => current_phase = Some(phase),
                EpochsCursor::Op { .. } => {
                    assert_eq!(current_epoch, Some(1));
                    assert_eq!(current_phase, Some(Phase::B));
                }
                _ => {}
            }
        }
    }

    /// An atom whose parent branch declares `requires: [<id>]` must render
    /// the `← requires: <id> (epoch K)` annotation when the id resolves to a
    /// branch in an earlier epoch.
    #[test]
    fn epochs_atom_row_shows_requires_annotation_across_epochs() {
        let upstream = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(pi_id("install")),
                ..PlanMeta::default()
            },
            children: vec![resource_leaf("/install")],
        };
        let downstream = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(pi_id("reload")),
                requires: vec![pi_id("install")],
                ..PlanMeta::default()
            },
            children: vec![resource_leaf("/reload")],
        };
        let root = PlanTree::Branch {
            meta: PlanMeta::default(),
            children: vec![upstream, downstream],
        };
        // Arena: 0=root, 1=install branch, 2=/install leaf, 3=reload branch, 4=/reload leaf.
        let atom_epoch: HashMap<usize, usize> = [(2, 0), (4, 1)].into_iter().collect();
        let view = AppView::default()
            .update(AppUpdate::ResourcesStart)
            .unwrap()
            .update(AppUpdate::ResourcesNode {
                index: 0,
                tree: root,
            })
            .unwrap()
            .update(AppUpdate::PipelineInfo {
                resource_epochs_total: 2,
                atom_epoch,
            })
            .unwrap();
        let state = EpochsPageState::default();
        let rows = build_epochs_rows(&view, &state);
        let reload_row = rows
            .iter()
            .find(|r| matches!(r.cursor, EpochsCursor::Atom { arena_index: 4 }))
            .expect("reload atom row");
        let ann = reload_row.annotation.as_ref().expect("annotation present");
        assert!(ann.contains("install"), "annotation: {ann}");
        // Epoch numbers in operator-facing text are 1-based to match the
        // header strip; the upstream `install` plan-item ran in 0-based
        // epoch 0, displayed as `epoch 1`.
        assert!(ann.contains("epoch 1"), "annotation: {ann}");
    }

    /// Header rows must use 1-based epoch numbers (`Epoch K/N`) to match the
    /// global header strip's `epoch K/N` and avoid mixing conventions.
    #[test]
    fn epochs_header_label_uses_one_based_numbering() {
        let view = epochs_view();
        let state = EpochsPageState::default();
        let rows = build_epochs_rows(&view, &state);
        let labels: Vec<&str> = rows
            .iter()
            .filter(|r| matches!(r.cursor, EpochsCursor::EpochHeader { .. }))
            .map(|r| r.label.as_str())
            .collect();
        assert!(
            labels.iter().any(|l| l.starts_with("Epoch 1/2")),
            "labels: {labels:?}",
        );
        assert!(
            labels.iter().any(|l| l.starts_with("Epoch 2/2")),
            "labels: {labels:?}",
        );
    }

    #[test]
    fn epochs_collapse_clamps_selection_to_remaining_header() {
        let view = epochs_view();
        let mut app = TuiApp::new("test".into());
        app.app_view = view;
        // Atom 5 lives in epoch 1 (see `epochs_view`); collapsing epoch 1
        // hides it and the clamp must land on epoch 1's header, not bounce
        // back to the first row (epoch 0's header).
        app.epochs.selected = Some(EpochsCursor::Atom { arena_index: 5 });

        app.epochs.collapsed.insert(1);
        app.clamp_epochs_selection_to_visible();
        assert_eq!(
            app.epochs.selected,
            Some(EpochsCursor::EpochHeader { epoch: 1 }),
        );
    }

    /// An empty resource epoch (no atoms map to it) is valid and must still
    /// render: header, both phase headers, no atom rows. Operators see the
    /// section so they know nothing was scheduled there.
    #[test]
    fn epochs_empty_resource_epoch_still_renders_section() {
        let view = view_with_two_branches();
        let atom_epoch: HashMap<usize, usize> = [(2, 0), (3, 0), (5, 0)].into_iter().collect();
        let view = view
            .update(AppUpdate::PipelineInfo {
                resource_epochs_total: 2,
                atom_epoch,
            })
            .unwrap();
        let state = EpochsPageState::default();
        let rows = build_epochs_rows(&view, &state);

        // Epoch 1's header is present even though no atoms land in it.
        let epoch_1_header = rows
            .iter()
            .find(|r| matches!(r.cursor, EpochsCursor::EpochHeader { epoch: 1 }))
            .expect("epoch 1 header");
        assert!(
            epoch_1_header.label.contains("0 atoms"),
            "label: {}",
            epoch_1_header.label,
        );
        // No atom rows under epoch 1; both its phase headers still present.
        let atom_rows_in_1 = rows
            .iter()
            .filter(|r| matches!(r.cursor, EpochsCursor::Atom { arena_index } if [2,3,5].contains(&arena_index) && view.epoch_of_atom(arena_index) == Some(1)))
            .count();
        assert_eq!(atom_rows_in_1, 0);
        let phase_rows_in_1 = rows
            .iter()
            .filter(|r| matches!(r.cursor, EpochsCursor::PhaseHeader { epoch: 1, .. }))
            .count();
        assert_eq!(phase_rows_in_1, 2);
    }

    /// A `requires` edge that resolves to the same epoch (or later) adds no
    /// signal about why this atom ran where it did - the annotation is
    /// reserved for cross-epoch edges per the spec.
    #[test]
    fn epochs_same_epoch_requires_does_not_annotate() {
        let a = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(pi_id("a")),
                ..PlanMeta::default()
            },
            children: vec![resource_leaf("/a")],
        };
        let b = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(pi_id("b")),
                requires: vec![pi_id("a")],
                ..PlanMeta::default()
            },
            children: vec![resource_leaf("/b")],
        };
        let root = PlanTree::Branch {
            meta: PlanMeta::default(),
            children: vec![a, b],
        };
        // Both atoms in epoch 0.
        let atom_epoch: HashMap<usize, usize> = [(2, 0), (4, 0)].into_iter().collect();
        let view = AppView::default()
            .update(AppUpdate::ResourcesStart)
            .unwrap()
            .update(AppUpdate::ResourcesNode {
                index: 0,
                tree: root,
            })
            .unwrap()
            .update(AppUpdate::PipelineInfo {
                resource_epochs_total: 1,
                atom_epoch,
            })
            .unwrap();
        let state = EpochsPageState::default();
        let rows = build_epochs_rows(&view, &state);
        let b_row = rows
            .iter()
            .find(|r| matches!(r.cursor, EpochsCursor::Atom { arena_index: 4 }))
            .expect("b atom row");
        assert!(
            b_row.annotation.is_none(),
            "annotation: {:?}",
            b_row.annotation
        );
    }

    /// Without `PipelineInfo`, the Epochs page can't know how many epochs
    /// exist - the row list must be empty so the renderer falls through to
    /// the "waiting for pipeline info" placeholder.
    #[test]
    fn epochs_rows_empty_until_pipeline_info_arrives() {
        let view = view_with_two_branches();
        let state = EpochsPageState::default();
        assert!(build_epochs_rows(&view, &state).is_empty());
    }

    /// Op rows must derive their badge from the live `OperationView` so the
    /// epochs page tracks running/complete/failed state during apply.
    #[test]
    fn epochs_op_badge_reflects_operation_view_state() {
        let view = epochs_view()
            .update(AppUpdate::OperationsApplyEpochAdded {
                epoch_index: 0,
                resource_epoch: 0,
                phase: Phase::A,
                operations: vec![command_op("op-x"), command_op("op-y")],
            })
            .unwrap()
            .update(AppUpdate::OperationApplyStart { index: (0, 0) })
            .unwrap()
            .update(AppUpdate::OperationApplyComplete {
                index: (0, 1),
                error: Some("boom".into()),
            })
            .unwrap();
        let state = EpochsPageState::default();
        let rows = build_epochs_rows(&view, &state);
        let op_rows: Vec<&EpochsRow> = rows
            .iter()
            .filter(|r| matches!(r.cursor, EpochsCursor::Op { .. }))
            .collect();
        assert_eq!(op_rows.len(), 2);
        assert_eq!(op_rows[0].badge, Some(Badge::Running));
        assert_eq!(op_rows[1].badge, Some(Badge::Failed));
    }

    // ------------------------------------------------------------------
    // n / N jump-to-change
    // ------------------------------------------------------------------

    /// A view with a Changed leaf at arena index 3 and an Ok leaf at arena
    /// index 2. Branch alpha (index 1) rolls up Changed (mix of Ok and
    /// Changed children); branch beta (4) and its leaf (5) stay Planned.
    fn view_with_one_change() -> AppView {
        view_with_two_branches()
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
            .unwrap()
    }

    /// Pressing `n` from the root walks forward to the first change-bearing
    /// row. With `view_with_one_change`, alpha (arena 1) rolls up Changed,
    /// so it gets selected before alpha's `/a/2` leaf (the actual change).
    #[test]
    fn tree_n_jumps_to_first_change_target() {
        let mut app = TuiApp::new("test".into());
        app.app_view = view_with_one_change();
        app.tree.selected = Some(0);

        app.handle_event_tree(KeyCode::Char('n'));

        assert_eq!(app.tree.selected, Some(1), "alpha branch rolls up Changed");
        assert!(app.tree.toast.is_none());
    }

    /// From the Changed branch itself, `n` descends into the first Changed
    /// descendant - alpha's `/a/2` at arena index 3. The Ok leaf at index 2
    /// is skipped.
    #[test]
    fn tree_n_skips_ok_leaves() {
        let mut app = TuiApp::new("test".into());
        app.app_view = view_with_one_change();
        app.tree.selected = Some(1);

        app.handle_event_tree(KeyCode::Char('n'));

        assert_eq!(app.tree.selected, Some(3));
        assert!(app.tree.toast.is_none());
    }

    /// Past the last change-bearing row, `n` is a no-op: selection stays
    /// and the footer toast surfaces.
    #[test]
    fn tree_n_past_last_change_sets_toast() {
        let mut app = TuiApp::new("test".into());
        app.app_view = view_with_one_change();
        app.tree.selected = Some(3);

        app.handle_event_tree(KeyCode::Char('n'));

        assert_eq!(app.tree.selected, Some(3));
        assert_eq!(app.tree.toast.as_deref(), Some("no more changes"));
    }

    /// `N` walks the same set in reverse. From `/a/2` (arena 3), the
    /// previous change target is alpha (arena 1).
    #[test]
    fn tree_capital_n_walks_backward() {
        let mut app = TuiApp::new("test".into());
        app.app_view = view_with_one_change();
        app.tree.selected = Some(3);

        app.handle_event_tree(KeyCode::Char('N'));

        assert_eq!(app.tree.selected, Some(1));
        assert!(app.tree.toast.is_none());
    }

    /// All leaves Planned (no changes anywhere); `n` is a no-op and the
    /// toast appears so the operator isn't left wondering whether the key
    /// did anything.
    #[test]
    fn tree_n_with_no_changes_sets_toast() {
        let mut app = TuiApp::new("test".into());
        app.app_view = view_with_two_branches();
        app.tree.selected = Some(0);

        app.handle_event_tree(KeyCode::Char('n'));

        assert_eq!(app.tree.selected, Some(0));
        assert_eq!(app.tree.toast.as_deref(), Some("no more changes"));
    }

    /// Any subsequent keypress clears the prior `n`/`N` toast; it's a
    /// one-shot indicator, not a sticky message.
    #[test]
    fn tree_toast_clears_on_next_keypress() {
        let mut app = TuiApp::new("test".into());
        app.app_view = view_with_two_branches();
        app.tree.selected = Some(0);

        app.handle_event_tree(KeyCode::Char('n'));
        assert!(app.tree.toast.is_some());

        app.handle_event_tree(KeyCode::Char('j'));
        assert!(app.tree.toast.is_none());
    }

    // ------------------------------------------------------------------
    // (epoch K) branch annotation
    // ------------------------------------------------------------------

    /// After `PipelineInfo` arrives, named plan-item branches carry the
    /// latest descendant epoch. The anonymous root does not - the global
    /// header strip already shows `epoch K/N`, so a duplicate tag here
    /// would be noise.
    #[test]
    fn tree_branch_rows_carry_epoch_after_pipeline_info() {
        let view = view_with_two_branches()
            .update(AppUpdate::PipelineInfo {
                resource_epochs_total: 2,
                atom_epoch: [(2, 0), (3, 0), (5, 1)].into_iter().collect(),
            })
            .unwrap();
        let state = TreePageState::new();
        let rows = build_visible_rows(&view, &state);

        let alpha = rows
            .iter()
            .find(|r| r.arena_index == 1)
            .expect("alpha branch");
        assert_eq!(alpha.epoch, Some(0), "alpha's leaves are both in epoch 0");

        let beta = rows
            .iter()
            .find(|r| r.arena_index == 4)
            .expect("beta branch");
        assert_eq!(beta.epoch, Some(1), "beta's leaf is in epoch 1");

        let root = rows
            .iter()
            .find(|r| r.arena_index == 0)
            .expect("root branch");
        assert!(root.epoch.is_none(), "anonymous root carries no epoch tag");

        // Leaves never carry the tag - the spec reserves it for plan-item
        // branches.
        for leaf in rows.iter().filter(|r| !r.is_branch) {
            assert!(
                leaf.epoch.is_none(),
                "leaf at {} got an epoch tag",
                leaf.arena_index
            );
        }
    }

    /// Without `PipelineInfo`, no atom_epoch entries exist and the
    /// `latest_epoch_by_branch` map is empty - every branch row reports
    /// `epoch: None` and the tag is suppressed.
    #[test]
    fn tree_branch_rows_have_no_epoch_until_pipeline_info_arrives() {
        let view = view_with_two_branches();
        let state = TreePageState::new();
        let rows = build_visible_rows(&view, &state);
        for row in &rows {
            assert!(
                row.epoch.is_none(),
                "row {} has epoch before pipeline info",
                row.arena_index
            );
        }
    }

    /// A branch whose descendant atoms span multiple epochs reports the
    /// latest one - matching `build_latest_epoch_by_branch` and so
    /// matching the "when does this branch finish?" question the tag
    /// answers.
    #[test]
    fn tree_branch_epoch_picks_latest_descendant() {
        let mixed = PlanTree::Branch {
            meta: PlanMeta {
                id: Some(pi_id("mixed")),
                ..PlanMeta::default()
            },
            children: vec![resource_leaf("/m/early"), resource_leaf("/m/late")],
        };
        let root = PlanTree::Branch {
            meta: PlanMeta::default(),
            children: vec![mixed],
        };
        // Arena: 0=root, 1=mixed, 2=/m/early (epoch 0), 3=/m/late (epoch 2).
        let view = AppView::default()
            .update(AppUpdate::ResourcesStart)
            .unwrap()
            .update(AppUpdate::ResourcesNode {
                index: 0,
                tree: root,
            })
            .unwrap()
            .update(AppUpdate::PipelineInfo {
                resource_epochs_total: 3,
                atom_epoch: [(2, 0), (3, 2)].into_iter().collect(),
            })
            .unwrap();
        let state = TreePageState::new();
        let rows = build_visible_rows(&view, &state);
        let mixed = rows
            .iter()
            .find(|r| r.arena_index == 1)
            .expect("mixed branch");
        assert_eq!(mixed.epoch, Some(2));
    }

    // ------------------------------------------------------------------
    // Help overlay
    // ------------------------------------------------------------------

    fn key_event(code: KeyCode) -> Event {
        Event::Key(KeyEvent::new(code, KeyModifiers::NONE))
    }

    #[test]
    fn help_overlay_opens_and_closes_on_question_mark() {
        let mut app = TuiApp::new("test".into());
        assert!(!app.show_help);
        app.handle_event(key_event(KeyCode::Char('?'))).unwrap();
        assert!(app.show_help);
        app.handle_event(key_event(KeyCode::Char('?'))).unwrap();
        assert!(!app.show_help);
    }

    #[test]
    fn help_overlay_closes_on_esc() {
        let mut app = TuiApp::new("test".into());
        app.app_view = view_with_two_branches();
        app.tree.selected = Some(3);
        app.handle_event(key_event(KeyCode::Char('?'))).unwrap();
        assert!(app.show_help);
        app.handle_event(key_event(KeyCode::Esc)).unwrap();
        assert!(!app.show_help);
        assert_eq!(app.tree.selected, Some(3), "selection unchanged");
    }

    #[test]
    fn help_overlay_does_not_block_quit() {
        let mut app = TuiApp::new("test".into());
        app.handle_event(key_event(KeyCode::Char('?'))).unwrap();
        assert!(app.show_help);
        let quit = app.handle_event(key_event(KeyCode::Char('q'))).unwrap();
        assert!(quit, "q must still quit while help is open");
    }

    // ------------------------------------------------------------------
    // Tab strip
    // ------------------------------------------------------------------

    fn render_tab_strip(width: u16, app: &TuiApp) -> ratatui::buffer::Buffer {
        use ratatui::Terminal;
        use ratatui::backend::TestBackend;
        let backend = TestBackend::new(width, 1);
        let mut terminal = Terminal::new(backend).unwrap();
        terminal
            .draw(|frame| {
                let area = frame.area();
                draw_tab_strip(frame, area, app);
            })
            .unwrap();
        terminal.backend().buffer().clone()
    }

    fn row_text(buffer: &ratatui::buffer::Buffer, y: u16) -> String {
        (0..buffer.area.width)
            .map(|x| buffer.cell((x, y)).unwrap().symbol().to_string())
            .collect()
    }

    fn segment_has_underline(buffer: &ratatui::buffer::Buffer, y: u16, needle: &str) -> bool {
        let text = row_text(buffer, y);
        let Some(start) = text.find(needle) else {
            return false;
        };
        let end = start + needle.len();
        // Map byte index back to a char index since text is ASCII here.
        for x in start as u16..end as u16 {
            let cell = buffer.cell((x, y)).unwrap();
            if !cell.modifier.contains(Modifier::UNDERLINED) {
                return false;
            }
        }
        true
    }

    #[test]
    fn tab_strip_marks_active_page() {
        for (page, label) in [
            (UiPage::Tree, "[1 Tree]"),
            (UiPage::Epochs, "[2 Epochs]"),
            (UiPage::Stderr, "[e Stderr]"),
        ] {
            let mut app = TuiApp::new("test".into());
            app.page = page;
            let buffer = render_tab_strip(80, &app);
            assert!(
                segment_has_underline(&buffer, 0, label),
                "page {page:?} should underline {label}; row: {:?}",
                row_text(&buffer, 0)
            );
        }
    }

    #[test]
    fn tab_strip_compacts_below_60_cols() {
        let app = TuiApp::new("test".into());
        let buffer = render_tab_strip(50, &app);
        let text = row_text(&buffer, 0);
        assert!(text.contains("[1]"), "row: {text:?}");
        assert!(text.contains("[2]"), "row: {text:?}");
        assert!(text.contains("[e]"), "row: {text:?}");
        assert!(!text.contains("Tree"), "labels dropped: {text:?}");
        assert!(!text.contains("Epochs"), "labels dropped: {text:?}");
        assert!(!text.contains("Stderr"), "labels dropped: {text:?}");
    }

    #[test]
    fn help_overlay_passes_through_confirm_keys() {
        let summary = lusid_apply_stdio::EpochSummary {
            atoms_total: 1,
            atoms_changed: 1,
            handlers_pending: 0,
            change_labels: vec![],
            truncated_count: 0,
        };
        let mut app = TuiApp::new("test".into());
        app.app_view.pending_epoch = Some((0, summary));
        app.show_help = true;

        app.handle_event(key_event(KeyCode::Char('n'))).unwrap();
        assert_eq!(app.pending_ack, Some(AckAction::Abort));
        assert!(app.show_help, "overlay stays open after confirm answer");

        // Re-stage a pending confirm and try the Apply path.
        app.pending_ack = None;
        app.app_view.pending_epoch = Some((
            0,
            lusid_apply_stdio::EpochSummary {
                atoms_total: 1,
                atoms_changed: 1,
                handlers_pending: 0,
                change_labels: vec![],
                truncated_count: 0,
            },
        ));
        app.handle_event(key_event(KeyCode::Enter)).unwrap();
        assert_eq!(app.pending_ack, Some(AckAction::Apply));
        assert!(app.show_help);
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
