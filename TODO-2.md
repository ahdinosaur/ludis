# Phase 2: TUI v2 (post-feedback iteration)

After Phase 1 landed the Tasks 7-17 TUI, the operator gave feedback on first use (see `/tmp/lusid-ui-v2-design.md` for the full design notes). Phase 2 lands six tasks polishing footer, navigation, follow behaviour, and the Tree/Epochs detail panes.

Land each task as one commit on the `new-ui` branch. No `Co-Authored-By: Claude` trailer.

---

## Task 18 — Slim footer + working help overlay

**Status:** pending

**Goal:** Trim the per-page footer to a single line of essentials and route the full key reference into a modal help overlay toggled by `?`.

**Why:** Operator feedback: "too much in the footer, should just show the basics and mention a help page... and if there's currently a help page, it isn't working." The `?` glyph is documented in Task 12's keymap but never bound.

**Files to read first:**
- `lusid/src/tui/mod.rs`: `draw_footer`, `footer_hint`, `handle_event`, `handle_event_tree`, `handle_event_epochs`, `handle_event_stderr`
- `lusid/src/tui/palette.rs`

**Plan:**
1. Add `show_help: bool` to `TuiApp` (default `false`). Bind `?` to toggle.
2. Add `draw_help_overlay(frame, body_rect, app)`. Render a centred `Block::bordered` over the body rect from the top-level layout split (anchor it to whatever rect the body occupies, so Task 19's tab strip insertion does not re-shuffle the anchor). Width = `min(60, body.width.saturating_sub(4))`; height = content rows + 2. Content groups: Navigation, View, Pages, Confirm, q.
3. Rewrite `footer_hint`:
   - Normal: `1/2/e pages · Tab focus · f follow · ? help · q quit`
   - Filter editing: unchanged.
   - Confirm prompt active (`pending_epoch.is_some()`): `Epoch K/N · A atoms, H handlers · ↵/y apply · n/Esc abort · ? help`. Note `n/Esc` (Task B in the design doc; the help overlay's Confirm group must match).
   - Toast (n/N "no more changes"): unchanged.
4. Key precedence in `handle_event` while `show_help`:
   1. If `app.app_view.pending_epoch.is_some()`, `Enter`/`y`/`n`/`Esc` still drive the confirm (operator must never be locked out of the ack channel). `Esc` here means "abort", not "close help".
   2. Else `?` or `Esc` closes the overlay.
   3. Else `q` quits.
   4. Else swallow.
5. Tests in `lusid/src/tui/mod.rs`:
   - `help_overlay_opens_and_closes_on_question_mark`: construct `TuiApp`, call `handle_event` with `?`, assert `show_help` flipped on; press `?` again, assert off.
   - `help_overlay_closes_on_esc`: open with `?`, press `Esc`, assert off and selection unchanged.
   - `help_overlay_does_not_block_quit`: open with `?`, press `q`, assert handler returns `true`.
   - `help_overlay_passes_through_confirm_keys`: stage `app_view.pending_epoch = Some(...)`, open help, press `n`, assert `pending_ack == Some(AckAction::Abort)` and overlay stays open. Mirror with `Enter` -> `Apply`.
   - Use the existing `epoch_ready_sets_pending_until_first_op_for_that_epoch` test in `apply-stdio/src/lib.rs` as the template for constructing a pending state.

**Acceptance criteria:**
- Footer fits one line on the nginx-cluster example at 80 cols.
- `?` opens the overlay; `?` or `Esc` closes; `q` quits with it open.
- Confirm keys (`Enter`/`y`/`n`/`Esc`) still work when the overlay is open and a confirm is pending.
- Help overlay anchoring works after Task 19 lands (verify by running both tasks locally before committing 19).

**Verify:**
```
cargo build --workspace
cargo test -p lusid
cargo clippy --workspace -- -D warnings
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local parse
# Press ?, then Esc, then ? again, then q.
```

**Pitfalls:**
- Centring math: the overlay is rendered inside the body rect, not the frame. Use `Rect::inner` or manual padding off the body rect, not the frame.
- Don't intercept keys before the confirm-key block; route help-overlay state-machine into the existing top-of-`handle_event` short-circuit.

---

## Task 19 — Tab strip

**Status:** pending

**Goal:** Add a 1-row tab strip below the header strip showing `[1 Tree] [2 Epochs] [e Stderr]` so operators see the page selectors at a glance.

**Why:** Operator feedback: "should show tree | epochs | stderr as tabs, so you know there are other pages (add 1, 2, or e indicators next to the labels)."

**Files to read first:**
- `lusid/src/tui/mod.rs`: `draw_ui`, `draw_header`
- Task 18 output (the help overlay is anchored to the body rect from the same layout split; this task reshapes that split).

**Plan:**
1. Change the top-level layout in `draw_ui` from `[Length(1), Min(3), Length(1)]` to `[Length(1) header, Length(1) tabs, Min(3) body, Length(1) footer]`. Update all draw call indices.
2. Add `draw_tab_strip(frame, area, app)`. Three tabs: `[1 Tree]`, `[2 Epochs]`, `[e Stderr]`, separated by spaces. Active tab uses bold + cyan + `Modifier::UNDERLINED`; inactive uses `DarkGray`.
3. Width fallback: when `area.width < 60`, render compact `[1] [2] [e]` (drop the labels).
4. Confirm the help overlay (Task 18) still centres correctly inside the new (smaller) body rect.
5. Tests in `lusid/src/tui/mod.rs`:
   - `tab_strip_marks_active_page`: render the strip for each `app.page` value into a buffer (use ratatui's `TestBackend`), assert the active tab segment carries `Modifier::UNDERLINED`.
   - `tab_strip_compacts_below_60_cols`: render at width 50, assert the strip text contains `[1] [2] [e]` (no labels).

**Acceptance criteria:**
- Tab strip renders on all three pages with the active tab highlighted.
- Body height shrinks by one row to accommodate the strip; existing pages still draw correctly.
- Help overlay (from Task 18) still anchors inside the body rect.
- Compact form kicks in below 60 cols.

**Verify:**
```
cargo test -p lusid
cargo clippy --workspace -- -D warnings
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local parse
# Resize terminal across 60 cols; confirm tab strip compacts and expands.
```

**Pitfalls:**
- Layout index drift: `draw_ui`'s match on `app.page` now reads `layout[2]`, footer reads `layout[3]`.

---

## Task 20 — Branch detail polish: drop children-count, indent resources

**Status:** pending

**Goal:** On the Tree page detail pane for a plan-item branch, drop the `children: N atom(s)` line and replace it with a "Resources" section listing the branch's resources in an indented hierarchy.

**Why:** Operator feedback: "for plan item detail: children is kinda silly. Also, could there be pretty indentation on the resource names and ids?"

**Files to read first:**
- `lusid/src/tui/mod.rs`: `detail_for_branch`, `walk_atoms`
- `render/src/lib.rs`: `RenderedNode::Tree`, `to_plain_string`, `to_ratatui_text`
- `resource/src/lib.rs`: `Resource` enum and per-family modules

**Plan:**
1. In `lusid-resource` (`resource/src/lib.rs`), add `impl Resource { pub fn family_name(&self) -> &'static str { ... } }` matching every variant of `Resource` to a stable lowercase identifier. Verify the exact variant set by reading the `Resource` enum definition in `resource/src/lib.rs` first; expected (subject to verification): `apt`, `apt_repo`, `aur`, `command`, `directory`, `file`, `flatpak`, `flatpak_remote`, `git`, `group`, `pacman`, `podman`, `secret`, `systemd`, `user`. Unit test covers every arm.
2. In `tui/mod.rs::detail_for_branch`:
   - Remove the `field_line("children", ...)` line.
   - After the `id` field and before `Requires`, insert a "Resources" section header.
   - Walk the branch's descendant leaves in arena order. Note: the existing `walk_atoms` is typed `F: FnMut(Badge)` (mod.rs:2317), so it cannot be reused directly. Either generalise it to a generic visitor (`F: FnMut(&LeafState)` plus the per-leaf collection happens in the closure) and migrate the existing `rollup_for_branch` call site, OR write a new sibling helper `walk_branch_leaves<F: FnMut(&LeafState)>(...)`. Pick the lower-churn option.
   - Build a `RenderedNode::Tree` with `RenderedNode::Plain("Resources")` as the root and one `RenderedNode::Plain(format!("{family}  {display}"))` per leaf.
   - Call `extend_lines` on the resulting node to push styled lines.
3. Tests: a two-leaf branch renders with both family/path rows under the Resources section; an empty branch (no leaves) renders no Resources section.

**Acceptance criteria:**
- Branch detail no longer shows `children: N atom(s)`.
- Resources section shows `family  resource-display` indented under the branch.
- `Resource::family_name()` exists in `lusid-resource` with a unit test.

**Verify:**
```
cargo test -p lusid-resource
cargo test -p lusid
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local parse
# Navigate to a branch row and confirm the detail pane shows the indented Resources list.
```

**Pitfalls:**
- `termtree` produces a multi-line string. `extend_lines` already flattens multi-line `Plain` content via `to_ratatui_text`; verify the indented tree lines render correctly inside the detail pane (no stray blank lines).
- Family name must be stable; don't localise.

---

## Task 21 — Epoch phases: conditional rendering + rename + pending preview

**Status:** pending

**Goal:** Hide the "Phase A"/"Phase B" subsection headers until ops actually arrive for that phase, rename them to "Plan Operations" / "Change Event Operations", and add a non-selectable preview line under the pending epoch's header showing `EpochSummary` counts.

**Why:** Operator feedback: "the phase A and B makes no sense, especially when they are showing for an epoch that hasn't run yet. Show these only when the epoch has run, and maybe not call them phase a and b, but Plan Operations and Change Event Operations." The pending-preview line preserves the handler-count forecast that would otherwise disappear with the Phase B header.

**Files to read first:**
- `lusid/src/tui/mod.rs`: `build_epochs_rows`, `detail_for_phase_header`, the `epochs_rows_*` tests
- `apply-stdio/src/lib.rs`: `EpochSummary`, `AppView::pending_epoch`

**Plan:**
1. In `build_epochs_rows`, only emit `EpochsCursor::PhaseHeader { phase: Phase::A, .. }` when `phase_a` is non-empty. Same for Phase B.
2. Rename the labels:
   - `Phase A · N op event(s)` -> `Plan Operations · N op event(s)`
   - `Phase B · N op event(s)` -> `Change Event Operations · N op event(s)`
3. Drop the `Phase B · (no on_change handlers fired)` row entirely.
4. Add a pending-preview row immediately after the epoch header when `view.pending_epoch.as_ref().map(|(e, _)| *e) == Some(epoch)`. The row is non-selectable (give it a new `EpochsCursor` variant, e.g. `EpochsCursor::PendingPreview { epoch }`, but filter it out of `epochs_move` so navigation skips it). Label: `Pending: {atoms_changed} atom changes, {handlers_pending} handlers`, styled dim/yellow.
5. Rename in `detail_for_phase_header`:
   - `Phase A: change ops produced by this epoch's atoms` -> `Plan Operations: change ops produced by this epoch's atoms`
   - `Phase B: on_change handlers fired by this epoch's branches` -> `Change Event Operations: on_change handlers fired by this epoch's branches`
6. Tests:
   - `epochs_rows_include_phase_headers_with_empty_b_annotation` -> rewrite to assert *absence* of phase headers before ops arrive.
   - New: after an `OperationsApplyEpochAdded` Phase A event, the Phase A header appears.
   - New: after an `EpochReady` event sets `pending_epoch`, the pending-preview row appears under the matching epoch's header with the right counts; absent for other epochs.
   - New: `epochs_move` skips the `PendingPreview` cursor.

**Acceptance criteria:**
- Before any op events for an epoch, the section shows only the epoch header (+ pending preview if applicable) + atoms.
- After Phase A ops arrive, the "Plan Operations" header appears.
- After Phase B ops arrive, the "Change Event Operations" header appears. Otherwise it never appears.
- Pending preview line is present only for the pending epoch and shows the right counts.
- All existing epoch tests still pass after the rewrites.

**Verify:**
```
cargo test -p lusid
cargo clippy --workspace -- -D warnings
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local apply
# Watch the Epochs page through a confirm; the pending preview appears under the awaiting epoch.
```

**Pitfalls:**
- The `EpochsCursor::PendingPreview` variant must be skipped by `epochs_move` and `epochs_jump_first/last`, otherwise it becomes selectable.
- Don't break the clamp logic: a collapsed epoch must still skip the preview row.

---

## Task 22 — Follow mode (sticky toggle, auto-arms on first probe)

**Status:** pending

**Goal:** Add an `f`-toggled follow mode that tracks the latest activity across all three pages. Auto-arms once per apply on the first `Probing` transition; never re-arms after the operator has turned it off.

**Why:** Operator feedback: "is not obvious what is happening. should have a button to follow the latest where you are - on tree or epoch list, go to latest thing (node or plan item or epoch) happening - on detail or stderr, scroll to bottom and follow along."

**Files to read first:**
- `lusid/src/tui/mod.rs`: `TuiApp`, `handle_event_tree`, `handle_event_epochs`, `handle_event_stderr`, `draw_tree_page`, `draw_epochs_page`, `draw_stderr_page`
- `apply-stdio/src/lib.rs`: `AppView`, `LeafState`, `transition_leaf`, the `OperationApply*` variants

**Plan:**
1. Add to `AppView`:
   - `last_activity_atom: Option<usize>`: updated in `transition_leaf` when transitioning to `Probing`, `Probed`, or `Changed`. Do NOT update on `NoChange`. (Note(cc): tracking Probing/Probed causes brief cursor twitches across a probe storm; refine to terminal-state-only if operators report jitter.)
   - `last_activity_op: Option<(usize, usize)>`: updated on every `OperationApply*` lifecycle event handler in `AppView::update`.
   - `auto_follow_armed: bool`: set to `true` once at the first transition to `Probing`. Used by `TuiApp` to flip `follow` on once.
2. Add to `TuiApp`:
   - `follow: bool`, defaults to `false`.
3. In `apply_update`, use the stash-pre-then-check-post pattern to detect the one-shot arm:
   ```rust
   let pre_armed = self.app_view.auto_follow_armed;
   let current = std::mem::take(&mut self.app_view);
   self.app_view = current.update(update)?;
   if !pre_armed && self.app_view.auto_follow_armed {
       self.follow = true;
   }
   ```
   This makes the arm explicitly one-shot and avoids the boolean flap from a future re-set.
4. Bind `f` (in all three page handlers and the top-level handler): toggle `app.follow`.
5. Header strip: append ` · [follow]` (yellow, bold) when `app.follow`.
6. Per-frame draw, if `app.follow`:
   - **Tree page** (`draw_tree_page`): candidate = `view.last_activity_atom`. If `build_visible_rows` does not contain it, fall back via `clamp_tree_selection_to_visible`'s rule (largest visible arena index <= candidate). Set `tree.detail_scroll = u16::MAX`.
   - **Epochs page** (`draw_epochs_page`): if `view.last_activity_op.is_some()`, set `epochs.selected = EpochsCursor::Op { ... }` for that index. Else if `view.last_activity_atom.is_some()`, set `epochs.selected = EpochsCursor::Atom { arena_index: ... }`. Pin `epochs.detail_scroll = u16::MAX`.
   - **Stderr page** (`draw_stderr_page`): `app.stderr_follow = true`.
7. Disabling: any of `j`/`k`/`h`/`l`/`Space`/`gg`/`G`/`n`/`N`/`Up`/`Down`/`Left`/`Right`/`PageUp`/`PageDown`/`Home`/`/`, or answering a confirm prompt (`y`/`Enter`/`n`/`Esc` while `pending_epoch.is_some()`) sets `app.follow = false`. Do NOT disable on `Tab`, `u`, `s`, `1`/`2`/`e`, `f` (toggle), `?` (help open/close).
8. Tests:
   - `last_activity_atom` updates on Probing/Probed/Changed, skips NoChange.
   - `last_activity_op` updates on all four op lifecycle events.
   - `auto_follow_armed` flips once on first Probing; subsequent Probing transitions don't re-arm.
   - With follow on, `draw_*` produces the expected selection. Use `TuiApp` test helpers as in the existing tests.
   - Pressing `j` clears follow.
   - Pressing `Tab` does not clear follow.

**Acceptance criteria:**
- `f` toggles follow; header shows `[follow]` when on.
- Follow auto-arms on first `Probing` but not on `parse` (no Probing events emit).
- Selection follows latest activity on all three pages while follow is on.
- Any nav key clears follow; non-nav keys do not.
- Confirm-prompt answer clears follow.

**Verify:**
```
cargo test -p lusid-apply-stdio
cargo test -p lusid
cargo clippy --workspace -- -D warnings
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local apply
# Watch follow auto-arm at first probe; press j to disable; press f to re-arm.
```

**Pitfalls:**
- `auto_follow_armed` must be checked + flipped atomically inside the update path so the next `apply_update` call doesn't re-trigger.
- Don't clear follow on `Tab` (focus swap): operators want to inspect the detail pane while still following.
- The `n` key under confirm is intercepted as Abort before reaching the page handler; ensure follow clears in that branch too.

---

## Task 23 — Change-count header indicator

**Status:** pending

**Goal:** Add a `· ~N changes (n/N walk)` segment in the header strip when there is at least one `LeafState::Changed` atom, so the operator sees both the count and the binding for walking through changes.

**Why:** Operator feedback: "where are the diffs shown?" The Task 14 diff renderer is wired but invisible without first finding a Changed atom. The header indicator surfaces both the count and the `n`/`N` key.

**Files to read first:**
- `lusid/src/tui/mod.rs`: `draw_header`, `status_summary`
- `apply-stdio/src/lib.rs`: `AppView::resources`, `LeafState`

**Plan:**
1. Add `AppView::changed_count(&self) -> usize` walking the resources tree counting `LeafState::Changed`. Skip `NoChange` and other states. Unit test covers zero, one, many.
2. In `draw_header`, after the status segment, append `· ~N changes (n/N walk)` styled yellow when `changed_count() > 0`. Suppress when zero so the strip stays clean on a no-change apply.
3. The header strip already runs after the auto-focus-on-first-change behaviour is provided by Follow auto-arming (Task 22); no further coupling needed.
4. Tests: a view with one `Changed` leaf produces `~1 changes`; a view with three produces `~3 changes`; a view with zero suppresses the segment.

**Acceptance criteria:**
- Header shows `· ~N changes (n/N walk)` for `N > 0`; absent otherwise.
- `AppView::changed_count` has unit-test coverage.

**Verify:**
```
cargo test -p lusid-apply-stdio
cargo test -p lusid
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local apply
# Header strip shows the indicator after the first change arrives.
```

**Pitfalls:**
- Don't double-count branches: only count leaves whose `LeafState` is `Changed`.
- Header strip already crowds at narrow widths; consider suppressing the indicator below 80 cols if it overflows. Verify with the nginx-cluster example.
