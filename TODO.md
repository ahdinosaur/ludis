# TODO: lusid TUI redesign

Sequenced implementation brief for the TUI redesign. Each task is a self-contained unit with acceptance criteria. Land them as a sequence of commits on a single branch; the merged branch becomes the PR.

## How to work this list

You are an agent picking up the next piece of work. Follow this loop:

1. Read this file. Skip the locked-in design (§"Design") and architectural facts (§"Facts") sections if you have them in working memory from a prior turn.
2. Find the first task whose `Status:` is `pending`. That is your task.
3. Edit the task's `Status:` to `in progress` and save the file.
4. Do the work described under "Plan", reading the listed files first.
5. Verify against every acceptance criterion. Run the verification commands. If any fail, do not mark the task done — fix the regression first.
6. Commit. One commit per task, message `<task-id>: <short description>`. No `Co-Authored-By: Claude` trailer (project rule).
7. Edit the task's `Status:` to `done` with the commit hash, save, exit.

Do not skip tasks. Do not work multiple tasks at once. If a task is blocked by an issue not in its plan, add a `Blocker:` line and stop; surface it to the user.

If the design seems wrong, raise it before coding. Don't silently deviate.

## Status legend

- `pending` — not started
- `in progress` — currently being worked
- `done <hash>` — committed at the given hash
- `blocked` — surfaced an issue; user must resolve

## Design (locked-in)

**CLI verbs**
```
lusid local parse                  # parse + validate; render plan tree; no probes; no mutations
lusid local apply                  # per-epoch confirm; SSH/dev variants symmetric
lusid local apply --yes / -y       # skip all confirms
lusid local apply --no-tui         # plain-log mode even on a TTY
```
No `lusid local plan` verb. No `--yes-destructive`. No typed-yes escalation.

**TUI pages**
- `Tree` (key `1`): plan-item tree (left) + detail pane (right). Default page.
- `Epochs` (key `2`): stacked resource epochs, each showing atoms + Phase A ops + Phase B handlers in one section.
- `Stderr` (key `e`): existing stderr scrollback.

`Tab` cycles pages. During apply, both `Tree` and `Epochs` update live; a header strip and a footer prompt sit above and below.

**Palette (Terraform style + monochrome fallbacks)**
| Glyph | Color | Mono | Meaning |
|---|---|---|---|
| `▸` | gray | `[pln]` | Planned (not started) |
| `↻` | blue | `[run]` | Running (probing or applying) |
| `✓` | green dim | `[ok]` | No change |
| `~` | yellow | `[chg]` | Changed, pending apply |
| `+` | green | `[add]` | Created |
| `-` | red | `[del]` | Removed |
| `✗` | red | `[err]` | Failed |

Branch rollup states: `planned` / `running` / `ok` / `changed` / `failed`. Any child `failed` → branch `failed`. Any child `running` → branch `running`. Mix of `ok` and `changed` → branch `changed` until all children settle, then `ok`/`changed`/`failed`.

**Keymap (consistent across pages)**
- `j`/`k` / Up/Down — move selection
- `h`/`l` / Left/Right — collapse/expand
- `Space` — toggle collapse (Enter is reserved for drill-in)
- `Enter` — drill into selected atom (fullscreen detail)
- `Tab` — swap focus to detail pane (scroll inside it)
- `1`/`2` — Tree / Epochs page
- `e` — Stderr page
- `gg` / `G` — first / last
- `/` — filter by id substring (dim non-matches, don't remove)
- `n` / `N` — next / prev change (skip `ok`)
- `u` — toggle show-unchanged rows
- `s` — toggle side-by-side in diff (only at ≥140 cols)
- `?` — help overlay
- `q` / Esc — quit (or close overlay)
- `y` / Enter — accept confirm prompt
- `n` / Esc — reject confirm prompt (Esc only when prompt is visible)

**Confirm flow (per resource epoch)**
- Apply pauses after probes + change computation; emits `EpochReady`; reads one line of JSON ack on stdin before running any op.
- Footer prompt: `Epoch K/N · A atoms, H handlers · ↵ apply  n abort  d details  ? help`
- Default is reject: pressing `Esc`, `n`, or sending EOF aborts.
- `--yes` skips the read entirely (apply never waits for stdin).
- Ack is checked at epoch boundaries only. Once an epoch is running, it drains to completion; pressing `n` mid-epoch is a no-op until the next gate. Footer says so during running.

**Responsive**
- ≥100 cols — Tree page: horizontal split (50/50 tree | detail).
- <100 cols — Tree page: vertical stack (60/40 tree top / detail below).
- ≥140 cols — Detail pane supports `s` for side-by-side diff.

## Facts (architectural)

Read in this order; reference where useful in your task:

1. `apply-stdio/src/lib.rs` — wire protocol (`AppUpdate`, `AppView`, `LeafState`, `FlatViewTree`). The current TUI receives styled-text Views, not structured domain objects.
2. `lusid-apply/src/lib.rs:244-328` — the per-epoch loop with Phase A / Phase B.
3. `plan/src/tree.rs:28-49` — `PlanMeta` (id, requires, required_by, handlers). Note: not serde-derived.
4. `lusid/src/tui.rs` — current TUI, ratatui-based.
5. `lusid/src/lib.rs:298-321, 339-516, 754-897` — CLI subcommand dispatch + apply spawning (local / remote / dev).

**Constraints**:
- Probes are per-epoch (not upfront). Epoch K's diff depends on what epochs 0..K-1 did. There is no honest "show every diff before applying anything" mode.
- `Operation` and `PlanMeta` are NOT `Serialize`/`Deserialize`. New wire fields use renderable `View` shadows, not derivations on domain types.
- `lusid-cmd::CommandOutput` (in `cmd/src/lib.rs`) takes the spawned `Child` by value into a `status` future — it doesn't expose `ChildStdin`. Bidirectional protocol requires refactoring this.
- `lusid-apply` writes JSON to stdout (`emit()` at `lusid-apply/src/lib.rs:542`). stdout is reserved for that protocol; never print human text there.
- The atoms tree is shipped up-front via `ResourcesNode { index: 0, tree }` — well before per-epoch probing begins. Plan-item metadata can extend that event rather than emit a separate event.
- Even "read-only" probes shell out (`dpkg-query`, `apt-cache`, `systemctl status`, `stat`). `parse` mode avoids them; `apply` does not.

**Cross-cutting rules** (from `AGENTS.md`):
- Errors use `thiserror` + rich enums; avoid `anyhow`. Blank line between variants. `displaydoc::Display` where it fits.
- Preserve `Spanned<...>` errors. Don't drop source info.
- No `Co-Authored-By: Claude` trailer on commits.
- No comments describing the change you just made. Comments explain intent or non-obvious invariants only.
- New deps must justify themselves. The `similar` crate (Task 8) is the only new one anticipated.
- Tests accompany each task. `cargo test --workspace` and `cargo clippy --workspace -- -D warnings` and `cargo fmt --all -- --check` must all pass before committing.

---

## Task 1 — `lusid local parse` + `--parse-only` short-circuit

**Status:** pending

**Goal:** New subcommand that parses + validates a plan and renders the plan-item tree, without probing any state or applying any operation.

**Why:** Smallest unit of new functionality. Plan authors gain a quick "does my plan even type-check?" command. Unlocks epoch annotations (Task 11) and grounds the TUI's terminal-page rendering (Tasks 6-7).

**Files to read first:**
- `lusid-apply/src/lib.rs` (full)
- `lusid-apply/src/main.rs`
- `lusid/src/lib.rs:69-95` (Cmd enum), `298-321` (cmd_local_apply)
- `lusid/src/tui.rs:1-180` (top-level event loop + outcome handling)

**Plan:**
1. Add `--parse-only` flag to `lusid-apply` CLI (in `lusid-apply/src/main.rs`).
2. Thread the flag through `ApplyOptions` in `lusid-apply/src/lib.rs`.
3. In `apply()`, after `ResourcesComplete` is emitted (around line 204), run `compute_epochs` as a validation step (catches cyclic dependencies), emit `PipelineInfo` (added in Task 4 — see below), and return `Ok(())` when `parse_only` is set. If `compute_epochs` errors, return the error so apply exits non-zero with a clear message.
4. Add `LocalCmd::Parse`, `RemoteCmd::Parse { machine_id }`, `DevCmd::Parse { machine_id }` to `lusid/src/lib.rs`. Wire them to new functions `cmd_local_parse`, `cmd_remote_parse`, `cmd_dev_parse` that mirror their apply counterparts but pass `--parse-only`.
5. Set exit codes: `0` = clean, `1` = validation error (cycles, schema errors), `2` = read/IO error. Map errors in `lusid-apply/src/main.rs` accordingly.

**Important:** Task 4 introduces `PipelineInfo`. For this task, just gate the emission behind a feature so the code compiles; Task 4 will fill in the type. Practical sequence: do Task 1 first, add a `// TODO(Task 4): emit PipelineInfo` comment at the right spot, then Task 4 fills it.

**Acceptance criteria:**
- `lusid local parse` exits 0 on `examples/nginx-cluster` and shows the plan tree.
- `cargo run -p lusid-apply -- --parse-only --root <ex> --plan <file> --log info` emits `ResourceParams`, `ResourcesStart`, `ResourcesNode`, `ResourcesComplete`, and (after Task 4) `PipelineInfo`, then exits 0. No `ResourceStates*`, `ResourceChanges*`, `Operations*`, `ApplyComplete`.
- `lusid local parse` against a plan with a cyclic `requires` graph exits non-zero with the cycle error.
- No new shell subprocesses spawn during parse (validate by hand once: `strace -f -e trace=execve lusid local parse 2>&1 | grep execve` should show no apt/dpkg/systemctl/etc.).
- `host-path` validation (the existing `validate_host_paths` step) still runs and surfaces errors.

**Verify:**
```
cargo build --workspace
cargo test --workspace
cargo clippy --workspace -- -D warnings
cargo fmt --all -- --check
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local parse
```

**Pitfalls:**
- `validate_host_paths` performs `stat`/`lstat` — those are filesystem syscalls but not shell invocations. Keep them; they catch typos cheaply. Document the distinction in code comments if you add one.
- The TUI today expects a `Resources*` → `States*` → ... → `ApplyComplete` sequence. Truncating after `ResourcesComplete` (or `PipelineInfo`) without `ApplyComplete` is intentional. Verify the TUI doesn't error on that — `tui()` exits when the wait future resolves, not when ApplyComplete arrives.

---

## Task 2 — Non-TTY detection + `--no-tui` flag + plain-log fallback

**Status:** pending

**Goal:** When stdout is not a terminal, or when `--no-tui` is passed, skip ratatui and stream a human-readable plain log instead. Confirm prompts auto-resolve to `--yes` in this mode (since there's no interactive terminal).

**Why:** Required for CI. The current TUI uses `ratatui::init` which assumes a real terminal. Without this, piping to `tee log.txt` or running under `systemd` will misbehave.

**Files to read first:**
- `lusid/src/tui.rs` (the `tui()` function + the `TerminalSession` wrapper)
- `lusid/src/main.rs`
- `lusid/src/lib.rs:298-321, 339-516, 754-897`

**Plan:**
1. Add `--no-tui` boolean flag at the top-level `Cli` in `lusid/src/lib.rs`.
2. Add `is_tty_stdout()` helper using `std::io::IsTerminal` (stable in 1.70+, already available in this toolchain).
3. Refactor `lusid::tui` module so that `tui()` is the TUI-mode entry, and add a sibling `plain()` function with the same signature. `plain()` reads the JSON stream from stdout, decodes `AppUpdate`s, and prints a human digest to stderr (since the JSON itself is on stdout from the child, and we don't want to double-emit).
4. In each `cmd_*_apply` function, choose `tui` vs `plain` based on `!cli.no_tui && is_tty_stdout()`.
5. The plain-log format (rough):
   - `ResourceParams` → `parsed plan: <n> items`
   - `ResourcesNode` → `expanded to <n> atoms across <n> epochs` (once Task 4 ships PipelineInfo)
   - Per atom probe complete → one line `[probed] <atom id>: <state digest>`
   - Per change → one line `[changed] <atom id>: <change digest>`
   - Per op apply → one line per stdout/stderr line, prefixed `[op N.M] <line>`
   - `OperationApplyComplete` → `[ok] <op id>` or `[err] <op id>: <error>`
   - `ApplyComplete` → `apply complete: <n> changes`

6. Task 10's confirm flow must honor `--no-tui` and non-TTY: if not `--yes` and non-TTY, error out at startup with "interactive confirmation requires a TTY; pass --yes or use a terminal". Document this here so Task 10's plan reflects it.

**Acceptance criteria:**
- `lusid local apply | cat` (pipes stdout) produces readable plain output and does not corrupt the terminal.
- `lusid --no-tui local apply -y` on a TTY produces the plain log (skips ratatui).
- `lusid local apply` (no `-y`, non-TTY) exits non-zero with the clear "needs TTY or --yes" message. (Confirm flow itself lands in Task 10; this task can stub it with a flag check that errors.)
- Plain log preserves ordering of events and shows progress.

**Verify:**
```
cargo build --workspace
cargo test --workspace
cargo clippy --workspace -- -D warnings
echo "" | cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml --no-tui local parse
```

**Pitfalls:**
- `ratatui::init()` (`lusid/src/tui.rs:163`) MUST NOT be called when non-TTY. It will try to put the terminal into raw mode and fail or corrupt the parent shell.
- The child process (`lusid-apply`) doesn't care about TTY-ness; only the parent does. Don't pass `--no-tui` down to the child.
- Don't print plain log lines to stdout if you're piping the child's stdout through; the child uses stdout for protocol. Print plain digest to stderr or to a separate log writer.

---

## Task 3 — Wire: plan-item shadow types + extend `ResourcesNode`

**Status:** pending

**Goal:** Ship per-plan-item metadata (id, requires, required_by, on_change handler labels) over the wire as renderable `View` shadows. Receiver can navigate the plan-item structure without needing serde on domain types.

**Why:** The Tree page needs to display structured plan-item info (Task 6); the Epochs page needs handler counts and edge annotations (Task 7). Today the producer renders `PlanMeta` down to a single `View` for the branch label and drops the rest.

**Files to read first:**
- `plan/src/tree.rs` (esp. `PlanMeta` and `render_plan_tree`)
- `apply-stdio/src/lib.rs:214-300` (`AppUpdate`), `460-597` (`AppView::update`)
- `lusid-apply/src/lib.rs:170-204` (where `ResourceParams` and `ResourcesNode` are emitted)
- `operation/src/lib.rs` (just the top — see how `Operation` is shaped; do NOT derive serde on it)

**Plan:**
1. In `apply-stdio/src/lib.rs`, define:
   ```rust
   #[derive(Debug, Clone, Default, Serialize, Deserialize)]
   pub struct PlanItemWireMeta {
       pub id: Option<View>,
       pub requires: Vec<View>,
       pub required_by: Vec<View>,
       pub handlers: Vec<View>,  // one per on_change handler, rendered
   }
   ```
2. Extend `AppUpdate::ResourcesNode` to carry `plan_item_meta: std::collections::HashMap<usize, PlanItemWireMeta>`. Key is the arena index of a `Branch` slot in the shipped atoms tree. Only branches with non-empty metadata are present in the map (no entry == no metadata).
3. In `lusid-apply/src/lib.rs`, when constructing the `ResourcesNode` event around line 199, walk the `atoms_nested` (or `atoms_flat`) tree and produce the map by rendering each branch's `PlanMeta` fields with the existing `Render` impl (`view/src/render.rs`).
4. Add the new field to `AppView` and update the `update()` fold logic so `plan_item_meta` is stored alongside `ResourcesTree`. Expose via `AppView::plan_item_meta(arena_index) -> Option<&PlanItemWireMeta>`.
5. Add unit tests in `apply-stdio/src/lib.rs`: round-trip serialize/deserialize a sample event with a populated map; assert AppView correctly stores it.

**Acceptance criteria:**
- A plan with `requires`, `required_by`, and `on_change` produces a non-empty `plan_item_meta` over the wire.
- A plan with none of those produces an empty map.
- AppView returns the right meta for each branch arena index.
- No serde derive on `Operation` or `PlanMeta` themselves.
- All existing tests still pass.

**Verify:**
```
cargo test -p lusid-apply-stdio
cargo test --workspace
cargo clippy --workspace -- -D warnings
```

**Pitfalls:**
- The arena index referred to by the map key is the `FlatViewTree` index in the receiver — same indexing as `ResourcesTree.nodes` (pre-order, branches and leaves both consuming a slot). Mirror `lusid_tree::FlatTree::from`'s order in the producer.
- Don't ship the structured `Operation`s as anything other than their rendered `View`. Receivers don't execute ops.
- Empty `PlanItemWireMeta` (all fields empty) wastes wire bytes; omit from the map.

---

## Task 4 — Wire: `PipelineInfo` event + Phase grouping on `OperationsApplyEpochAdded`

**Status:** pending

**Goal:** Ship the total resource-epoch count once at startup (for the header strip) and tag every operation epoch with its resource epoch + Phase (A/B).

**Why:** The Epochs page (Task 7) needs to group ops by resource epoch and distinguish Phase A vs Phase B. The header strip needs the total epoch count. Today neither is on the wire.

**Files to read first:**
- `lusid-apply/src/lib.rs:215-328, 349-440` (per-epoch loop + `apply_op_phase`)
- `apply-stdio/src/lib.rs:214-300, 461-597`

**Plan:**
1. In `apply-stdio/src/lib.rs`, add:
   ```rust
   #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
   pub enum Phase { A, B }

   // New variant:
   AppUpdate::PipelineInfo { resource_epochs_total: usize }
   ```
   Extend `AppUpdate::OperationsApplyEpochAdded` with `resource_epoch: usize` and `phase: Phase`.
2. In `lusid-apply/src/lib.rs`, after `compute_epochs` (line 216), emit `PipelineInfo { resource_epochs_total: epochs_count }`. This must also fire under `--parse-only` from Task 1, so it lives strictly between `ResourcesComplete` and the per-epoch loop.
3. Plumb `resource_epoch_idx` (the loop variable) and a `Phase` enum value into `apply_op_phase`. Phase A call site passes `Phase::A`; Phase B call site passes `Phase::B`.
4. Inside `apply_op_phase`, attach `resource_epoch` and `phase` to every `OperationsApplyEpochAdded` event it emits.
5. Update `AppView` to store the new fields. Expose via `AppView::resource_epochs_total()` and per-op-epoch accessors.
6. Tests: AppView correctly stores PipelineInfo; OperationsApplyEpochAdded round-trips with the new fields.

**Acceptance criteria:**
- Exactly one `PipelineInfo` event per apply invocation, before any per-epoch event.
- Every `OperationsApplyEpochAdded` carries both `resource_epoch` and `phase`.
- AppView exposes both.
- `--parse-only` still emits `PipelineInfo`.
- Existing tests pass.

**Verify:**
```
cargo test -p lusid-apply
cargo test -p lusid-apply-stdio
cargo test --workspace
cargo clippy --workspace -- -D warnings
```

**Pitfalls:**
- The existing monotonic `op_epoch_counter` is preserved as-is for the per-op `index: (epoch, op)` tuple — we're adding fields, not replacing.
- Confirm the wire is backwards-incompatible. The CLI ships its own `lusid-apply` binary embedded at build time, so we don't need a compat shim.

---

## Task 5 — Redact secrets in `View` construction for file diffs

**Status:** pending

**Goal:** When a file resource's source is a secret (under the secrets directory, or `@resource/secret`), the `View` for its change/state must contain `<redacted: N bytes>` (or a content-hash digest), not the plaintext.

**Why:** Task 8 will render real file content as a unified diff in the detail pane. Without this redaction, the new TUI would leak decrypted secrets to operator terminal scrollback — a regression vs the current TUI which doesn't show diffs.

**Files to read first:**
- `secrets/src/redactor.rs`
- `lusid-apply/src/lib.rs:131-133, 386-422` (where Redactor is used today)
- `resource/src/resources/file.rs` (the file resource's `Render` impls — find them)
- `resource/src/resources/secret.rs` (the @resource/secret type)

**Plan:**
1. Audit where file content makes it into a `View`. Two main paths: (a) the file resource's `state` view (showing observed bytes — likely already a digest, but confirm), and (b) the file resource's `change` view (showing desired→observed diff). Search for `View::Paragraph` / `View::Span` carrying file body bytes.
2. Decide on representation: for files >1 KiB use `<redacted: N bytes, sha256:abcd…>`. For smaller files same format but without hash (or always hash — pick one and stick). The hash makes "did this secret change?" answerable without revealing content.
3. Thread a "is this source a secret" predicate through the `Render` path. Options:
   - **A**: Add a `RenderCtx` parameter to `Render::render` carrying the `Redactor` and the secrets dir. Invasive.
   - **B**: At construction time (when the file resource is built in `plan/src/resource.rs` or similar), mark the resource with a `secret: bool` flag based on whether its source path resolves under the secrets dir. The `Render` impl checks the flag.
   - **C**: After Views are built, walk them with the `Redactor` (which already knows the plaintexts loaded in `Secrets`) and substitute matches. Requires existing redactor to support View traversal.
   Pick **B** as the simplest sound approach: file resources get a `secret: bool` field, populated at plan time; Render uses it.
4. Make sure `@resource/secret`'s view is also redacted (its whole purpose is to deliver plaintext to a target file; the operator's terminal should never see it).
5. Tests: render a file resource with `secret: true`; assert the View contains the digest/redacted marker; assert the plaintext does not appear.

**Acceptance criteria:**
- A plan with `@resource/secret` produces a View whose serialized form does NOT contain the plaintext.
- A file resource whose source is under the project's `secrets/` directory produces a redacted View.
- A non-secret file resource produces a normal View.
- No regression: existing Redactor behavior (stdout/stderr line scrubbing) still works.

**Verify:**
```
cargo test --workspace
cargo clippy --workspace -- -D warnings
# Manual: grep through a captured apply event stream for known secret plaintexts; nothing found.
```

**Pitfalls:**
- The `Redactor`'s existing job is post-hoc string scrubbing on op stdout/stderr (`lusid-apply/src/lib.rs:386-422`). That works because op output is `String`. View payloads are structured (`Span`, `Line`, `Paragraph`, `Fragment`); walking them with the same scrubbing logic is possible but tedious. Approach (B) above avoids it by not letting the plaintext into the View in the first place.
- A file resource may NOT be marked as a secret while still receiving a secret-like value via templating. For v1 we only redact files whose *source path* points into the secrets dir — substring redaction (path-agnostic, content-driven) is a later refinement; do not attempt.

---

## Task 6 — Tree page: plan-item tree + detail pane + palette + responsive

**Status:** pending

**Goal:** New primary TUI page. Plan-item tree on the left, detail pane on the right. Status badges with the Terraform palette. Responsive layout. Replaces the existing 6-stage pipeline strip.

**Why:** This is the main UX surface the user wants. It's where you spend most of your time both before and during apply.

**Files to read first:**
- `lusid/src/tui.rs` (the whole file — you're rewriting a lot of it)
- `apply-stdio/src/lib.rs:380-779` (`ResourcesTree`, `LeafState`, projections)
- `view/src/lib.rs` and submodules (View, Span, Line, Paragraph, Fragment, Render)
- The output of Tasks 3-4 (`plan_item_meta`, `PipelineInfo`)

**Plan:**
1. Strip out the old stage strip (`PipelineStage`, `follow_pipeline`, the 6 per-stage `TreeState`s, the `draw_main_pipeline` switch). Replace with the new page enum: `UiPage::Tree`, `UiPage::Epochs`, `UiPage::Stderr`. Keep `UiPage::Stderr`.
2. Define a unified `TreePageState`:
   - Selected node (arena index in the plan-item tree)
   - Per-branch collapse state
   - Filter string (set by `/`)
   - `show_unchanged: bool` (toggled by `u`; default `false` after parse, `true` during/after apply)
3. Build the **plan-item tree projection** in `AppView` (or a new module if it gets long). The plan-item tree is the existing `ResourcesTree` but rendered with these additions:
   - Each branch with non-empty `plan_item_meta` displays its `id` view as the label; falls back to current `.` for anonymous branches.
   - Each branch carries an aggregate status (rollup from leaves below). Use the simplified 5-state rollup: `planned`/`running`/`ok`/`changed`/`failed`.
   - Atom leaves keep their existing per-leaf state but render with the Terraform palette glyph + 3-letter mono suffix.
4. Render the page:
   - Header strip (1 row, top): `lusid · <subcommand> · epoch K/N · <status summary> · <key hints>`.
   - Body: split based on terminal width.
     - ≥100 cols: left 50% tree, right 50% detail. Vertical separator.
     - <100 cols: top 60% tree, bottom 40% detail. Horizontal separator.
   - Footer (1 row): help line, OR confirm prompt (Task 10 adds the prompt; for now, just the help line).
5. Detail pane content per node type — implement the table in the design above. Each variant rendered with the matching View elements.
6. Implement the keymap. Reuse existing event-loop dispatch; route through one match per page.
7. Filter (`/`): a small text input overlay or inline at the bottom. On submit, the filter string dims non-matching rows.
8. The Terraform palette: define `Color`/`Modifier` constants in a single module (`lusid/src/tui/palette.rs` is fine).

**Acceptance criteria:**
- `lusid local parse examples/nginx-cluster/lusid.toml` shows the plan tree with correct labels, structure, and ids.
- After apply: each atom has the correct badge for its state. Branches show rolled-up state.
- Resizing the terminal across 100 cols toggles the layout cleanly.
- Detail pane updates as selection moves. Each LeafState renders the right content per the table.
- Keymap: j/k/Up/Down/h/l/Left/Right/Space/Enter/Tab/gg/G/?/q/Esc/u all behave per spec. `/` filters. `1`/`2`/`e` switch pages (Epochs page is a placeholder until Task 7).
- Old stage strip code is gone, not commented out.

**Verify:**
```
cargo build --workspace
cargo test --workspace
cargo clippy --workspace -- -D warnings
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local parse
# Manual: open the TUI, navigate, verify rendering at different widths.
```

**Pitfalls:**
- The plan-item tree is structurally the same arena as the atoms tree (one is a projection of the other); don't introduce a second tree storage layer.
- "Show unchanged off" filter must not break navigation across the visible rows (the existing `build_visible_rows` pattern is the right starting point).
- Detail pane state (selected atom, scroll position) persists across page switches.
- The header strip "epoch K/N" should read from `PipelineInfo`; show `epoch ?/?` until it arrives.

---

## Task 7 — Epochs page

**Status:** pending

**Goal:** Page 2: a stacked, scrollable view of each resource epoch. Each section shows the epoch's atoms, then its Phase A ops (merged), then its Phase B handlers.

**Why:** The user asked for an epochs page showing both resources and operations. Hierarchy reveals the Phase A→B order naturally; no mode toggle needed.

**Files to read first:**
- The Task 6 output (Tree page; you'll reuse its detail pane and badge rendering)
- `apply-stdio/src/lib.rs` (`operations_epochs`, `OperationView`, with the Task 4 `resource_epoch`/`phase` additions)
- `lusid-apply/src/lib.rs:244-440` (per-epoch loop + apply_op_phase)

**Plan:**
1. Add `UiPage::Epochs`. Add `EpochsPageState` (selected section, scroll offset).
2. Compute the layered view. For each `resource_epoch` 0..=`resource_epochs_total`-1:
   - Section header: `Epoch K/N · A atoms · X changed · H handlers`.
   - Atoms list: project the AppView's leaves filtered by causality epoch. (You'll need the wire to expose which atoms are in which resource epoch — that information is in apply but not on the wire today. **If this is missing**, also send a `ResourcesNode` extension `atom_epoch: HashMap<usize, usize>` (atom-index → resource-epoch). If you add this, document in this task plan and update the verify steps accordingly. Otherwise reconstruct by reading the per-atom `Probing` event order — atoms in the same epoch probe together.)
   - Phase A subsection: ops from `operations_epochs[k]` where `phase == A && resource_epoch == K`, in order.
   - Phase B subsection: same for `phase == B`. If empty, show `(no on_change handlers fired)`.
3. Per-atom row: badge + label + (if applicable) `← requires: <id> (epoch J)` annotation derived from `plan_item_meta.requires` resolving to atoms with `resource_epoch == J`.
4. Detail pane behavior on this page: same as Tree page — selection drives the right pane.
5. Same keymap as Tree page. `Space` collapses an Epoch section (default expanded).

**Acceptance criteria:**
- For a 3-epoch plan (e.g. nginx-cluster: install nginx → write config → reload), each epoch renders as a section with the correct atoms.
- Phase B handlers show up under the epoch where the `on_change` was registered.
- A `requires` edge crossing epochs is visible as the annotation.
- Selecting an atom updates the detail pane the same as on the Tree page.
- Tab cycles back to Tree page; numbered keys jump.

**Verify:**
```
cargo test --workspace
cargo clippy --workspace -- -D warnings
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local parse
# Press 2 to switch to Epochs page; verify structure.
```

**Pitfalls:**
- If you have to add `atom_epoch` to `ResourcesNode`, do so cleanly — `HashMap<usize, usize>` keyed by atom arena index, value is the resource epoch index. Compute it in `lusid-apply/src/lib.rs` between `compute_epochs` (line 216) and the per-epoch loop. Send before the first per-epoch event.
- An atom with no `requires` may end up in epoch 0 alongside many siblings — don't try to show edges for that case; it's noise.
- Phase B's ops are dynamically determined per-apply (only fire if the bearing branch changed). Pre-apply (parse mode), they show as "would fire if change occurred" or simply don't render them at all in parse mode. Pick the simpler: parse mode shows the structural shape (handlers declared) but states "fires only on change".

---

## Task 8 — Diff renderer in detail pane

**Status:** pending

**Goal:** When the detail pane shows a `Changed` atom, render the diff well. Scalars get a `before | after` table. File content gets a unified diff (with optional side-by-side at ≥140 cols).

**Why:** The whole point of showing the detail pane is to make the change scannable. Bare Views are not scannable enough.

**Files to read first:**
- Task 5's output (where secrets are redacted; this task renders the same Views but more nicely)
- The `View` types in `view/src/view/*`
- `Cargo.toml` (you'll add `similar`)

**Plan:**
1. Add `similar = { version = "2", default-features = false, features = ["text"] }` to `view/Cargo.toml` (or `lusid/Cargo.toml` if confining to the TUI).
2. For each `ResourceState`/`ResourceChange` shape (file, package, user, group, systemd, ...): determine which fields are scalar vs which carry file content. Use the existing View structure to drive the renderer:
   - A `View::Paragraph` is multi-line content → render as unified diff if it appears in a `change` (i.e., paired with an old version).
   - A `View::Line` or `View::Span` is scalar → render as a row in a `before | after` table.
3. Build a `DiffPane` widget in the TUI that takes a `LeafState::Changed { state, change, ... }` and renders accordingly.
4. Side-by-side toggle (`s`): only effective when terminal width ≥140 cols. When toggled, render two parallel paragraphs (left=before, right=after) with paired line numbers.
5. Binary content guard: if any byte in the Paragraph isn't valid UTF-8 (or contains `\0`), show `<binary content, N bytes>` instead.
6. Always prefix the diff with the atom's path (plan-item id `/` atom id).

**Acceptance criteria:**
- A `@resource/file` change on `nginx.conf` renders as a readable unified diff with green/red colors.
- A `@resource/file` mode change (e.g. 0644 → 0600) renders as a row in a scalar table.
- A `@resource/secret` (redacted by Task 5) renders as `<redacted: N bytes, sha256:...>` for both sides, with no plaintext leak.
- `s` toggles side-by-side; only works at ≥140 cols.

**Verify:**
```
cargo test --workspace
cargo clippy --workspace -- -D warnings
# Manual: run apply on nginx-cluster with a config change; check the detail pane.
```

**Pitfalls:**
- `similar` has multiple algorithms; the default `myers` is fine for our case.
- Unified diff defaults to 3 lines of context; small files may have less. Don't show "no diff" for "same content".
- Side-by-side at <140 cols is jarring; refuse the toggle and show a brief hint at the bottom of the detail pane.

---

## Task 9 — `lusid-cmd` refactor: expose `ChildStdin` from `CommandOutput`

**Status:** pending

**Goal:** Make the spawned-child's stdin accessible from the caller, so the parent CLI can write JSON acks to `lusid-apply`. Pure plumbing task — no behavior change.

**Why:** Required by Task 10 (per-epoch confirm). Today `lusid_cmd::CommandOutput` consumes the child into a future and stdin is lost.

**Files to read first:**
- `cmd/src/lib.rs` (the whole module, especially `Command`, `CommandOutput`, `spawn`, `output`)
- `lusid/src/lib.rs:298-321, 339-516, 754-897` (call sites)
- `lusid/src/tui.rs:81-100` (the `tui()` signature — Wait/Stdout/Stderr generics)
- `ssh/src/command.rs:27-71` (the SSH analog already exposes `stdin()`; mirror its shape)

**Plan:**
1. Modify `lusid_cmd::CommandOutput` so it owns three streams + the wait future:
   ```rust
   pub struct CommandOutput {
       pub stdout: ChildStdout,
       pub stderr: ChildStderr,
       pub stdin: ChildStdin,
       pub status: BoxFuture<'static, Result<(), CommandError>>,
   }
   ```
   (Adjust types to match what's actually used; the shape is: don't move the Child into the future before taking stdin.)
2. Update `Command::output()` to take stdin handle before constructing the status future.
3. Update every call site in `lusid/src/lib.rs` (local apply destructures `output.stdout, .stderr`; add `.stdin` to the destructure even if unused for now).
4. Update the `tui()` function signature in `lusid/src/tui.rs` to accept a `stdin: impl AsyncWrite + Unpin` parameter. For now, the function ignores it; Task 10 wires it.
5. Pass `output.stdin` (local) and `handle.channel.stdin()` (SSH) at each call site.

**Acceptance criteria:**
- `cargo build` passes.
- Local apply still works end-to-end (no functional regression).
- Remote / dev apply still works.
- `tui()` accepts (and ignores) a `stdin` argument.

**Verify:**
```
cargo build --workspace
cargo test --workspace
cargo clippy --workspace -- -D warnings
# Manual: run local apply on an example plan; confirm it still completes.
```

**Pitfalls:**
- `tokio::process::Child` lets you take stdin/stdout/stderr exactly once each. Order matters: take all three handles before moving the Child into a wait future.
- If a call site discards stdin (intentionally), it must drop the handle so the child sees EOF and doesn't block.
- The SSH command handle's `stdin()` returns `Option<&mut ChannelWriteHalf<...>>` or similar — adapt to whatever existing type it returns; an enum or trait object around `AsyncWrite + Unpin` is fine if needed.

---

## Task 10 — Per-epoch confirm: `EpochReady` event + ack channel + `--yes` flag + footer prompt

**Status:** pending

**Goal:** Apply pauses between resource epochs (after probes and op planning, before any op runs). TUI shows a footer prompt; user `y`/Enter sends ack JSON, `n`/Esc aborts. `--yes` skips all prompts.

**Why:** The user's "confirm before changes apply" requirement, ergonomically integrated with per-epoch probing.

**Files to read first:**
- The output of Tasks 3, 4, 6, 9
- `lusid-apply/src/lib.rs:244-328`
- `apply-stdio/src/lib.rs:214-300, 461-597`
- `lusid/src/tui.rs` (post-Task 6 shape)

**Plan:**
1. Wire format additions in `apply-stdio/src/lib.rs`:
   ```rust
   #[derive(Debug, Clone, Serialize, Deserialize)]
   pub struct EpochSummary {
       pub atoms_total: usize,
       pub atoms_changed: usize,
       pub handlers_pending: usize,
       // Plus a short labeled list of the atoms about to be changed; cap at, say, 8 entries:
       pub change_labels: Vec<View>,  // truncated; "+ 3 more" handled in TUI
       pub truncated_count: usize,
   }

   AppUpdate::EpochReady { resource_epoch: usize, summary: EpochSummary }
   ```
2. Reverse-direction wire: define a small enum `enum AckAction { Apply, Abort }` and serialize as `{"action": "apply"}` or `{"action": "abort"}`, one per `EpochReady`.
3. `lusid-apply` side:
   - Add `--yes` / `-y` flag.
   - In the per-epoch loop, after Phase A probes complete and change ops are computed but before they apply, emit `EpochReady { resource_epoch: K, summary }`.
   - If `--yes`: skip stdin read; treat as `Apply`.
   - Else: read one line from `tokio::io::stdin()`, parse as `AckAction`. EOF or parse error treated as `Abort`.
   - On `Abort`: do not run ops for this or later epochs. Emit `ApplyComplete { had_changes: ... }` reflecting changes already applied. Exit non-zero with code 130 (SIGINT-equivalent; standard "user-canceled").
4. `lusid` CLI side:
   - Add `--yes` / `-y` flag on `local apply`, `remote apply --machine X`, `dev apply --machine X`. Plumb to `lusid-apply --yes`.
   - Update `tui()` to wire the `stdin` from Task 9. When `EpochReady` arrives without `--yes`, show the footer prompt. On `y` / Enter: write `{"action":"apply"}\n`. On `n` / Esc: write `{"action":"abort"}\n`.
   - Footer states:
     - Idle: `↵ apply  n abort  d details  ? help`
     - Running (between gate-pass and next gate): `running epoch K — abort takes effect after this epoch`. n still writes abort, but won't cancel mid-epoch.
   - Non-TTY without `--yes`: exit at startup with the error from Task 2.
5. Cancellation correctness: ack is only read at epoch boundaries. Mid-epoch, in-flight probes/ops drain. Do not introduce `select!` cancellation around `try_join_all` — leaves shell subprocess leak risk.
6. Tests:
   - Unit (AppView): `EpochReady` recorded; `AckAction` enum round-trips.
   - Integration: a small synthetic plan; pipe predetermined acks via stdin; assert behavior.
   - Integration: `--yes` skips reads, runs through.

**Acceptance criteria:**
- `lusid local apply` on nginx-cluster pauses before each resource epoch; prompts work; full apply completes after passing each gate.
- `lusid local apply -y` runs straight through, no prompts.
- Abort on epoch 2 of 4 exits non-zero with "Aborted at epoch 2. Epochs 0..1 have been applied. Re-run to retry." printed.
- Non-TTY + no `--yes` errors at startup, doesn't hang.
- Remote / dev apply also gate correctly (test by manual run if practical, or by unit-testing the ack-write side via mocked AsyncWrite).

**Verify:**
```
cargo test --workspace
cargo clippy --workspace -- -D warnings
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local apply  # interactive
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local apply -y  # no prompts
```

**Pitfalls:**
- Don't ack from the same future that reads stdout. Use a separate task that listens for key events on the prompt and writes to stdin.
- Don't read stdin in `--yes` mode at all — if stdin happens to close mid-apply (parent dies), don't treat it as abort signal we shouldn't be receiving.
- `EpochReady` is emitted in the producer only after probes succeed. If probes fail, error propagates and apply exits before EpochReady — TUI never shows a prompt for a doomed epoch. Don't try to recover.
- Empty epochs (resource epoch with no atoms changing, no handlers firing) — still emit `EpochReady` so the user sees the gate. They can press `y` to continue, or `--yes` skips them automatically. Or: skip emission for empty epochs to reduce prompt fatigue. Pick "skip empty epochs" — less rubber-stamping risk.

---

## Task 11 — Polish: `n`/`N` jump-to-change + epoch tags in parse output

**Status:** pending

**Goal:** Two small quality-of-life additions.

**Why:** Lets users find changes in a big plan quickly (`n`/`N`) and see "when does this run" without flipping to the Epochs page (epoch tag in Tree).

**Files to read first:**
- The Task 6 output (Tree page state + navigation)
- The Task 4 output (`atom_epoch` wire data, if added in Task 7)

**Plan:**
1. On the Tree page, bind `n` to "select next atom whose rollup state is `changed`, `failed`, or anything that contains a change". `N` is the reverse. Skip `ok` rows and `planned` rows.
2. In the plan-item tree, when rendering each branch (or atom), append `(epoch K)` next to its id. Use the wire's epoch mapping (Task 4's `atom_epoch` if it was added in Task 7, otherwise compute from `OperationsApplyEpochAdded`).
3. Test:
   - Unit: jump-to-change moves through expected atoms.
   - Unit: epoch tag matches `compute_epochs` output.

**Acceptance criteria:**
- `n`/`N` cycles through changed atoms in DFS order.
- Plan tree rows show `(epoch K)` after the label.
- No-op if there are no changes.

**Verify:**
```
cargo test --workspace
cargo clippy --workspace -- -D warnings
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local parse
```

**Pitfalls:**
- If the cursor is past the last changed atom, `n` wraps to first or no-ops — pick "no-op with a brief footer toast 'no more changes'" rather than wrapping silently.
- The epoch tag is informational; don't let it widen the tree column to the point where labels truncate at small widths. Show it only at ≥80 cols.

---

## When all tasks are done

1. Run the full verification battery one more time:
   ```
   cargo test --workspace
   cargo clippy --workspace -- -D warnings
   cargo fmt --all -- --check
   ```
2. Run all examples end-to-end:
   ```
   cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local parse
   cargo run -p lusid -- --config examples/arch-desktop/lusid.toml local parse
   cargo run -p lusid -- --config examples/dotfiles/lusid.toml local parse
   ```
3. Manually exercise the TUI: navigate, confirm, abort, resize the terminal across the responsive breakpoint, view a secret diff, view a file diff, hit `?` for help on each page.
4. Update `apply-stdio/README.md` to reflect the new wire format (`PipelineInfo`, `EpochReady`, the extended `ResourcesNode` and `OperationsApplyEpochAdded`).
5. Update `README.md`'s "Apply a plan" section to mention the `parse` verb and the `-y` flag.
6. Surface the branch to the user for final review and PR creation.
