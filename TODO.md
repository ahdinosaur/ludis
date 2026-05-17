# TODO: lusid TUI redesign

Sequenced implementation brief. Two phases:

- **Phase 0 (Tasks 1-6)**: replace `lusid-view` with a structured wire + a separate renderer. ~3 weeks (Task 5 is the bottleneck at ~1 week even split into 5a/5b/5c). Foundational; everything else builds on it.
- **Phase 1 (Tasks 7-17)**: the TUI redesign itself. ~5 weeks.

Land as a sequence of commits on a single branch. The branch becomes the PR. Total estimate: ~7-8 weeks of focused work. If review fatigue becomes real, split at the Phase 0 boundary into two PRs.

## How to work this list

You are an agent picking up the next piece of work. Follow this loop:

1. Read this file. Skip the locked-in design (§"Design") and architectural facts (§"Facts") if you have them in working memory from a prior turn.
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

**Wire format direction**: structured serde-derived domain types over the wire. The receiver renders to text via a separate `lusid-render` crate. The producer's own CLI uses the same renderer for its display, mirroring Terraform's `jsonformat` and Pulumi's `pkg/backend/display`. No View layer; no Render trait in domain crates. The `view/` crate is deleted at the end of Phase 0.

Optional `rendered: String` field allowed only on event variants where the producer holds knowledge the consumer cannot reconstruct: plan-source diagnostics (Rimu `Spanned` errors rendered against the original plan file) and pre-styled subprocess output. Everything else is pure structured.

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

**Palette (Terraform-style + monochrome fallbacks)**
| Glyph | Color | Mono | Meaning |
|---|---|---|---|
| `▸` | gray | `[pln]` | Planned (not started) |
| `↻` | blue | `[run]` | Running (probing or applying) |
| `✓` | green dim | `[ok]` | No change |
| `~` | yellow | `[chg]` | Changed, pending apply |
| `+` | green | `[add]` | Created |
| `-` | red | `[del]` | Removed |
| `✗` | red | `[err]` | Failed |

Branch rollup states: `planned` / `running` / `ok` / `changed` / `failed`. Any child `failed` → branch `failed`. Any child `running` → branch `running`. Mix of `ok` and `changed` → branch `changed` until all children settle.

**Keymap (consistent across pages)**
- `j`/`k` / Up/Down — move selection
- `h`/`l` / Left/Right — collapse/expand
- `Space` — toggle collapse (Enter is reserved)
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
- Ack is checked at epoch boundaries only. Once an epoch is running, it drains to completion.

**Responsive**
- ≥100 cols — Tree page: horizontal split (50/50 tree | detail).
- <100 cols — Tree page: vertical stack (60/40 tree top / detail below).
- ≥140 cols — Detail pane supports `s` for side-by-side diff.

## Facts (architectural)

Read these in order on first invocation; reference where useful:

1. `apply-stdio/src/lib.rs` — wire protocol (`AppUpdate`, `AppView`, `LeafState`). After Phase 0, all variants ship structured serde-derived types.
2. `lusid-apply/src/lib.rs:244-328` — per-epoch loop with Phase A / Phase B.
3. `plan/src/tree.rs:28-49` — `PlanMeta` (id, requires, required_by, handlers).
4. `operation/src/lib.rs` and `operation/src/operations/*.rs` — `Operation` enum and per-family types. **All pure data** (no executor handles inside `Operation` values); runtime handles live on the `OperationType` trait's associated types and are constructed by `apply()` on demand.
5. `resource/src/lib.rs` and `resource/src/resources/*.rs` — `ResourceParams` / `Resource` / `ResourceState` / `ResourceChange`. Pure data.
6. `lusid/src/tui.rs` — current TUI, ratatui-based.
7. `lusid/src/lib.rs:298-321, 339-516, 754-897` — CLI subcommand dispatch + apply spawning.

**Constraints (post-Phase 0)**:
- Wire ships structured serde types. No `View` on the wire. The receiver calls `lusid-render` to produce styled text from structured payloads.
- Probes are per-epoch (not upfront). Epoch K's diff depends on what epochs 0..K-1 did.
- `lusid-cmd::CommandOutput` (`cmd/src/lib.rs`) takes the spawned `Child` by value into a `status` future — doesn't expose `ChildStdin`. Bidirectional protocol requires refactoring (Task 15).
- `lusid-apply` writes JSON to stdout (`emit()` at `lusid-apply/src/lib.rs:542`). stdout is reserved for that protocol; never print human text there.
- The atoms tree ships up-front via `ResourcesNode { index: 0, ... }` — well before per-epoch probing begins.
- Even "read-only" probes shell out (`dpkg-query`, `apt-cache`, `systemctl status`, `stat`). `parse` mode avoids them; `apply` does not.

**Cross-cutting rules** (from `AGENTS.md`):
- Errors use `thiserror` + rich enums; avoid `anyhow`. Blank line between variants. `displaydoc::Display` where it fits.
- Preserve `Spanned<...>` errors. Don't drop source info.
- No `Co-Authored-By: Claude` trailer on commits.
- No comments describing the change you just made. Comments explain intent or non-obvious invariants only.
- New deps must justify themselves. `similar` (Task 14) is the only new one anticipated.
- Tests accompany each task. `cargo test --workspace`, `cargo clippy --workspace -- -D warnings`, `cargo fmt --all -- --check` must all pass before committing.

---

# Phase 0: lusid-view migration

The View layer ships styled text the TUI never reads (the comment in `view/src/lib.rs:20-23` already admits the style fields are speculative overhead). Every comparable tool in this neighborhood — cargo, Terraform, kubectl, Nix, Bazel, LSP, OpenTelemetry, Pulumi — ships structured data and renders at the consumer. Phase 0 aligns lusid with that pattern: serde-derive the domain, render at the consumer through a dedicated `lusid-render` crate, delete `view/`.

---

## Task 1 — Drop style metadata from View

**Status:** done 4f60b71

**Goal:** Remove `TextStyle`, `Color`, `Alignment`, `Modifier` fields from `Span` / `Line` / `Paragraph`. View becomes a stripped-down text container.

**Why:** Audit confirmed the TUI never reads these fields (it applies ratatui styles from semantic context). Removing them shrinks the wire by an estimated ~30%, removes ~150 LoC, and clears the path for Phase 0 tasks 2-6.

**Files to read first:**
- `view/src/view/span.rs`, `line.rs`, `paragraph.rs`, `fragment.rs`, `text.rs`
- `view/src/view/mod.rs`
- `view/src/lib.rs` (just to confirm no other exports use the style types)

**Plan:**
1. Delete `view/src/view/text.rs` (`TextStyle`, `Color`, `Alignment` enums).
2. From `Span` / `Line` / `Paragraph` types, remove `style: TextStyle`, `alignment: Alignment` fields. Update constructors accordingly. `Span` collapses to `{ text: String }`; `Line` to `{ spans: Vec<Span> }`; `Paragraph` to `{ lines: Vec<Line> }`. `Fragment` is unchanged.
3. Remove the `text` module from `view/src/view/mod.rs`. Remove style/alignment exports from `view/src/lib.rs`.
4. Update every constructor call site in the workspace. Most are `Span::new(text)` / `Line::new(spans)` / `Paragraph::new(lines)`; the style parameters were almost always defaults.
5. Verify Display impls produce identical output.

**Acceptance criteria:**
- `cargo build --workspace` passes.
- `cargo test --workspace` passes.
- No reference to `TextStyle`, `Color`, `Alignment`, `Modifier` in the workspace (after this task's deletion).
- Serialized JSON for a Span no longer contains `style: {...}`.

**Verify:**
```
cargo build --workspace
cargo test --workspace
cargo clippy --workspace -- -D warnings
# Scope the grep to non-TUI crates; ratatui's own Color/Modifier/Style are unrelated and stay.
grep -rn "lusid_view::TextStyle\|lusid_view::Color\|lusid_view::Alignment" --include="*.rs" .  # should return nothing
grep -rn "view::TextStyle\|view::Alignment" --include="*.rs" view/ operation/ resource/ plan/ apply-stdio/ lusid-apply/  # should return nothing
```

**Pitfalls:**
- `Color` (lusid's enum) is different from `ratatui::style::Color` — the latter stays. Only remove the lusid one.
- Some constructors take builder-style style arguments today. Drop them from the signature.
- If a Render impl happens to set a style (audit says none do, but verify), drop the call.
- The TUI (`lusid/src/tui.rs`) imports `ratatui::style::{Color, Modifier, Style}` — leave those alone. A naive `grep "Color"` will hit hundreds of TUI sites; that's expected, not a problem.

---

## Task 2 — Serde-derive the `Operation` tree

**Status:** done a594409

**Goal:** Add `Serialize` + `Deserialize` to `Operation` and every `*Operation` / `*Executor` / `*Source` / `*Path` type underneath it. After this task, an `Operation` value round-trips through JSON cleanly.

**Why:** Phase 0 needs to ship structured operation data on the wire. `Operation` is already pure data (`CommandExecutor = enum { Direct, Shell }`; runtime handles live on the trait, not in values), so the derives are mechanical.

**Files to read first:**
- `operation/src/lib.rs` (Operation enum)
- `operation/src/operations/*.rs` (each family)
- `operation/Cargo.toml`

**Plan:**
1. Add `serde` to `operation/Cargo.toml` if not already there: `serde = { version = "1", features = ["derive"] }`.
2. Add `#[derive(Serialize, Deserialize)]` to `Operation` (`operation/src/lib.rs:74`) and every per-family type in `operation/src/operations/*.rs`. The 13 families are: `AptOperation`, `AptRepoOperation`, `AurOperation`, `PacmanOperation`, `PodmanOperation`, `FlatpakOperation` (its inner `AddRemote`/`ModifyRemote`/`RemoveRemote` variants too), `FileOperation`, `FileSource`, `FilePath`, `DirectoryOperation`, `CommandOperation`, `CommandExecutor`, `GitOperation`, `SystemdOperation`, `UserOperation`, `GroupOperation`. Plus any helper types referenced (enums, structs inside those modules).
3. If a field references a type from another crate (e.g. `PathBuf`, `HashSet<String>`, `Spanned<X>`), confirm it serde-derives. `PathBuf`, `HashSet`, `BTreeMap`, etc. are fine. `Spanned<...>` from `rimu` is not serde-derived in v1 of this PR — Task 3 introduces a `WireSpanned<X>` shadow as the stop-gap, with a clean future migration path to upstream Rimu serde derives.
4. Add a round-trip test in each operation file: build a representative `*Operation`, serialize to JSON, deserialize, assert equality.

**Acceptance criteria:**
- Every variant in `Operation` serializes cleanly.
- Round-trip tests pass for all 13 families: Apt, AptRepo, Aur, Pacman, Podman, Flatpak, File, Directory, Command, Git, Systemd, User, Group.
- No new `unsafe`, no behavior change.

**Verify:**
```
cargo build --workspace
cargo test -p lusid-operation
cargo test --workspace
cargo clippy --workspace -- -D warnings
```

**Pitfalls:**
- `CommandExecutor` has a custom `FromStr` for plan-parsing; the serde representation should match the human-readable form (`"direct"` / `"shell"`) for consistency. Use `#[serde(rename_all = "lowercase")]` on the enum.
- If a per-family type holds `Spanned<Value>` (Rimu value), that may not serde cleanly. Audit; if the field is only used at parse time, consider `#[serde(skip)]` with a sensible Default — but only if the field isn't needed downstream.

---

## Task 3 — Serde-derive `ResourceParams` / `Resource` / `ResourceState` / `ResourceChange`

**Status:** done 21c925d

**Goal:** Add `Serialize` + `Deserialize` to the resource-side type trees. After this task, the full domain of "what we're applying" round-trips through JSON.

**Why:** Same as Task 2. After this, every domain type the wire would want to carry can carry itself.

**Files to read first:**
- `resource/src/lib.rs` (the four top-level enums)
- `resource/src/resources/*.rs` (every family)
- `rimu::Spanned`, `rimu::Span`, `rimu::SourceId` — confirm serde shape

**Plan:**
1. Add `serde` derive feature in `resource/Cargo.toml`.
2. Define `WireSpanned<T>` in `apply-stdio` (or a small helper crate `wire-types/` if `apply-stdio` ends up with too many helpers):
   ```rust
   #[derive(Debug, Clone, Serialize, Deserialize)]
   pub struct WireSpanned<T> {
       pub value: T,
       pub source_file: Option<String>,  // resolved from rimu SourceId by the producer
       pub span_start: usize,
       pub span_end: usize,
   }

   impl<T> WireSpanned<T> {
       pub fn from_spanned(spanned: rimu::Spanned<T>, ctx: &SourceFileLookup) -> Self { ... }
   }
   ```
   The producer (in `lusid-apply`) does the SourceId-to-path lookup before emission. The consumer has the file path as a string.
3. **Stop-gap rationale + migration path.** Rimu is owned by this project's author and can be modified, but `Spanned` / `Span` / `SourceId` are not serde-derived today. Adding the derives upstream couples this PR to a Rimu release cycle. The stop-gap is `WireSpanned<T>` — pure shadow type, no upstream changes. Migration path for a future PR: add `#[derive(Serialize, Deserialize)]` to `rimu::Spanned<T>` and `rimu::Span` upstream (SourceId may need a wire-friendly representation — string interning vs raw `usize` decision lives there), release a new Rimu, then replace `WireSpanned<T>` usages with `Spanned<T>` directly. The stop-gap and migration are independent: each Task-3 site that uses `WireSpanned` is one mechanical search-and-replace later.
4. Audit `Spanned<...>` usage in resource types. Two cases:
   - **Spans needed downstream** (plan-source error rendering): replace `Spanned<T>` field with `WireSpanned<T>`.
   - **Validation-time-only spans** (e.g. `FileParams::Sourced::source_span` used only by `HostPathValidationError` before apply runs): `#[serde(skip, default)]` is fine here, with a one-line comment explaining the field is not preserved across the wire.
5. Add `#[derive(Serialize, Deserialize)]` to `ResourceParams`, `Resource`, `ResourceState`, `ResourceChange`, plus every per-family variant.
6. Round-trip tests per family.

**Acceptance criteria:**
- Every domain type serializes cleanly.
- Per-family round-trip tests pass.
- `Spanned<...>` fields preserved via `WireSpanned<...>` (or skipped with explicit rationale per field).

**Verify:**
```
cargo build --workspace
cargo test -p lusid-resource
cargo test --workspace
cargo clippy --workspace -- -D warnings
```

**Pitfalls:**
- `FileChange::Sourced { content: Vec<u8> }` and similar — these may carry large byte arrays. Serde-derivation works but pay attention to base64 vs binary array encoding. Pick base64 (`serde_with::base64`) to avoid wasting JSON bytes on `[1,2,3,...]` arrays.
- Some resource params types reference rimu Value at the boundary (e.g. for typed params not yet validated). Those typically don't appear in the final `ResourceParams` (validation already consumed them), but double-check.
- `rimu::Spanned` and `rimu::Span` are owned by this project's author but are not serde-derived today. Use `WireSpanned<T>` as the stop-gap for this PR; a follow-up Rimu release can add serde derives, after which usages can be migrated 1-for-1 (no urgency).

---

## Task 4 — Create `lusid-render` crate

**Status:** done 87f4a5e

**Goal:** New crate that takes structured domain types (output of Tasks 2-3) and produces text + semantic tags suitable for both ratatui rendering and plain-log mode. Lifts the display logic out of `lusid-view::Render` impls.

**Why:** Separates "what data" from "how to display". TUI uses `lusid-render`'s output to build ratatui widgets; plain-log mode uses the same to write newline-delimited human text. Future consumers (Slack, web) can either consume the structured wire or call into this crate if in-process.

**Files to read first:**
- All `impl_display_render!` macro invocations in the workspace (search: `grep -r "impl_display_render" --include="*.rs" .`)
- The hand-written Render impls: `resource/src/lib.rs`, `operation/src/lib.rs`, `apply-stdio/src/lib.rs:64-74`
- Domain `Display` impls in `resource/src/resources/*.rs` and `operation/src/operations/*.rs`

**Plan:**
1. Create `lusid-render/` crate with `Cargo.toml` and `src/lib.rs`.
2. Define a `RenderedNode` shape that captures "text + semantic role":
   ```rust
   pub enum RenderedNode {
       Plain(String),
       Tagged { tag: SemanticTag, content: Vec<RenderedNode> },
       Tree(Vec<RenderedNode>),  // children, rendered as indented tree
   }

   pub enum SemanticTag {
       Added,
       Removed,
       Modified,
       Unchanged,
       Error,
   }
   ```
   Keep `SemanticTag` minimal. Add more only when the TUI palette genuinely needs them — `Identifier`, `Warning`, `BinaryPlaceholder`, `Redacted` etc. can be expressed as `Plain(...)` with the relevant text. The palette in `lusid/src/tui/palette.rs` maps each tag to a ratatui `Style`.
3. Add `Render` trait *inside this crate* (not in `view/`):
   ```rust
   pub trait Render {
       fn render(&self) -> RenderedNode;
   }
   ```
4. Provide a `display_render!` macro inside `lusid-render` that produces `impl Render for X { fn render(&self) -> RenderedNode { RenderedNode::Plain(self.to_string()) } }`. **Most domain types (~95%) just delegate to `Display`; use the macro at each call site.** Only hand-write `Render` impls where today's `Display` is not the desired text or where structured tagging adds value (diffs, multi-line content).
5. Place per-domain `Render` impls in `lusid-render/src/`, mirroring the source layout: `lusid-render/src/resources.rs`, `lusid-render/src/operations.rs`, `lusid-render/src/plan_id.rs`. One file per domain crate.
6. Add helpers: `RenderedNode::to_plain_string()` (for plain-log mode), `RenderedNode::to_ratatui_text(palette: &Palette) -> Text<'static>` (for the TUI; palette maps `SemanticTag` to `ratatui::Style`).
7. Don't touch `view/` yet — Task 5 migrates AppUpdate variants and Task 6 deletes `view/`.

**Acceptance criteria:**
- `lusid-render` builds standalone.
- Every domain type has a `Render` impl in `lusid-render` producing the same text as today's `Display`.
- `to_plain_string` matches `Display` output for sample inputs.
- `to_ratatui_text` produces a `ratatui::text::Text` with appropriate styles.

**Verify:**
```
cargo build -p lusid-render
cargo test -p lusid-render
cargo test --workspace
cargo clippy --workspace -- -D warnings
```

**Pitfalls:**
- Keep `SemanticTag` small. Don't try to express "every possible style" — just the categories the TUI palette needs (matches the Design palette above).
- `lusid-render` depends on `resource` and `operation` crates. Cycle check: those crates must NOT depend on `lusid-render`. They keep only their `Display` impls.
- This is a moving-code task, not a logic-change task. If you find yourself rewriting how something looks, stop and confirm with the user.

---

## Task 5 — Migrate `AppUpdate` variants to structured payloads

**Status:** done (5a: 3e2d120; 5b: 33fc4e7; 5c: e1603ae)

**Goal:** Replace `View` / `ViewTree` fields in every `AppUpdate` variant with structured serde types from Tasks 2-3. The TUI and plain-log mode use `lusid-render` to produce display text.

**Why:** The wire change. After this, the protocol is fully structured.

**Scope warning:** This is the biggest task in Phase 0. It rewrites `AppUpdate`, `AppView`, `LeafState`, all four projections, the ops-splice algorithm in `apply-stdio`, and every consumer in `tui.rs:648-1192`. Expected diff size: ~2000 LoC. **Split into three commits within this task slot**:

- **5a**: Producer-side migration. `lusid-apply` no longer renders to `View` before `emit()`; ships structured types directly. Add a temporary `to_view()` shim on the consumer side so `AppView` projections keep compiling and returning `FlatViewTree` for now. Verify: wire payloads are now structured; TUI still works against the shim.
- **5b**: Replace `LeafState`'s `View` fields with structured types (`resource: Resource`, `state: ResourceState`, `change: ResourceChange`, `ops: Option<(PlanTree<Operation>, u64)>`). Update `transition_leaf` accordingly. Drop the `to_view()` shim. Projections still exist but return projected structured trees.
- **5c**: Rewrite `tui.rs` consumption to read structured projections and call `lusid-render` for display. Drop any remaining `View`/`Display`-based rendering on the TUI side.

**Files to read first:**
- `apply-stdio/src/lib.rs` (full — every `AppUpdate` variant and the `AppView` fold)
- `lusid-apply/src/lib.rs` (every `emit()` call site)
- `lusid/src/tui.rs` (every consumer of the AppView's tree projections, especially the four `draw_main_pipeline` branches and the `tree_and_state_for_stage` plumbing)

**Plan:**
1. (5a) For each `AppUpdate` variant that carries a `View` or `ViewTree`, change the field type:
   - `ResourceParams { resource_params: ViewTree }` → `ResourceParams { resource_params: PlanTree<ResourceParams> }`.
   - `ResourcesNode { tree: ViewTree }` → `ResourcesNode { tree: PlanTree<Resource> }`.
   - `ResourceStatesNodeComplete { node: View }` → `ResourceStatesNodeComplete { state: ResourceState }`.
   - `ResourceChangesNode { node: Option<View> }` → `ResourceChangesNode { change: Option<ResourceChange> }`.
   - `OperationsNode { operations: ViewTree }` → `OperationsNode { operations: PlanTree<Operation> }`.
   - `OperationsApplyEpochAdded { operations: Vec<View> }` → `OperationsApplyEpochAdded { operations: Vec<Operation> }`.
   - `OperationApplyStdout/Stderr` ship `String`; unchanged.
2. (5a) `lusid-apply/src/lib.rs` `emit()` call sites no longer call `.render()` on domain values; pass values directly. Drop `render_plan_tree` calls.
3. (5b) `AppView` stores the structured types. Update `LeafState` to carry `ResourceState` / `ResourceChange` / `PlanTree<Operation>` directly.
4. (5b) The four projection methods (`resources_view`, `resource_states_view`, `resource_changes_view`, `operations_tree_view`) now return structured projections suitable for the TUI to render. The ops-splice algorithm preserves the existing `ops_seq_counter` arrival-order semantics on structured trees.
5. (5c) `tui.rs` reads structured projections and calls `lusid-render::Render::render()` per node to produce ratatui `Text`.
6. (5c) Rewrite the ~13 `apply-stdio/src/lib.rs` tests using structured `ResourceParams::File(...)` / `ResourceState::File(...)` / etc. constants. Confirm the lifecycle/projection assertions still hold.

**Acceptance criteria (5a):**
- `AppUpdate` variants carry structured types over the wire.
- `cargo test -p lusid-apply -p lusid-apply-stdio` passes (tests using the `to_view()` shim).
- Manual: local apply still renders the same.

**Acceptance criteria (5b):**
- `LeafState` carries structured payloads; no `View` fields anywhere in `AppView` / `LeafState` / `ResourcesTree`.
- Projection tests pass against structured trees.

**Acceptance criteria (5c):**
- `tui.rs` has no `lusid_view` imports.
- TUI renders identically to before Phase 0 (same content, structured wire under the hood).
- All existing apply-stdio tests rewritten and passing.

**Verify (final, after 5c):**
```
cargo test --workspace
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local apply -y
# Compare TUI rendering with a pre-Phase-0 manual snapshot. Same content.
```

**Pitfalls:**
- **Ship the full `PlanMeta` over the wire** (after Task 2-3 it serde-derives). Do NOT introduce a wire shadow that drops fields. Task 9 depends on this; if it's stripped here, Task 9 explodes from "verification + helpers" into a multi-day plumbing task.
- `PlanNodeId` (in `plan/src/id.rs`) — confirm it serde-derives or add the derive in this task.
- `PlanNodeId::PlanItem { plan_id: PlanId::Path(PathBuf) }` carries an absolute path from the producer. The consumer side has no filesystem semantics for it — it's display-only. Don't try to canonicalize.
- The TUI's existing `LeafState` enum already carries domain shapes indirectly via `View`. Update it to carry `ResourceState` / `ResourceChange` / `PlanTree<Operation>` directly. The state machine logic doesn't change; only the payload types.
- **Tests will need rewriting**: the ~13 tests in `apply-stdio/src/lib.rs` use `View::Span("foo")` constants throughout. After this task they need to use structured payloads (`ResourceParams::File(...)`, `ResourceState::File(...)`, etc.). Budget time for this in your estimate.

---

## Task 6 — Delete `view/` crate (stop-rule milestone)

**Status:** done ddd21e2

**Goal:** Remove the `view/` crate, the `Render` trait that lived there, and every remaining `impl_display_render!` invocation. This is the commitment: Phase 0 is finished.

**Why:** Without this final cleanup, View and structured-shadows coexist indefinitely. Stop-rule prevents drift.

**Files to read first:**
- Output of `cargo metadata` to see who still depends on `lusid-view`
- `grep -r "lusid_view\|lusid-view\|view::" --include="*.rs" .`

**Plan:**
1. Search the workspace for any remaining references to `lusid-view`, `lusid_view`, `View`, `ViewTree`, `Render` (from view's trait). After Tasks 4-5 there should be very few.
2. For each one, replace with the structured equivalent (call `lusid-render::Render` instead of `lusid-view::Render`) or remove.
3. Audit `Fragment`: after Task 1 stripped style metadata, `Fragment` may be vestigial (Span/Line/Paragraph cover all real uses). If no code path still requires its no-separator-concat semantics, drop it.
4. Delete `view/` directory.
5. Remove `view` from the workspace `[workspace]` members in `Cargo.toml`.
6. Remove `lusid-view` dependency from every `Cargo.toml` in the workspace.
7. Update `AGENTS.md`:
   - §"Reading order": replace `view/` references with `lusid-render`.
   - §"Gotchas / invariants to preserve": add a note that the wire is now fully structured; renderers consume serde types and produce ratatui/plain output.
   - Drop the §"Spans and diagnostics are important" caveat about `Spanned<...>` traveling on the wire — it now travels via `WireSpanned<T>` from Task 3.

**Acceptance criteria:**
- `view/` directory no longer exists.
- `cargo metadata --format-version 1 | grep lusid-view` returns nothing.
- Workspace builds with no warnings.
- All tests pass.
- `AGENTS.md` reflects the new architecture.

**Verify:**
```
ls view/  # should fail
cargo build --workspace
cargo test --workspace
cargo clippy --workspace -- -D warnings
```

**Pitfalls:**
- `apply-stdio/src/lib.rs` `FlatViewTree` was the wire-side projection of View. With View gone, FlatViewTree is now `FlatTree<RenderedNode>` or similar — but actually the wire ships structured domain types, so the consumer-side rendering is on the fly via `lusid-render`. FlatViewTree as a *wire type* should disappear (replaced by the per-AppUpdate-variant structured types). Audit and remove if stale.
- The `termtree` dependency on `view/` flows from `ViewTree::Display`. If the new flow still needs indented-tree rendering, move `termtree` to `lusid-render` and re-use it there.

---

# Phase 1: TUI redesign

After Phase 0, the wire is fully structured and the renderer lives in its own crate. Phase 1 builds the new UI on top of that foundation.

---

## Task 7 — `lusid local parse` + `--parse-only` short-circuit

**Status:** done cf04447

**Goal:** New subcommand that parses + validates a plan and renders the plan-item tree, without probing any state or applying any operation.

**Why:** Smallest unit of new TUI-facing functionality. Plan authors gain a quick "does my plan even type-check?" command. Unlocks epoch annotations (Task 17) and grounds the TUI's terminal-page rendering.

**Files to read first:**
- `lusid-apply/src/lib.rs` (full)
- `lusid-apply/src/main.rs`
- `lusid/src/lib.rs:69-95` (Cmd enum), `298-321` (cmd_local_apply)
- `lusid/src/tui.rs` (top-level event loop + outcome handling)

**Plan:**
1. Add `--parse-only` flag to `lusid-apply` CLI in `lusid-apply/src/main.rs`. Thread through `ApplyOptions`.
2. In `apply()`, after `ResourcesComplete` is emitted (around line 204), run `compute_epochs` as a validation step (catches cyclic dependencies), emit `PipelineInfo` (added in Task 10), and return `Ok(())` when `parse_only` is set.
3. If `compute_epochs` errors, return the error so apply exits non-zero.
4. Add `LocalCmd::Parse`, `RemoteCmd::Parse { machine_id }`, `DevCmd::Parse { machine_id }` to `lusid/src/lib.rs`. Wire to new functions `cmd_local_parse`, `cmd_remote_parse`, `cmd_dev_parse` that mirror their apply counterparts but pass `--parse-only`.
5. Set exit codes: `0` = clean, `1` = validation error, `2` = read/IO error. Map errors in `lusid-apply/src/main.rs`.

**Note on Task 10 ordering**: Task 10 adds the `PipelineInfo` event. For this task, add a `// TODO(Task 10): emit PipelineInfo` comment at the right spot; Task 10 fills it in.

**Acceptance criteria:**
- `lusid local parse` exits 0 on `examples/nginx-cluster` and shows the plan tree.
- Direct invocation `cargo run -p lusid-apply -- --parse-only --root <ex> --plan <file>` emits `ResourceParams`, `ResourcesStart`, `ResourcesNode`, `ResourcesComplete`, then exits 0. No `ResourceStates*`, `ResourceChanges*`, `Operations*`, `ApplyComplete`.
- Cyclic-requires plan exits non-zero with cycle error.
- Exit code mapping: `0` on clean parse; `1` on parse / validation / cycle errors (i.e. `ApplyError::Plan` / `ApplyError::Epoch` / `ApplyError::HostPathValidation` / `ApplyError::Secrets`); `2` on IO / context / system / JSON-parameter errors. Explicit `match` in `lusid-apply/src/main.rs`.
- No shell subprocesses spawn during parse beyond plan-source reading. Verify: `strace -f -e trace=execve lusid local parse 2>&1 | grep execve` shows no apt/dpkg/etc.
- `host-path` validation (existing `validate_host_paths`) still runs.

**Verify:**
```
cargo build --workspace
cargo test --workspace
cargo clippy --workspace -- -D warnings
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local parse
```

**Pitfalls:**
- `validate_host_paths` does `stat`/`lstat` — those are filesystem syscalls but not shell invocations. Keep them.
- The TUI today expects events through `ApplyComplete`. Truncating after `ResourcesComplete` is intentional; `tui()` exits on the wait future, not on `ApplyComplete`.

---

## Task 8 — Non-TTY detection + `--no-tui` flag + plain-log fallback

**Status:** pending

**Goal:** When stdout is not a terminal, or when `--no-tui` is passed, skip ratatui and stream a human-readable plain log instead. Confirm prompts auto-resolve to `--yes` in this mode.

**Why:** Required for CI. The current TUI uses `ratatui::init` which assumes a real terminal.

**Files to read first:**
- `lusid/src/tui.rs` (the `tui()` function + `TerminalSession`)
- `lusid/src/main.rs`, `lusid/src/lib.rs:298-321, 339-516, 754-897`
- `lusid-render` crate (for the plain rendering path)

**Plan:**
1. Add `--no-tui` boolean flag at the top-level `Cli` in `lusid/src/lib.rs`.
2. Add `is_tty_stdout()` helper using `std::io::IsTerminal` (stable, available in this toolchain).
3. Refactor `lusid/src/tui` module so `tui()` is the TUI-mode entry; add a sibling `plain()` function with the same signature. `plain()` reuses `AppView` to fold updates (same data path as the TUI), walks the structured state once per event, and prints a human digest using `lusid-render::RenderedNode::to_plain_string()`. **Do not reinvent projection logic** — share via `AppView`.
4. In each `cmd_*_apply` / `cmd_*_parse` function, choose `tui` vs `plain` based on `!cli.no_tui && is_tty_stdout()`.
5. Plain-log format (rough):
   - `ResourceParams` → `parsed plan: <n> items`
   - `ResourcesNode` → `expanded to <n> atoms across <n> epochs`
   - Per atom probe complete → `[probed] <atom id>: <state digest>`
   - Per change → `[changed] <atom id>: <change digest>`
   - Per op apply → one line per stdout/stderr line, prefixed `[op N.M] <line>`
   - `OperationApplyComplete` → `[ok] <op id>` or `[err] <op id>: <error>`
   - `ApplyComplete` → `apply complete: <n> changes`
6. Task 16's confirm flow honors `--no-tui` and non-TTY: if not `--yes` and non-TTY, exit at startup with "interactive confirmation requires a TTY; pass --yes or use a terminal".

**Acceptance criteria:**
- `lusid local apply | cat` produces readable plain output, doesn't corrupt terminal.
- `lusid --no-tui local apply -y` on a TTY produces plain log (skips ratatui).
- `lusid local apply` non-TTY without `-y` errors with the clear message.
- Plain log preserves event order.

**Verify:**
```
cargo build --workspace
cargo test --workspace
cargo clippy --workspace -- -D warnings
echo "" | cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml --no-tui local parse
```

**Pitfalls:**
- `ratatui::init()` MUST NOT be called when non-TTY.
- The child (`lusid-apply`) doesn't care about TTY-ness. Don't pass `--no-tui` down.
- Print plain log lines to stderr, not stdout (stdout is reserved for the child's JSON protocol on pipe-through cases).

---

## Task 9 — Plan-item metadata on `ResourcesNode`

**Status:** pending

**Goal:** Ship structured per-plan-item metadata (id, requires, required_by, on_change handlers) over the wire as part of `ResourcesNode`. After Phase 0, this is just including `PlanMeta` in the wire payload.

**Why:** The Tree page (Task 12) needs structured plan-item info to render branches with their ids, dependencies, and handler counts. The Epochs page (Task 13) needs `requires` / `required_by` to render dependency edges.

**Files to read first:**
- `plan/src/tree.rs` (`PlanMeta` definition, now serde-derived after Phase 0)
- `apply-stdio/src/lib.rs:214-300, 461-597`
- `lusid-apply/src/lib.rs:170-204`

**Plan:**
1. After Task 5, `ResourcesNode` already carries `PlanTree<Resource>` which carries `PlanMeta` per branch. Confirm the meta is reaching the consumer correctly (the consumer needs to walk the tree and use `meta.id`, `meta.requires`, `meta.required_by`, `meta.handlers` per branch).
2. Add an `AppView::plan_item_meta(branch_arena_index: usize) -> Option<&PlanMeta>` helper for the TUI to query.
3. Tests: a plan with `requires` and `on_change` produces a `ResourcesNode` whose tree carries those fields. AppView returns them correctly per arena index.

**Acceptance criteria:**
- A plan with `requires`, `required_by`, and `on_change` produces non-empty `PlanMeta` in the wire payload.
- A plan with none produces empty `PlanMeta` (default values).
- AppView returns the right meta for each branch arena index.

**Verify:**
```
cargo test -p lusid-apply-stdio
cargo test --workspace
```

**Pitfalls:**
- After Phase 0 this task is mostly a verification + helper-adding task. If Task 5 left `PlanMeta` out of the wire (unlikely if you followed the plan), include it here.
- Arena indices in the shipped tree mirror `lusid_tree::FlatTree::from` pre-order. Mirror this in the consumer.

---

## Task 10 — `PipelineInfo` event + Phase grouping on `OperationsApplyEpochAdded`

**Status:** pending

**Goal:** Ship the total resource-epoch count once at startup (for the header strip) and tag every operation epoch with its resource epoch + Phase (A/B).

**Why:** The Epochs page (Task 13) needs to group ops by resource epoch and distinguish Phase A vs Phase B. The header strip needs the total epoch count.

**Files to read first:**
- `lusid-apply/src/lib.rs:215-328, 349-440`
- `apply-stdio/src/lib.rs:214-300, 461-597`

**Plan:**
1. In `apply-stdio/src/lib.rs`, add:
   ```rust
   #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
   pub enum Phase { A, B }

   // New variant on AppUpdate:
   PipelineInfo {
       resource_epochs_total: usize,
       atom_epoch: HashMap<usize, usize>,  // atom-arena-index → resource-epoch
   }
   ```
   Extend `AppUpdate::OperationsApplyEpochAdded` with `resource_epoch: usize` and `phase: Phase`.
2. In `lusid-apply/src/lib.rs`, after `compute_epochs` (line 216), build the `atom_epoch` map by walking the computed epochs and mapping each atom's arena index to its epoch number. Emit `PipelineInfo { resource_epochs_total: epochs_count, atom_epoch }`. Must also fire under `--parse-only` (Task 7). Position: strictly between `ResourcesComplete` and the per-epoch loop.
3. Plumb `resource_epoch_idx` and `Phase` into `apply_op_phase`. Phase A call site passes `Phase::A`; Phase B call site passes `Phase::B`.
4. Inside `apply_op_phase`, attach `resource_epoch` and `phase` to every emitted `OperationsApplyEpochAdded`.
5. Update `AppView` to store the new fields. Expose `AppView::resource_epochs_total()`, `AppView::epoch_of_atom(idx) -> Option<usize>`, and per-op-epoch accessors.

**Acceptance criteria:**
- Exactly one `PipelineInfo` event per apply, before any per-epoch event.
- `PipelineInfo` carries both the total epoch count and a complete `atom_epoch` map covering every atom arena index in the shipped tree.
- Every `OperationsApplyEpochAdded` carries both `resource_epoch` and `phase`.
- AppView exposes total, per-atom-epoch lookup, and per-op-epoch accessors.
- `--parse-only` emits `PipelineInfo`.

**Verify:**
```
cargo test -p lusid-apply
cargo test -p lusid-apply-stdio
cargo test --workspace
```

**Pitfalls:**
- The existing monotonic `op_epoch_counter` is preserved for the per-op `index: (epoch, op)` tuple — we're adding fields, not replacing.

---

## Task 11 — Redact secrets at domain-type construction

**Status:** pending

**Goal:** When a file resource's source is a secret (under the secrets directory) or it's an `@resource/secret`, the structured `ResourceChange` / `ResourceState` carries `Content::Redacted { len, sha256 }` instead of the raw bytes.

**Why:** Task 14 will render real file content as a unified diff in the detail pane. Without this redaction, the new TUI leaks decrypted secrets to operator terminal scrollback — a regression vs the current TUI which doesn't show diffs.

**Files to read first:**
- `secrets/src/redactor.rs`
- `lusid-apply/src/lib.rs:131-133, 386-422`
- `resource/src/resources/file.rs` and `resource/src/resources/secret.rs`

**Plan:**
1. Add a `Content` enum in the file resource (or a shared location):
   ```rust
   pub enum Content {
       Bytes(Vec<u8>),
       Redacted { len: usize, sha256: [u8; 32] },
   }
   ```
   Used in `FileState::Sourced { content: Content }`, `FileChange::Sourced { before: Content, after: Content }`, etc.
2. At construction time — when the file resource is built from `ResourceParams` via `ResourceParams::resources()` in `resource/src/lib.rs` (around line 412 — verify exact location) — mark the resource with `is_secret: bool` based on whether its source path resolves under the project secrets directory.
3. When computing state/change for a secret resource, populate `Content::Redacted { len, sha256 }` instead of `Content::Bytes`. The hash makes "did this secret change?" answerable without revealing content.
4. `@resource/secret` resources always produce redacted views (their whole purpose is to deliver plaintext to a target; the operator's terminal should never see it).
5. Tests: a secret resource produces redacted Content; a non-secret resource produces normal Content; no plaintext appears in the serialized wire payload.

**Acceptance criteria:**
- A plan with `@resource/secret` produces wire payloads with no plaintext.
- A file resource whose source is under the project's `secrets/` produces redacted Content.
- Non-secret resources produce normal Content (bytes-on-wire).
- Op stdout/stderr redaction (existing `Redactor` for line-level) still works.

**Verify:**
```
cargo test --workspace
cargo clippy --workspace -- -D warnings
# Manual: grep through a captured apply event stream for known secret plaintexts; nothing found.
```

**Pitfalls:**
- For v1 we only redact files whose *source path* points into the secrets dir — substring redaction (path-agnostic, content-driven) is a later refinement.
- `Content::Redacted` must serde-derive; trivial.

---

## Task 12 — Tree page

**Status:** pending

**Goal:** New primary TUI page. Plan-item tree on the left, detail pane on the right. Status badges with the Terraform palette. Responsive layout. Replaces the existing 6-stage pipeline strip.

**Why:** Main UX surface for the redesign.

**Files to read first:**
- `lusid/src/tui.rs` (full — rewriting most of it)
- `apply-stdio/src/lib.rs:380-779`
- `lusid-render` crate (for converting structured types to ratatui Text)

**Plan:**
1. Strip out the old stage strip (`PipelineStage`, `follow_pipeline`, the 6 per-stage `TreeState`s, `draw_main_pipeline`). Replace with: `UiPage::Tree`, `UiPage::Epochs`, `UiPage::Stderr`.
2. Define a unified `TreePageState`:
   - Selected node (arena index in the plan-item tree)
   - Per-branch collapse state
   - Filter string (set by `/`)
   - `show_unchanged: bool` (toggled by `u`; default `false` after parse, `true` during/after apply)
3. Build the **plan-item tree** from the wire's structured types. Each branch with `PlanMeta` displays its `id` view as the label; falls back to `.` for anonymous branches. Each branch carries an aggregate status (5-state rollup from children).
4. Render the page:
   - Header strip (1 row, top): `lusid · <subcommand> · epoch K/N · <status summary> · <key hints>`
   - Body split by terminal width:
     - ≥100 cols: 50/50 horizontal (tree | detail)
     - <100 cols: 60/40 vertical (tree / detail)
   - Footer (1 row): help line (Task 16 adds confirm prompt later)
5. Detail pane: implement the per-LeafState content table. Use `lusid-render::Render` to convert structured types to ratatui `Text`.
6. Keymap: j/k/Up/Down/h/l/Left/Right/Space/Enter/Tab/gg/G/?/q/Esc/u. `/` filter. `1`/`2`/`e` page switch (Epochs is a placeholder until Task 13).
7. Filter dims non-matches.
8. Palette: define `Color`/`Modifier` constants for each `SemanticTag` in `lusid/src/tui/palette.rs`.

**Acceptance criteria:**
- `lusid local parse examples/nginx-cluster/lusid.toml` shows the plan tree with correct labels and structure.
- After apply: each atom has the correct badge. Branches show rolled-up state.
- Resizing across 100 cols toggles layout cleanly.
- Detail pane updates as selection moves. Each LeafState renders the right content.
- Keymap behaves per spec.
- Old stage strip code is gone.

**Verify:**
```
cargo build --workspace
cargo test --workspace
cargo clippy --workspace -- -D warnings
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local parse
# Manual: navigate, verify rendering at different widths.
```

**Pitfalls:**
- "Show unchanged off" filter must preserve navigation across visible rows.
- Detail pane state (selected atom, scroll position) persists across page switches.
- Header strip "epoch K/N" reads from `PipelineInfo`; show `epoch ?/?` until it arrives.

---

## Task 13 — Epochs page

**Status:** pending

**Goal:** Page 2: stacked sections, one per resource epoch. Each section shows the epoch's atoms, Phase A ops (merged), Phase B handlers.

**Why:** User asked for an epochs page showing both resources and operations. Hierarchy reveals Phase A→B order naturally.

**Files to read first:**
- Task 12 output (Tree page; reuse detail pane and badge rendering)
- `apply-stdio/src/lib.rs` (operations_epochs, Task 10 additions)
- `lusid-apply/src/lib.rs:244-440`

**Plan:**
1. Add `UiPage::Epochs`. Add `EpochsPageState` (selected section, scroll offset, per-section collapse state).
2. Compute the layered view. For each `resource_epoch` 0..=`resource_epochs_total`-1:
   - Section header: `Epoch K/N · A atoms · X changed · H handlers`
   - Atoms list: leaves where `AppView::epoch_of_atom(idx) == Some(K)`. (Task 10 ships the `atom_epoch` map in `PipelineInfo`; just consume it.)
   - Phase A subsection: ops from `operations_epochs` where `phase == A && resource_epoch == K`
   - Phase B subsection: same for Phase B. If empty, show `(no on_change handlers fired)`
3. Per-atom row: badge + label + `← requires: <id> (epoch J)` annotation when `requires` resolves to an atom in earlier epoch J. Derived from `PlanMeta.requires` + `epoch_of_atom`.
4. Detail pane behavior: same as Tree page; selection drives content.
5. Same keymap. `Space` collapses an Epoch section (default expanded).

**Acceptance criteria:**
- Multi-epoch plan (nginx-cluster) renders correctly: install → write config → reload.
- Phase B handlers appear under the epoch their resource was in.
- A `requires` edge crossing epochs shows as annotation.
- Selection updates detail pane same as Tree.
- Tab cycles to Tree; numbered keys jump.

**Verify:**
```
cargo test --workspace
cargo clippy --workspace -- -D warnings
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local parse
# Press 2 to switch to Epochs.
```

**Pitfalls:**
- Phase B ops are determined dynamically per-apply (only if bearing branch changed). Parse mode shows them as "would fire if change occurred" or simply omits them — pick "show with dimmed style + label 'conditional'".
- Empty resource epochs are valid (rare but possible). Render the section header and an empty body.

---

## Task 14 — Diff renderer in detail pane

**Status:** pending

**Goal:** When the detail pane shows a `Changed` atom, render the diff well. Scalars get `before | after` table; file content gets unified diff (optional side-by-side at ≥140 cols).

**Why:** The detail pane needs to be scannable. Raw structured data is not.

**Files to read first:**
- Task 11 output (redacted Content type)
- The structured `ResourceChange` types per family
- `Cargo.toml` (adding `similar` dependency)

**Plan:**
1. Add `similar = { version = "2", default-features = false, features = ["text"] }` to `lusid-render/Cargo.toml` (the renderer owns diff logic; the TUI just displays).
2. In `lusid-render`, add diff rendering for `ResourceChange`:
   - Scalar fields (mode, owner, package version): produce `RenderedNode::Tagged { tag: Modified, content: [Plain("field"), Plain("before"), Plain("after")] }` rendered as a table row.
   - File content (`Content::Bytes` → `Content::Bytes`): unified diff via `similar`, default 3 lines context, with `Tagged { Added }` / `Tagged { Removed }` / `Tagged { Unchanged }` per line.
   - Redacted file (`Content::Redacted`): render `<redacted: N bytes, sha256:abcd...>` on both sides. No diff.
   - Binary content: detect by UTF-8 invalidity or `\0` byte; render `<binary content, N bytes>`.
3. In the TUI's detail pane (Task 12), call into `lusid-render::diff_change` and render the result as ratatui `Text`.
4. Side-by-side toggle (`s`): when terminal width ≥140 cols and toggled, render two parallel paragraphs.
5. Always prefix the diff with the atom's path (plan-item id + atom id).

**Acceptance criteria:**
- A `@resource/file` content change renders as readable unified diff with green/red.
- A mode change (e.g. 0644 → 0600) renders as a scalar table row.
- A `@resource/secret` renders as `<redacted: N bytes, sha256:...>` on both sides; no plaintext.
- `s` toggles side-by-side at ≥140 cols.

**Verify:**
```
cargo test -p lusid-render
cargo test --workspace
cargo clippy --workspace -- -D warnings
# Manual: apply on nginx-cluster with config change; check detail pane.
```

**Pitfalls:**
- Binary detection: don't attempt unified diff on non-UTF-8 content.
- Side-by-side at <140 cols: refuse toggle; show a brief footer hint.

---

## Task 15 — `lusid-cmd` refactor: expose `ChildStdin`

**Status:** pending

**Goal:** Make the spawned child's stdin accessible from the caller, so the parent CLI can write JSON acks to `lusid-apply`. Pure plumbing — no behavior change.

**Why:** Required by Task 16 (per-epoch confirm).

**Files to read first:**
- `cmd/src/lib.rs` (especially `Command`, `CommandOutput`, `spawn`, `output`)
- `lusid/src/lib.rs:298-321, 339-516, 754-897` (call sites)
- `lusid/src/tui.rs:81-100` (the `tui()` signature)
- `ssh/src/command.rs:27-71` (mirror the SSH analog)

**Plan:**
1. Modify `CommandOutput` to own stdin:
   ```rust
   pub struct CommandOutput {
       pub stdout: ChildStdout,
       pub stderr: ChildStderr,
       pub stdin: ChildStdin,
       pub status: BoxFuture<'static, Result<(), CommandError>>,
   }
   ```
2. Update `Command::output()` to take stdin before constructing the status future.
3. Update every call site in `lusid/src/lib.rs` (local apply destructures `output.stdout, .stderr`; add `.stdin`).
4. Update `tui()` and `plain()` signatures to accept `stdin: impl AsyncWrite + Unpin`. Ignore it for now; Task 16 wires it.
5. Pass `output.stdin` (local) and `handle.channel.stdin()` (SSH) at call sites. **Verify first**: read `ssh/src/command.rs` and confirm the SSH channel exposes a writable stdin handle that lives alongside stdout/stderr/wait. If it doesn't, refactor it symmetrically with the local change (a parallel `cmd/`-style mini-refactor on the SSH side).

**Acceptance criteria:**
- `cargo build` passes.
- Local apply still works end-to-end.
- Remote / dev apply still works.
- `tui()` / `plain()` accept and ignore a `stdin` argument.
- `lusid_ssh::SshCommandHandle` (or whatever the type is called) exposes `stdin` symmetric to `stdout` / `stderr`.

**Verify:**
```
cargo build --workspace
cargo test --workspace
cargo clippy --workspace -- -D warnings
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local apply -y
```

**Pitfalls:**
- `tokio::process::Child` lets you take stdin/stdout/stderr exactly once each. Order matters.
- If a call site discards stdin, drop the handle so the child sees EOF.

---

## Task 16 — Per-epoch confirm: `EpochReady` + ack channel + `--yes` + footer prompt

**Status:** pending

**Goal:** Apply pauses between resource epochs (after probes and op planning, before any op runs). TUI shows a footer prompt; user `y`/Enter sends ack, `n`/Esc aborts. `--yes` skips.

**Why:** The user's "confirm before changes apply" requirement, ergonomically integrated with per-epoch probing.

**Files to read first:**
- Tasks 9, 10, 12, 15 outputs
- `lusid-apply/src/lib.rs:244-328`
- `apply-stdio/src/lib.rs:214-300`
- `lusid/src/tui.rs` (post-Task 12)

**Plan:**
1. Wire format additions in `apply-stdio/src/lib.rs`:
   ```rust
   #[derive(Debug, Clone, Serialize, Deserialize)]
   pub struct EpochSummary {
       pub atoms_total: usize,
       pub atoms_changed: usize,
       pub handlers_pending: usize,
       pub change_labels: Vec<ChangeLabel>,  // structured; not View
       pub truncated_count: usize,
   }

   pub struct ChangeLabel {
       pub atom_id: String,    // rendered PlanNodeId
       pub kind: ChangeKind,   // Added/Removed/Modified
       pub summary: String,    // one-line label
   }

   AppUpdate::EpochReady { resource_epoch: usize, summary: EpochSummary }
   ```
2. Reverse-direction wire: `enum AckAction { Apply, Abort }` serialized as `{"action": "apply"}` / `{"action": "abort"}`, one per `EpochReady`.
3. `lusid-apply` side:
   - Add `--yes` / `-y` flag.
   - In per-epoch loop, after Phase A probes complete and change ops computed but before they apply, build `EpochSummary` from the structured changes and emit `EpochReady`. Skip emission for empty epochs (no atoms changed, no handlers pending) to reduce prompt fatigue.
   - If `--yes`: skip stdin read; treat as `Apply`.
   - Else: read one line from `tokio::io::stdin()`, parse as `AckAction`. EOF or parse error = `Abort`.
   - On `Abort`: do not run ops for this or later epochs. Emit `ApplyComplete { had_changes: ... }`. Exit non-zero (130, "user-canceled").
4. `lusid` CLI side:
   - Add `--yes` / `-y` flag on `local apply`, `remote apply`, `dev apply`. Plumb to `lusid-apply --yes`.
   - Update `tui()` to wire `stdin` from Task 15. When `EpochReady` arrives without `--yes`, show footer prompt. On `y`/Enter: write `{"action":"apply"}\n`. On `n`/Esc: write `{"action":"abort"}\n`.
   - Footer states:
     - Idle: `↵ apply  n abort  d details  ? help`
     - Running: `running epoch K — abort takes effect after this epoch`
   - Non-TTY without `--yes`: exit at startup (Task 8's error path).
5. Cancellation: ack at epoch boundaries only. Mid-epoch, in-flight probes/ops drain.
6. Tests: AppView records `EpochReady`; `AckAction` round-trips; integration test with piped stdin verifies pause/resume; `--yes` skips reads.

**Acceptance criteria:**
- `lusid local apply` on nginx-cluster pauses before each non-empty resource epoch.
- `lusid local apply -y` runs straight through.
- Abort at epoch 2 of 4 exits non-zero with "Aborted at epoch 2. Epochs 0..1 have been applied. Re-run to retry."
- Non-TTY + no `--yes` errors at startup; doesn't hang.
- Remote / dev apply also gates correctly.

**Verify:**
```
cargo test --workspace
cargo clippy --workspace -- -D warnings
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local apply
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local apply -y
```

**Pitfalls:**
- Don't ack from the same future that reads stdout. Use a separate task that listens for key events and writes to stdin.
- Don't read stdin in `--yes` mode (could hang if parent closes stdin).
- Empty epochs: skip emission (no prompt, just continue).
- For parse mode (`--parse-only`), don't read stdin.

---

## Task 17 — Polish: `n`/`N` jump-to-change + epoch tags

**Status:** pending

**Goal:** Two small quality-of-life additions.

**Why:** `n`/`N` lets users find changes in big plans quickly. Epoch tag in Tree page shows "when does this run" without flipping to Epochs page.

**Files to read first:**
- Task 12 output (Tree page navigation)
- Task 10 / Task 13 outputs (epoch info on the wire)

**Plan:**
1. On the Tree page, bind `n` to "select next atom whose rollup state is `changed`, `failed`, or contains a change". `N` is reverse. Skip `ok` and `planned`.
2. In plan-item tree rendering, append `(epoch K)` next to each branch label when terminal width ≥80 cols. Use wire's epoch mapping.
3. Tests: jump-to-change finds expected atoms; epoch tag matches `compute_epochs`.

**Acceptance criteria:**
- `n`/`N` cycles through changed atoms in DFS order.
- Plan tree rows show `(epoch K)` after label at ≥80 cols.
- No-op when no changes (with a brief footer toast "no more changes").

**Verify:**
```
cargo test --workspace
cargo clippy --workspace -- -D warnings
cargo run -p lusid -- --config examples/nginx-cluster/lusid.toml local parse
```

**Pitfalls:**
- Wrap-around: pick "no-op with toast" over "wrap silently".
- Epoch tag must not widen the tree column past where labels truncate; hide at narrow widths.

---

## When all tasks are done

1. Run the full verification battery:
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
3. Manually exercise the TUI: navigate, confirm, abort, resize across the responsive breakpoint, view a secret diff, view a file diff, hit `?` for help on each page.
4. Update `apply-stdio/README.md` to reflect the new wire format (structured types throughout; no View; `PipelineInfo`, `EpochReady`, extended `OperationsApplyEpochAdded`).
5. Update `README.md`'s "Apply a plan" section to mention `parse`, `-y`, `--no-tui`.
6. Update `AGENTS.md` §"Reading order" to reflect that the wire is now structured and `lusid-render` owns rendering.
7. Surface the branch to the user for final review and PR creation.
