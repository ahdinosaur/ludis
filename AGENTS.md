# AGENTS.md

**lusid** is a Rust project for declarative machine configuration via “plans”, producing a dependency-aware resource/operation tree and applying it with a streaming TUI.

## What this project is

**Lusid** takes a `.lusid` plan (written in the **Rimu** language), optionally a parameters object, and:

1. Loads + evaluates the plan’s `setup(params, system)` function → returns a list of **PlanItem**s.
2. Converts PlanItems into either:
   - **Resource modules** (`@resource/*`) → become typed `ResourceParams` (apt/file/pacman today)
   - Or nested plans (module path) → recursively planned
3. Validates parameter schemas and values (with good span/source error reporting).
4. Builds a **causality tree** (nodes can have `id`, `requires`, `required_by` dependencies).
5. Computes dependency **epochs** (topological layers).
6. Applies operations epoch-by-epoch, streaming structured UI updates as JSON to stdout.
7. The `lusid` CLI runs `lusid-apply-*` and renders a TUI from those updates.

Key design themes:
- Strong typing at the boundaries (params → typed structs).
- Span-aware diagnostics using Rimu `Spanned<T>` values.
- Tree-first architecture: nested `Tree` and arena-based `FlatTree`.
- Dependency ordering via `CausalityMeta { id, requires, required_by }` and Kahn’s algorithm.

## Principles

- Premature optimization is the root of all evil
- Do not second guess or make assumptions
- Prefer robustness over performance
- Achieve performance with simple fit-for-purpose abstractions, not clever hacks

### Complexity check

Before adding significant amounts of code, verify:

1. The approach is solid — not just the first thing that came to mind.
2. No simpler alternative achieves the same goal.
3. Compare to industry-standard tools and specifications if relevant.
4. Check if a good Rust crate already handles the task.

Complexity is fine when warranted - this is a genuinely complex project. The point is to be deliberate.

## Reading order

To understand the runtime behavior, read in this order:
1. `lusid-apply/src/lib.rs` (full pipeline)
2. `plan/src/lib.rs` (planning recursion + resource modules)
3. `params/src/lib.rs` (schema/value validation)
4. `causality/src/epoch.rs` (dependency scheduling)
5. `lusid/src/tui.rs` (how updates are rendered)

## “Gotchas” / invariants to preserve

### Spans and diagnostics are important
Many errors are `Spanned<...>` or embed `Span` to point to plan source locations. When adding new parsing/validation logic:
- Preserve spans where possible.
- Prefer returning `Spanned<Error>` variants when the error is attributable to a specific value.

### ParamType HostPath vs TargetPath
In `params`:
- `HostPath` expects a **relative** string; it is resolved relative to the source file directory using span source info.
- `TargetPath` expects an **absolute** path string.

If you add new path-like types, follow this pattern and be explicit about absolute/relative requirements.

### `state: "sourced"` vs `state: "linked"`

`@resource/file` and `@resource/directory` both expose two ways to materialise a host-path source on the target:

- **`state: "sourced"`** — byte-copy of the file, or recursive `cp -r` of the directory tree, into `path`. Accepts optional `mode`/`user`/`group`. Edits to `source` only propagate on the next apply. Use this when the bytes need to live on the target independently of the operator's filesystem (system configs, deployed artifacts, dev/remote apply).
- **`state: "linked"`** — atomic symlink at `path` pointing to `source`. Refuses `mode`/`user`/`group` at the parser level (Linux symlinks have no meaningful mode of their own, and chmod/chown via the link silently mutates the target file in the operator's repo — declined). Edits to `source` show up at `path` immediately. Use this for dotfiles-style ergonomics.

Both states validate at plan-load time (post-`plan()`, pre-resources expansion) that `source` exists and has the expected type — regular file for `@resource/file`, directory for `@resource/directory`. See `ResourceParams::validate_host_paths` in `resource/src/lib.rs`.

Implementation notes:
- The Linked state probe is *lexical*: `readlink(2)` against the source string. We deliberately don't canonicalise; otherwise drift between a plan declaring `./foo` and an existing link declaring something else is invisible.
- The Sourced directory state probe is intentionally weak (`path` exists as a directory ⇒ `Sourced`). Content drift in `source` after first apply is not detected; declare `state: "absent"` and re-apply to force a refresh. A content-aware recursive diff is a future direction (cf. Salt's `file.recurse`).

### Causality IDs must be unique
`compute_epochs` fails on duplicate IDs across leaves/branches. Any new code generating ids should avoid collisions (or scope them like `map_plan_subitems()` does by minting a `scope_id`).

### Streaming output protocol
`lusid-apply` emits **newline-delimited JSON** `AppUpdate` messages to stdout.
The `lusid` TUI expects this exact protocol. Avoid printing human text to stdout from `lusid-apply`; use tracing/logging to stderr.


## Build / run / test (agent checklist)

### Typical commands
- Build workspace:
  - `cargo build`
- Run CLI:
  - `cargo run -p lusid -- --help`
- Run apply binary (manual):
  - `cargo run -p lusid-apply -- --root <root> --plan <path/to/plan.lusid> --log info --params '{"k":"v"}'`
- Run tests:
  - `cargo test`
- Lint:
  - `cargo clippy --workspace -- -D warnings`
- Format:
  - `cargo fmt --all`

## Coding style expectations (match existing code)

- Error handling uses `thiserror` + rich enums; avoid `anyhow`-style catchalls.
- Many crates use `displaydoc::Display` for error messages; follow that pattern.
- Use a blank line between each error enum variant.
- Prefer small pure functions
- Keep public APIs conservative: prefer adding new types/functions instead of changing signatures.
- Maintain `Clone` friendliness when types are used in trees/flat arenas.
- Import order: std, external crates, internal crates (`lusid_*`), within crate (`crate::`/`self::`/`super::`), with a blank line between each group.

## Safety and operational concerns

This project runs privileged operations (`sudo apt-get`, `sudo pacman`, filesystem ownership changes). When adding new operations:
- Ensure commands are non-interactive.
- Avoid leaking secrets in logs/structured UI updates.
- Keep stdout/stderr streaming for long-running commands.

## Reviews

- Think about the long-term maintenance of the project
- Check all algorithms are correct
  - Look at relevant specifications where possible
- Check all unsafe usage is correct (and documented with SAFETY comments)
- Check there's not a simpler way to do (or say) what is needed
- Imagine alternative abstractions, compare with current abstractions
- Add `debug_assert!` to validate any assumptions
- Add more tests, but only if useful
- For any observations that don't lead to a change now:
  - Make a comment `Note(cc): xxx` to document for future readers,
  - Or `TODO(cc): xxx` if we should make a change in the future

## Testing

- Don't assume the current code is correct
  - Don't ever fix a test in order to pass, unless you are absolutely certain this is correct
- Before adding tests, think about specific edge cases that should be tested
  - Don't add tests just for the sake of adding tests
- If a test is redundant, remove it

## Tracing

- Use `tracing::instrument` or manual spans where they add context.
- Use all levels deliberately: `error!` for breakage, `warn!` for degraded-but-recoverable, `info!` for lifecycle events, `debug!` for operational detail, `trace!` for per-frame/hot-path detail.
- Prefer structured fields (`info!(plan_id = %id, "planning complete")`) over string interpolation.
- Write log messages as if you will read them at 3 AM debugging a production issue two years from now.

## Before submitting changes (AI agent self-check)

- Does the change preserve span-aware errors where applicable?
- Does it maintain the stdout JSON protocol from `lusid-apply`?
- Did you avoid printing non-JSON to stdout in apply?
- Are causality IDs still unique and dependencies valid?
- Are new operations safe/non-interactive and appropriately `sudo()`-wrapped?
- Did you add/adjust tests for logic-heavy changes?

