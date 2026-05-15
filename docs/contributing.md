# Contributing

How the codebase fits together, for people working on lusid itself.

For end-user docs, see [the index](./README.md).

## Workspace layout

lusid is a Cargo workspace. The crates split by responsibility:

### Pipeline

| Crate | What it does |
| --- | --- |
| [`plan`](../plan/) | Loads `.lusid` files, evaluates Rimu, produces a tree of typed `ResourceParams`. |
| [`params`](../params/) | Param schema + the typed parser used at the resource boundary. |
| [`resource`](../resource/) | Resource types: params → atoms → state → change → operations. |
| [`operation`](../operation/) | Concrete mutations that run on the target. |
| [`causality`](../causality/) | Dependency ordering — `compute_epochs` flattens a tree into topological layers (Kahn). |
| [`tree`](../tree/) | Generic `Tree` and arena-backed `FlatTree` types. |
| [`lusid-apply`](../lusid-apply/) | Pipeline orchestrator. Loads a plan, runs the pipeline, streams JSON `AppUpdate`s on stdout. |

### CLI

| Crate | What it does |
| --- | --- |
| [`lusid`](../lusid/) | User-facing CLI. Reads `lusid.toml`, spawns `lusid-apply`, renders progress in a ratatui TUI. |
| [`apply-stdio`](../apply-stdio/) | Shared wire protocol between `lusid-apply` and the TUI. |
| [`view`](../view/) | Serializable styled-text view primitives. |
| [`config`](../lusid/src/config.rs) | `lusid.toml` schema. |

### Targets

| Crate | What it does |
| --- | --- |
| [`vm`](../vm/) | Local QEMU VMs for `dev apply` / `dev ssh`. |
| [`ssh`](../ssh/) | Async SSH client (russh-based) for `dev apply` and `remote apply`. |
| [`machine`](../machine/) | Declarative target-machine description. |
| [`system`](../system/) | Runtime host detection (hostname, arch, OS, user). |

### Plumbing

| Crate | What it does |
| --- | --- |
| [`ctx`](../ctx/) | Shared runtime context (root path, XDG dirs, HTTP client). |
| [`cmd`](../cmd/) | Thin wrapper over `tokio::process::Command` with sudo / stdio routing. |
| [`fs`](../fs/) | Async filesystem helpers with rich errors. |
| [`http`](../http/) | HTTP client for fetching remote artifacts. |
| [`store`](../store/) | Abstract content store for bytes referenced by a plan. |
| [`rimu-interop`](../rimu-interop/) | Rust ↔ Rimu value bridge. |
| [`secrets`](../secrets/) | Age-encrypted secret loading + the `@resource/secret` machinery. |

Each crate has its own README with a fuller orientation.

## Reading order

For someone new to the codebase, [`AGENTS.md`](../AGENTS.md) at the repo root has a "Reading order" section pointing at the most useful files. Briefly:

1. `lusid-apply/src/lib.rs` — pipeline overview.
2. `plan/src/lib.rs` — how a `.lusid` becomes a tree.
3. `params/src/lib.rs` — schema and validation.
4. `causality/src/epoch.rs` — Kahn's algorithm in lusid form.
5. `lusid/src/tui.rs` — TUI rendering.

## Pipeline summary

```
lusid CLI
  │ spawn
  ▼
lusid-apply
  │ 1. plan()                                   → PlanTree<ResourceParams>
  │ 2. ResourceParams::resources()              → tree of Resource atoms
  │ 3. Resource::state(ctx) (async, per leaf)   → tree of (Resource, State)
  │ 4. Resource::change(state)                  → tree of Option<Change>
  │ 5. Change::operations()                     → tree of Operation
  │ 6. compute_epochs()                         → Vec<Vec<Operation>>
  │ 7. per epoch: merge + apply (stream output)
  ▼
TUI (ratatui)        — folds AppUpdate JSON into a live view
```

Each phase emits start / per-node / complete events as newline-delimited JSON on stdout. The TUI deserializes them and renders.

## Project conventions

(Cribbed from [`AGENTS.md`](../AGENTS.md) — read it for the full set.)

### Coding style

- Error handling: `thiserror` + rich enums, no `anyhow`-style catchalls.
- Many crates use `displaydoc::Display` — follow that pattern.
- Blank line between error enum variants.
- Prefer small pure functions.
- Keep public APIs conservative — add new types/functions before changing signatures.
- Import order: std, external crates, internal `lusid_*` crates, within crate (`crate::` / `self::` / `super::`), blank line between groups.

### Spans are load-bearing

Most errors carry a `Spanned<T>` or embed a `Span` so diagnostics can point at the offending plan line. When adding parsing or validation:

- Preserve spans.
- Prefer `Spanned<Error>` over bare `Error` for value-attributable failures.

### Causality IDs must be unique

`compute_epochs` fails on duplicate IDs across leaves/branches. Code that mints ids should scope them (see `map_plan_subitems` for the pattern).

### Stdout JSON protocol

`lusid-apply` writes only newline-delimited `AppUpdate` JSON to stdout. Tracing goes to stderr. Don't print human-readable text to stdout from apply.

### Privileged operations

Anything that needs root wraps its command in `Command::sudo()`. Only escalate when the underlying tool actually needs it (`apt`, `pacman` — yes; `git`, `command` — no, unless the user asks).

### Comments

- Default to no comments. Add one only when the WHY is non-obvious.
- Don't reference time-bound context ("added for X", "the old way was Y") — that rots.
- `Note(cc): xxx` for observations preserved without an action.
- `TODO(cc): xxx` for things we should change later.

## Adding a new resource

1. New module under `resource/src/resources/`.
2. Implement `ResourceType` for a zero-sized marker (`struct MyResource;`).
3. Add a variant to each of `ResourceParams`, `Resource`, `ResourceState`, `ResourceStateError`, `ResourceChange` in `resource/src/lib.rs`.
4. Thread it through the match arms in `resource/src/lib.rs`.
5. Register the module in `plan/src/resource.rs` so plans can reference `@resource/<id>`.
6. Document it in [`docs/reference/resources.md`](./reference/resources.md).

If the resource needs new operation types, follow the same pattern in `operation/`.

## Build / test

```sh
cargo build
cargo test
cargo clippy --workspace -- -D warnings
cargo fmt --all
```

The `justfile` has shortcuts for the examples — e.g. `just nginx-cluster-apply-a` chains the worker build, CLI build, and apply.

## Before submitting changes

A short checklist:

- Span-aware errors preserved where applicable?
- stdout JSON protocol still well-formed (no stray prints in `lusid-apply`)?
- Causality IDs unique and dependencies valid?
- New operations safe / non-interactive / appropriately `sudo()`-wrapped?
- Tests for logic-heavy changes?
