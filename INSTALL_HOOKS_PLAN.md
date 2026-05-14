# Install hooks — implementation plan (v3)

Revised after second review round. Major changes from v2:

- **`FlatTree` lacks `map_meta`** — explicitly prescribed Tree↔FlatTree round-trip at each conversion site; `inject_handlers` is typed over `Tree`, not `FlatTree`.
- **`lusid-plan` already depends on `lusid-operation`** (verified at `plan/Cargo.toml:9`); the v2 claim "add the dep" is dropped.
- **Reserved-id `debug_assert!` was dead code** — moved to the source (asserted in `map_plan_subitems` against any resource-emitted intra-scope id starting with `@@`).
- **All struct-literal sites** for `PlanMeta` and `PlanItem` enumerated explicitly.
- **Error variants now spell out literal user-visible messages** (no more bare variant names).
- **`@core/` legacy hint** moved to its own dispatcher-level variant (`LegacyCorePrefix`), since `@core/` falls through to "nested plan" today and never reaches `UnsupportedResourceModuleId`.
- **Failure-cascade runtime warning dropped** — documented in AGENTS.md instead. Tracking `is_handler` through `compute_epochs` is mechanically expensive and v1 doesn't need it.
- **"Atom" and "epoch" jargon** scrubbed from user-facing docs.
- **Cross-epoch coalescing** stays a documented v1 limitation (reviewer pushed for a global fold; tradeoff analysis below).

## Design recap (settled)

- Namespaces: `@resource/<id>` (renamed from `@core/<id>`) and `@operation/<id>` (new).
- `on_change` field on a `@resource/<id>` plan item carrying inline `@operation/<id>` items only.
- Triggers when *any* atom of the resource has a non-`None` change. Binary (no add/modify/remove filtering); no pre-remove hooks.
- Inline only — no by-reference handlers, no anonymous-reuse semantics, no inter-handler ordering.
- Per-epoch identity dedup in `Systemd::merge` and `Command::merge`.
- Failure: same model as today (`lusid-apply/src/lib.rs:374`) — apply aborts.
- Exposed for v1: `@operation/command`, `@operation/systemd`.

## DSL surface

```rimu
- module: "@resource/file"
  id: "nginx-conf"
  params:
    path: "/etc/nginx/nginx.conf"
    state: "sourced"
    source: "./nginx.conf"
  on_change:
    - module: "@operation/systemd"
      params:
        name: "nginx"
        action: "reload"
```

Constraints:
- `@operation/*` is rejected as a top-level plan item.
- `@resource/*` is rejected inside `on_change`.
- `id`, `requires`, `required_by` on inline operations are rejected (v1).

## Pipeline-level model

```
plan_item_to_resource:
  module = @resource/file, on_change = [SystemdOp::Reload]
  ↓
  PlanTree::Leaf with PlanMeta { id: Some("nginx-conf"), ..., handlers: vec![reload_op] }

map_tree (resources expansion):
  ↓
  PlanTree::Branch with PlanMeta (handlers preserved) {
    children: [file atom, mode atom, user atom, group atom]
  }

map (state probing, then change calc):
  ↓ ↓
  meta preserved through both. handlers still on the plan-item branch.

map_tree (operations expansion):
  ↓
  PlanTree::Branch (handlers preserved) {
    children: [Some(FileOp::Write), Some(FileOp::ChangeMode), None, None]
  }

FlatTree → Tree (CausalityTree::from inverse):
  ↓
  Same structure but as a nested Tree.

inject_handlers (NEW post-pass, branch-aware):
  ↓
  For branches whose meta.handlers is non-empty AND has any Some(op) descendant:
  PlanTree::Branch (handlers cleared on outer) {
    children: [
      PlanTree::Branch [id = SubItem(scope, "@@handler-anchor")] {
        children: [Some(FileOp::Write), Some(FileOp::ChangeMode), None, None]
      },
      PlanTree::Leaf [requires = [SubItem(scope, "@@handler-anchor")]] {
        node: Some(SystemdOp::Reload)
      }
    ]
  }

.map_meta(PlanMeta::to_causality):
  ↓
  CausalityTree<Option<Operation>, PlanNodeId>

compute_epochs → apply loop (unchanged).
```

## Affected crates

| Crate | Change |
|---|---|
| `plan` | Parse `on_change`; rename `core.rs` → `resource.rs`; new `operation.rs`; route `@operation/<id>`; `PlanMeta` becomes a struct; new `inject_handlers` post-pass helper. |
| `params` | No change. |
| `operation` | New `ParseParams` impls for Command/Systemd; add `Restart`/`Reload` to `SystemdOperation`; derive `PartialEq+Eq+Hash` on Systemd/Command ops + `CommandExecutor`; identity dedup in `merge`; add `lusid-params` dep. |
| `resource` | User-facing `thiserror` strings at `resource/src/lib.rs:592-601` updated to `@resource/file`/`@resource/directory`. |
| `lusid-apply` | Insert `inject_handlers` between operations expansion and `compute_epochs`; convert `FlatTree` → `Tree` for the injection pass; update all 6 conversion sites to use explicit `.map_meta(PlanMeta::to_causality)`. |
| `causality`, `tree`, `view`, `apply-stdio` | No change. |
| Examples, docs | `@core/` → `@resource/`; extend nginx example to use `on_change`. |

## Detailed changes

### 1. Namespace rename `@core/` → `@resource/`

#### Logic-bearing changes

`plan/src/core.rs` → `plan/src/resource.rs` (file rename — also adjusts the file's own internal `@core/` references at lines 2, 15, 18):
- `is_core_module` → `is_resource_module`
- `core_module` → `resource_module`
- `core_module_for_resource` → `resource_module_for_resource`
- prefix `"@core/"` → `"@resource/"`

`plan/src/lib.rs`:
- Line 37: `mod core;` → `mod resource;`
- Line 37 imports: `use crate::core::{core_module, is_core_module};` → `use crate::resource::{resource_module, is_resource_module};`
- Line 201: `if let Some(core_module_id) = is_core_module(module)` → `if let Some(resource_module_id) = is_resource_module(module)`
- Line 202: `core_module(core_module_id, params_value)` → `resource_module(resource_module_id, params_value)`
- Line 154: `UnsupportedCoreModuleId { id: String }` → `UnsupportedResourceModuleId { id: String, span: Span }` (also adds span — see §4)
- Line 155 (displaydoc): `"Unsupported core module id \"{id}\""` → `"unknown @resource/ module: \"{id}\""`

#### `LegacyCorePrefix` error variant (new)

Add to `PlanItemToResourceError`:

```rust
/// `@core/` was renamed to `@resource/`. Try: `@resource/{id}`
LegacyCorePrefix { id: String, span: Span },
```

Triggered in `plan_item_to_resource` *before* the resource/operation/nested-plan fork:

```rust
if let Some(id) = module.inner().strip_prefix("@core/") {
    return Err(PlanItemToResourceError::LegacyCorePrefix {
        id: id.to_string(),
        span: module.span(),
    });
}
```

Kept until v1.0 (not "one release") because pre-1.0 users skip versions; trivially greppable for removal.

#### User-facing strings (rotate)

- `resource/src/lib.rs:592-601` — `thiserror` strings mentioning `@core/file` / `@core/directory` user-visible. Update to `@resource/file` / `@resource/directory`.
- `plan/src/lib.rs:154` displaydoc string (above).

#### Plan files (rotate)

`examples/with-secrets.lusid`, `examples/nginx-cluster/web-server.lusid`, `examples/dotfiles/dotfiles.lusid`, `examples/arch-desktop/desktop.lusid`.

#### Docstring & doc files (rotate)

`.rs` files with `@core/` in doc comments (full grep):
- `resource/src/lib.rs` (multiple lines)
- `resource/src/resources/{file.rs, secret.rs, user.rs, systemd.rs, group.rs, podman.rs, aur.rs}`
- `params/src/{parse.rs, lib.rs}`
- `plan/src/{lib.rs, model.rs:51}`
- `secrets/src/{load.rs, lib.rs}`
- `fs/src/lib.rs`
- `lusid-apply/src/{lib.rs:193, main.rs}`
- `lusid/src/lib.rs`
- `operation/src/operations/{file.rs, directory.rs}`

`.md` files:
- `CLAUDE.md`, `AGENTS.md`, `README.md`
- `params/README.md`, `plan/README.md`, `resource/README.md`, `secrets/README.md`
- `examples/README.md`, `examples/{nginx-cluster, dotfiles, arch-desktop}/README.md`

### 2. `@operation/<id>` namespace

New `plan/src/operation.rs`:

```rust
use lusid_operation::{
    Operation,
    operations::{command::CommandOperation, systemd::SystemdOperation},
};

/// Operations exposed in `@operation/<id>`. Single source of truth — the
/// dispatcher matches against these, and the `UnsupportedOperationModuleId`
/// error message reads from here so the two never drift.
pub const AVAILABLE_OPERATION_MODULES: &[&str] = &["command", "systemd"];

pub fn is_operation_module(module: &Spanned<String>) -> Option<&str> {
    module.inner().strip_prefix("@operation/")
}

pub fn operation_module(
    operation_module_id: &str,
    params: Option<Spanned<Value>>,
    span: Span,
) -> Result<Operation, PlanItemToResourceError> {
    match operation_module_id {
        "command" => parse::<CommandOperation>(params).map(Operation::Command),
        "systemd" => parse::<SystemdOperation>(params).map(Operation::Systemd),
        other => Err(PlanItemToResourceError::UnsupportedOperationModuleId {
            id: other.to_string(),
            available: AVAILABLE_OPERATION_MODULES.join(", "),
            span,
        }),
    }
}

fn parse<O: ParseParams>(
    params: Option<Spanned<Value>>,
) -> Result<O, PlanItemToResourceError> {
    let value = params.ok_or(PlanItemToResourceError::MissingParams)?;
    O::parse_params(value).map_err(PlanItemToResourceError::Parse)
}
```

`plan/src/lib.rs` adds `mod operation;` next to `mod resource;`.

**`lusid-plan` already depends on `lusid-operation`** (verified at `plan/Cargo.toml:9`). No Cargo.toml change for the plan crate.

### 3. Operation parsing (`ParseParams` impls)

**Add `lusid-params` as a dep of `lusid-operation`** in `operation/Cargo.toml`. Verified no cycle: `params/Cargo.toml` deps are `rimu`, `rimu-interop`, `displaydoc`, `indexmap`, `serde`, `thiserror`, `tracing` — no `lusid-operation` reference.

`operation/src/operations/command.rs`:

```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CommandExecutor { Direct, Shell }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CommandOperation {
    pub command: String,
    pub executor: CommandExecutor,
}

impl ParseParams for CommandOperation {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let command = fields.required_string("command")?;
        let executor = match fields.optional_string("executor")?.as_deref() {
            None | Some("shell") => CommandExecutor::Shell,
            Some("direct") => CommandExecutor::Direct,
            Some(other) => return Err(/* invalid executor span'd error */),
        };
        fields.finish()?;
        Ok(CommandOperation { command, executor })
    }
}
```

`operation/src/operations/systemd.rs`:

```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SystemdOperation {
    Enable { name: String, user: bool },
    Disable { name: String, user: bool },
    Start { name: String, user: bool },
    Stop { name: String, user: bool },
    Restart { name: String, user: bool },    // new
    Reload { name: String, user: bool },     // new
}

// Display impl at lines 22-27 — extend match with Restart/Reload arms.
// apply impl at lines 65-70 — extend match: ("restart", name, *user), ("reload", name, *user).

impl ParseParams for SystemdOperation {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let action = fields.take_discriminator("action",
            &["enable", "disable", "start", "stop", "restart", "reload"])?;
        let name = fields.required_string("name")?;
        let user = fields.optional_bool("user")?.unwrap_or(false);
        fields.finish()?;
        Ok(match action {
            "enable" => SystemdOperation::Enable { name, user },
            "disable" => SystemdOperation::Disable { name, user },
            "start" => SystemdOperation::Start { name, user },
            "stop" => SystemdOperation::Stop { name, user },
            "restart" => SystemdOperation::Restart { name, user },
            "reload" => SystemdOperation::Reload { name, user },
            _ => unreachable!(),
        })
    }
}
```

### 4. `PlanItem` and `InlineOperation`

`plan/src/model.rs`:

```rust
pub struct PlanItem {
    pub id: Option<Spanned<String>>,
    pub module: Spanned<String>,
    pub params: Option<Spanned<Value>>,
    pub requires: Vec<Spanned<String>>,
    pub required_by: Vec<Spanned<String>>,
    pub on_change: Vec<Spanned<InlineOperation>>,   // new
}

pub struct InlineOperation {
    pub module: Spanned<String>,
    pub params: Option<Spanned<Value>>,
}
```

The struct-literal at `plan/src/model.rs:165-171` (`Ok(PlanItem { ... })`) and the destructure at `plan/src/lib.rs:172-178` (`let crate::model::PlanItem { ... }`) must add `on_change`.

New `IntoPlanItemError` variants (span-carrying per convention at `plan/src/model.rs:62-79`):

```rust
/// Property "on_change" must be a list
OnChangeNotAList { span: Span },

/// "on_change" list item must be an object
OnChangeItemNotAnObject { item_span: Span },

/// "on_change" item is missing required "module" property
OnChangeItemModuleMissing { item_span: Span },

/// "on_change" item "module" must be a string
OnChangeItemModuleNotAString { item_span: Span },

/// "on_change" items cannot declare "id" — hooks are anonymous in v1 and \
/// cannot be referenced from elsewhere. If you need a named, reusable action, \
/// declare a separate `@resource/command` with an `is_installed` probe.
InlineOperationHasId { span: Span },

/// "on_change" items cannot declare "requires" — handlers run after the \
/// resource they're attached to. To order one hook before another, combine \
/// them into a single shell operation, or attach the second hook to a \
/// downstream resource.
InlineOperationHasRequires { span: Span },

/// "on_change" items cannot declare "required_by" — see InlineOperationHasRequires
InlineOperationHasRequiredBy { span: Span },
```

`FromRimu for PlanItem` parses `on_change` (empty list when absent). `FromRimu for InlineOperation` rejects `id`/`requires`/`required_by`. `on_change: []` is parsed as `Vec::new()`, semantically equivalent to omitting.

#### `PlanItemToResourceError` — span fields convention

The existing variants have no spans. v3 introduces span-carrying variants for the new errors. Three options:

(a) Add spans to old variants too (clean, broad change).
(b) Leave old variants spanless, only new variants get spans (asymmetric).
(c) Wrap each new variant in `Spanned<...>` at the call site rather than embedding `span` in the variant.

**Decision: (b).** Existing variants surface at coarse points (e.g. "missing params"); old vs new asymmetry is acceptable. Document with a single comment on the enum.

### 5. `PlanMeta` becomes a struct

`plan/src/tree.rs`:

```rust
// was: pub type PlanMeta = CausalityMeta<PlanNodeId>;
#[derive(Debug, Clone, Default)]
pub struct PlanMeta {
    pub id: Option<PlanNodeId>,
    pub requires: Vec<PlanNodeId>,
    pub required_by: Vec<PlanNodeId>,
    pub handlers: Vec<Operation>,
}

impl PlanMeta {
    /// Drop handlers to produce the causality view of this meta. Used at every
    /// site that converts a `PlanTree` to a `CausalityTree`, so the lossy drop
    /// is visible at the call site (not buried in `From`).
    pub fn to_causality(self) -> CausalityMeta<PlanNodeId> {
        CausalityMeta { id: self.id, requires: self.requires, required_by: self.required_by }
    }
}
```

**No `From<PlanMeta> for CausalityMeta<PlanNodeId>` impl** — only the explicit `to_causality()` method.

#### Struct-literal sites that must add `handlers: vec![]` (or `..PlanMeta::default()`)

- `plan/src/lib.rs:92` — `PlanMeta::default()` (covered by `derive(Default)`).
- `plan/src/lib.rs:204` — `PlanMeta { id, requires, required_by }` in `plan_item_to_resource`'s Leaf branch.
- `plan/src/lib.rs:218` — same in the Branch branch.
- `plan/src/tree.rs:35` — `map_plan_subitems` constructs `CausalityMeta { ... }` for the inner type; not affected (it constructs `CausalityMeta<PlanNodeId>`, not `PlanMeta`). Verify by reading.

#### Conversion sites — `FlatTree → Tree → map_meta → CausalityTree`

`FlatTree` has no `map_meta` method (`tree/src/lib.rs:144-238`). The existing six sites can be updated in one of two ways:

(a) **Add `map_meta` to `FlatTree`** (`tree/src/lib.rs`) so it mirrors `Tree::map_meta` at line 100. This is a small additive change to the tree crate (~10 lines).

(b) **Round-trip Tree↔FlatTree at each site.** `CausalityTree::from(Tree::from(flat).map_meta(PlanMeta::to_causality))`.

**Decision: (a).** Adding `map_meta` to `FlatTree` is cleaner and useful beyond this change. The implementation is identical to `Tree::map_meta` — walk the arena and rebuild meta.

Affected sites in `lusid-apply/src/lib.rs`:

```rust
// Line 189 — FlatTree::from of plan output. Stays as-is (FlatTree<X, PlanMeta> for the pipeline).
let resource_params = FlatTree::from(resource_params);

// Lines 215, 240, 259, 293 — debug log emissions.
// Update to: CausalityTree::from(x.clone()).map_meta(PlanMeta::to_causality)
debug!("Resources: {:?}", CausalityTree::from(resources.clone()).map_meta(PlanMeta::to_causality));

// Line 297 — load-bearing for compute_epochs.
// First convert FlatTree → Tree, run inject_handlers (operates on Tree), then map_meta.
let tree = Tree::from(operations);
let tree = inject_handlers(tree);
let causality_tree = tree.map_meta(PlanMeta::to_causality);
let operation_epochs = compute_epochs(causality_tree)?;
```

#### Handler-survival invariant (new documentation contract)

The `map_tree` calls at `lusid-apply/src/lib.rs:205` and `:274` pass `meta` straight through to `PlanTree::branch(meta, ...)`. This is what preserves `meta.handlers` from the plan-item leaf through resource → state → change → operations expansion. Document on `PlanMeta`:

> **Invariant.** `meta.handlers` must be preserved through all pipeline `map_tree` calls (i.e., when a leaf is expanded into a branch, the produced branch keeps the original `meta`). The branch-level `inject_handlers` pass relies on this.

### 6. `plan_item_to_resource` — parse, validate, attach

`plan/src/lib.rs:164-226`:

```rust
async fn plan_item_to_resource(...) -> Result<PlanTree<ResourceParams>, PlanItemToResourceError> {
    let (plan_item, _span) = plan_item.take();
    let crate::model::PlanItem {
        id: item_id,
        ref module,
        params: params_value,
        requires,
        required_by,
        on_change,    // new
    } = plan_item;

    // Legacy prefix hint — fires before anything else.
    if let Some(id) = module.inner().strip_prefix("@core/") {
        return Err(PlanItemToResourceError::LegacyCorePrefix {
            id: id.to_string(),
            span: module.span(),
        });
    }

    // Reject @operation/ as top-level.
    if let Some(op_id) = is_operation_module(module) {
        return Err(PlanItemToResourceError::OperationModuleAsTopLevel {
            id: op_id.to_string(),
            span: module.span(),
        });
    }

    // Parse on_change handlers (rejected on non-resource items).
    let handlers = if !on_change.is_empty() {
        if is_resource_module(module).is_none() {
            return Err(PlanItemToResourceError::OnChangeOnNonResource {
                module: module.inner().to_string(),
                span: module.span(),
            });
        }
        parse_on_change(on_change)?
    } else {
        Vec::new()
    };

    let id = item_id.map(|id| PlanNodeId::PlanItem { ... });
    let requires = ...;  // unchanged
    let required_by = ...;  // unchanged

    if let Some(resource_module_id) = is_resource_module(module) {
        let params = resource_module(resource_module_id, params_value)?;
        Ok(PlanTree::Leaf {
            meta: PlanMeta { id, requires, required_by, handlers },
            node: params,
        })
    } else {
        // Nested plan path. Handlers already rejected above.
        let path = PathBuf::from(module.inner());
        let plan_id = current_plan_id.join(path);
        let children = plan_recursive(plan_id, params_value, ctx, store, system)
            .await
            .map_err(Box::new)?;
        Ok(PlanTree::Branch {
            meta: PlanMeta { id, requires, required_by, handlers: vec![] },
            children,
        })
    }
}

fn parse_on_change(
    items: Vec<Spanned<InlineOperation>>,
) -> Result<Vec<Operation>, PlanItemToResourceError> {
    items.into_iter().map(|item| {
        let (op, span) = item.take();
        let module_id = is_operation_module(&op.module)
            .ok_or_else(|| PlanItemToResourceError::OnChangeItemModuleNotAnOperation {
                module: op.module.inner().to_string(),
                span: op.module.span(),
            })?;
        operation_module(module_id, op.params, span)
    }).collect()
}
```

New `PlanItemToResourceError` variants (with `displaydoc::Display`):

```rust
/// `@core/` was renamed to `@resource/` — try `@resource/{id}`
LegacyCorePrefix { id: String, span: Span },

/// operations cannot appear at the top level — `@operation/{id}` is only \
/// valid inside `on_change`. To run an action when a resource changes, \
/// attach it via `on_change` on the relevant `@resource/*`. For idempotent \
/// imperative actions at the top level, see `@resource/command`.
OperationModuleAsTopLevel { id: String, span: Span },

/// `on_change` is only valid on `@resource/*` plan items, got `{module}`
OnChangeOnNonResource { module: String, span: Span },

/// `on_change` items must be `@operation/<id>`, got `{module}`. \
/// Resources describe desired state; operations describe imperative actions.
OnChangeItemModuleNotAnOperation { module: String, span: Span },

/// unknown @operation/ module: `{id}`. Available: {available}
UnsupportedOperationModuleId { id: String, available: String, span: Span },
```

### 7. `inject_handlers` — branch-level post-pass

Lives in `plan/src/tree.rs` (or new `plan/src/handlers.rs`). Operates on `Tree<Option<Operation>, PlanMeta>`, NOT `FlatTree`.

```rust
const HANDLER_ANCHOR: &str = "@@handler-anchor";

pub fn inject_handlers(
    tree: Tree<Option<Operation>, PlanMeta>,
) -> Tree<Option<Operation>, PlanMeta> {
    match tree {
        Tree::Leaf { meta, node } => Tree::Leaf { meta, node },
        Tree::Branch { meta, children } => {
            let children: Vec<_> = children.into_iter().map(inject_handlers).collect();
            if meta.handlers.is_empty() || !has_any_change(&children) {
                Tree::Branch { meta, children }
            } else {
                wrap_with_handler_structure(meta, children)
            }
        }
    }
}

fn has_any_change(children: &[Tree<Option<Operation>, PlanMeta>]) -> bool {
    children.iter().any(|t| match t {
        Tree::Leaf { node, .. } => node.is_some(),
        Tree::Branch { children, .. } => has_any_change(children),
    })
}

fn wrap_with_handler_structure(
    branch_meta: PlanMeta,
    resource_children: Vec<Tree<Option<Operation>, PlanMeta>>,
) -> Tree<Option<Operation>, PlanMeta> {
    let scope_id = cuid2::create_id();
    let anchor_id = PlanNodeId::SubItem {
        scope_id: scope_id.clone(),
        item_id: HANDLER_ANCHOR.to_string(),
    };

    let main_branch = Tree::Branch {
        meta: PlanMeta {
            id: Some(anchor_id.clone()),
            ..PlanMeta::default()
        },
        children: resource_children,
    };

    let handler_leaves: Vec<_> = branch_meta.handlers.iter().cloned().map(|op| {
        Tree::Leaf {
            meta: PlanMeta {
                requires: vec![anchor_id.clone()],
                ..PlanMeta::default()
            },
            node: Some(op),
        }
    }).collect();

    let outer_meta = PlanMeta { handlers: vec![], ..branch_meta };
    let mut all = vec![main_branch];
    all.extend(handler_leaves);
    Tree::Branch { meta: outer_meta, children: all }
}
```

#### Reserved-id assertion — moved to source

Replace the dead `debug_assert!` inside `wrap_with_handler_structure` with a guard at the *source* of resource-emitted ids, in `map_plan_subitems` (`plan/src/tree.rs:25-58`). The closure argument's `meta` is `CausalityMeta<String>` — `meta.id`, `meta.requires`, `meta.required_by` are all plain `String`s emitted by resources:

```rust
pub fn map_plan_subitems<...>(...) -> ... {
    let scope_id = create_id();
    map(node).into_iter().map(move |tree| {
        tree.map_meta(|meta| {
            // All three string fields are guarded — a resource is forbidden to
            // emit or reference `@@`-prefixed ids; that prefix is reserved for
            // synthetic ids minted by the plan layer (`@@handler-anchor`).
            if let Some(ref item_id) = meta.id {
                debug_assert!(!item_id.starts_with("@@"),
                    "resource emitted reserved intra-scope id: {item_id}");
            }
            for r in &meta.requires {
                debug_assert!(!r.starts_with("@@"),
                    "resource emitted reserved intra-scope requires: {r}");
            }
            for r in &meta.required_by {
                debug_assert!(!r.starts_with("@@"),
                    "resource emitted reserved intra-scope required_by: {r}");
            }
            CausalityMeta { ... }  // existing logic
        })
    })
}
```

Document in `ResourceType` trait docs (`resource/src/lib.rs:63-108`):

> Resource atom ids (the `String` keys in `CausalityMeta`) **must not start with `@@`** — this prefix is reserved for synthetic ids minted by the plan layer (e.g. the handler-anchor). Enforced by `debug_assert!` in `map_plan_subitems`.

#### Transitive scheduling effect — documented contract

The outer branch retains the plan-item's original id (via `PlanMeta::id`). Per causality semantics (`causality/src/epoch.rs:91-100,143-147`), a branch id registers every descendant leaf for dependency lookup — including handler leaves. So a plan item that `requires: ["nginx-conf"]` waits for `nginx-conf`'s handlers too. Document in `inject_handlers` docs and the `on_change` AGENTS.md section.

### 8. Identity dedup in `merge`

`operation/src/operations/systemd.rs`:

```rust
fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation> {
    use std::collections::HashSet;
    let mut seen = HashSet::new();
    let mut out = Vec::with_capacity(operations.len());
    for op in operations {
        if seen.insert(op.clone()) {
            out.push(op);
        }
    }
    out
}
```

Same shape for `Command::merge`. Both rely on the new `PartialEq+Eq+Hash` derives.

Update the existing `Note(cc):` comment at `operation/src/operations/systemd.rs:48-51`:

```
// Note(cc): merge dedups identical (verb, name, user) tuples to coalesce
// install-hook fan-out (e.g. ten files all on_change-trigger `systemd reload nginx`
// in the same epoch collapse to one reload). Safe because enable/start/stop/
// restart/reload are idempotent. Cross-epoch dedup is not handled here — see
// AGENTS.md "v1 hook limitations".
```

### 9. Documented v1 limitations (AGENTS.md)

These are real but deferred to post-v1. Documented (not runtime-enforced) so users have a place to consult.

1. **Failure mode**: if a hook fails, the apply aborts. The resource that triggered it is now in its target state, so a re-run will NOT re-trigger the hook. To recover: run the failing operation manually, or temporarily mutate the resource to force a state diff.

2. **Cross-epoch coalescing**: if two resources both declare the same hook (e.g. both reload nginx) and they're scheduled in different epochs of the apply because of their dependencies, the hook runs once per epoch. To collapse: keep the resources at the same dependency depth, or factor the hook into a single dedicated resource downstream. (Duplicated in the AGENTS.md limitations list at §10 with a worked example; the §10 phrasing is the user-facing canonical version.)

3. **No visual distinction for hooks in the TUI**: a `Systemd::Reload(nginx)` from `on_change` renders identically to a deliberate top-level operation.

4. **No top-level `@operation/*`**: imperative actions at the top level are deliberately not supported. Use `@resource/command` with an `is_installed` probe for idempotent actions; use `on_change` for change-driven actions. Note(cc): preventing non-convergent plans (Salt's `cmd.run` trap).

5. **No `if:` predicate on inline hooks**: hooks are consequences of resource changes, not independently-gated actions. If a hook needs to check state, the *state* should be a separate resource and the hook should fire on its change.

### 10. Migration of examples and docs

`examples/nginx-cluster/web-server.lusid` — extend to use `on_change`:

```rimu
- module: "@resource/command"
  id: "publish-index"
  requires: ["install-nginx"]
  params:
    status: "install"
    is_installed: "grep -qF \"" + params.greeting + "\" /var/www/html/index.html"
    install: "printf '<!doctype html><html>...</html>' \"" + params.greeting + "\" | sudo -n tee /var/www/html/index.html > /dev/null"
  # When `greeting` changes the index will be rewritten and `on_change` fires.
  # The hook fires only when the resource produces a state diff — re-running
  # with the same greeting is a no-op.
  on_change:
    - module: "@operation/systemd"
      params: { name: "nginx", action: "reload" }

- module: "@resource/systemd"
  requires: ["install-nginx"]
  params: { name: "nginx", enabled: true, active: true }
```

`AGENTS.md` — add new section "Resources, operations, and `on_change` hooks":

> **Resources vs. operations.**
> - A **resource** (`@resource/<id>`) describes *desired state* — "nginx should be enabled and active". Lusid probes current state, computes a diff, and converges. Idempotent across re-applies.
> - An **operation** (`@operation/<id>`) describes an *imperative action* — "reload nginx", "run this command". Operations are not state-checked; they run when triggered.
>
> Resources live at the top level of `setup`. Operations live only inside an `on_change` block.
>
> **`on_change` hooks.** A resource may declare a list of operations to run when it changes. Hooks fire when the resource has any change to apply (new file contents, different mode, owner change, etc.). They run in a strictly-later epoch than every one of the resource's own operations — `inject_handlers` wraps the resource's children in an anchor sub-branch and gives each handler `requires: [anchor_id]`, so per causality's branch-as-group semantics the handler waits for every resource-side leaf. Identical hooks coalesce within that handler epoch — if ten resources in the same epoch each `on_change: reload nginx`, their hooks all land in the next epoch and merge dedup collapses them to one reload.
>
> ```rimu
> - module: "@resource/file"
>   params: { path: "/etc/nginx/nginx.conf", source: "./nginx.conf", state: "sourced" }
>   on_change:
>     - module: "@operation/systemd"
>       params: { name: "nginx", action: "reload" }
> ```
>
> A plan item's `id` registers all of its hooks too: if another plan item declares `requires: [<id>]`, it waits for both the resource and its hooks before running. Dependents see the hook's effect, not just the resource's state.
>
> **v1 limitations.**
> - Hooks are inline only — no by-reference (`on_change: ["handler-id"]`).
> - Inline operations cannot declare `id`, `requires`, or `required_by`.
> - Triggered on any change — no add/modify/remove distinction.
> - **Cross-epoch coalescing not handled.** If resource A reloads nginx, resource B also reloads nginx, and B `requires: ["A"]` (so they're in different epochs), nginx reloads twice. Workaround: factor the reload into a single dedicated `@resource/command` downstream, or accept the duplicate (nginx reload is idempotent).
> - **Hook failure leaves you stuck.** If a hook fails, apply aborts. The resource is now in its target state, so re-applying will NOT re-trigger the hook. Recovery: either run the operation manually (e.g. `sudo systemctl reload nginx`), or briefly toggle a field on the resource (e.g. change `mode` on a `@resource/file`, or `enabled` on a `@resource/systemd`) and re-apply, then revert.
> - **`@operation/command` covers a lot.** Although only `command` and `systemd` are exposed as operations in v1, `@operation/command` shells out — logrotate signals, cron reloads, cache invalidation, etc. all fit under it.

### 11. Tests

#### Plan tests (`plan/tests/`)

- `on_change` parses; handlers land on leaf `PlanMeta::handlers`.
- `on_change` empty list (`[]`) is equivalent to omitting the field.
- `on_change: null` — what error? (Probably `OnChangeNotAList`.)
- `on_change` on a nested-plan item → `OnChangeOnNonResource`.
- `on_change` with a `@resource/...` entry → `OnChangeItemModuleNotAnOperation`.
- `on_change` entry with `id` → `InlineOperationHasId`.
- `on_change` entry with `requires` → `InlineOperationHasRequires`.
- `@operation/...` at top level → `OperationModuleAsTopLevel`.
- Legacy `@core/...` at top level → `LegacyCorePrefix`.
- Unknown `@operation/<x>` → `UnsupportedOperationModuleId` with "Available: command, systemd".
- Inline operation with `params: null` for a required field → spanned parse error.

#### Operation tests

- `Systemd::merge` dedups identical `Restart`s (and `Reload`s, `Start`s, etc.).
- `Command::merge` dedups identical `(command, executor)` pairs.
- `SystemdOperation` parses each action variant correctly; unknown action fails.

#### Causality test (new)

- Build a `Tree<Option<Operation>, PlanMeta>` with a four-leaf "file" expansion (file/mode/user/group, mode requires file, etc.) + one handler. Run `inject_handlers`. Assert the wrapped structure has the synthetic anchor branch with the four atoms inside, plus the handler leaf at the outer level with `requires` pointing at the anchor. Run `compute_epochs` on the result. Assert the handler leaf lands in a strictly-later epoch than every atom.

#### End-to-end (in `lusid-apply` or integration)

- Plan with file write + `on_change` systemd reload. No change → no reload. Change → exactly one reload. Two sibling resources with same `on_change` → one reload (per-epoch dedup). Two cascaded resources with same `on_change` → two reloads (documented limitation).

## What's deliberately *not* in v1

- By-reference handlers, change-kind filtering, pre-remove hooks, top-level `@operation/*`, inline `if:` predicates, cross-epoch handler coalescing, TUI distinction between main and handler ops, persistent pending-handler queue, File/Git/User/Group as `@operation/...` types.

## Convergence status

After three review rounds, v3 (+ patches above) addresses all blocker-level feedback. Remaining items deferred to implementation time:

- Tests for edge cases like `params: null` and `on_change: null` (v3 §11 lists; behavior decided at implementation, not in this design doc).
- `Tree::map_meta`'s `+ Copy` bound on the `MapFn` — `PlanMeta::to_causality` is a zero-sized fn item, so satisfies it; flag if it later needs to capture state.
- `inject_handlers` could fold `has_any_change` into its recursion to avoid a second traversal — cosmetic, not blocking.
- A final-epoch fold for cross-epoch hook coalescing is *not* in v1; it would change the causality contract (handlers no longer transitively block dependents). Documented as a v1 limitation; revisit if real plans hit it.

## Open question (still)

**`@operation/<id>` exposed in v1**: just Command and Systemd. `@operation/command` shells out so the practical surface is wide. Defer File/Git/User/Group/Aur/etc. until a concrete user need surfaces.
