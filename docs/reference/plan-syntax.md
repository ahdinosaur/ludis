# Plan syntax

Plans are written in [Rimu](https://rimu.dev), a small expression-oriented language. This page covers the shape lusid expects a plan to have; for the full Rimu language, see [rimu.dev](https://rimu.dev).

## Top-level shape

```yaml
name: "my-plan"
version: "0.1.0"

params:
  greeting:
    type: "string"

setup: (params, system) =>
  - module: "@resource/file"
    params:
      state: "sourced"
      source: "./hello.txt"
      path: "/etc/hello.txt"
```

A plan is a single Rimu object. Three top-level keys:

| Key | Type | Required | What |
| --- | --- | --- | --- |
| `name` | string | yes | Identifying name. |
| `version` | string | yes | Version, like a Cargo manifest. |
| `params` | object or list (schema) | optional | Schema for params the plan accepts. See [param schema](#param-schema). |
| `setup` | function `(params, system) => [items]` | yes | Returns the list of plan items. |

## Plan items

Each item in `setup`'s output list is an object:

```yaml
- module: "@resource/apt"      # or "@operation/..." (inside on_change) or "./nested.lusid"
  id: "install-nginx"          # optional; identifier other items can `requires:` against
  params:                      # optional; required if the module expects them
    package: "nginx"
  requires:                    # optional; ids of items this depends on
    - "configure-firewall"
  required_by:                 # optional; inverse of requires
    - "start-nginx"
  on_change:                   # optional; only on @resource/* items
    - module: "@operation/systemd"
      params: { name: "nginx", action: "reload" }
```

| Field | Where it's allowed |
| --- | --- |
| `module` | every item |
| `id` | every item except those inside `on_change` |
| `params` | every item, if the module accepts them |
| `requires` | every item except those inside `on_change` |
| `required_by` | every item except those inside `on_change` |
| `on_change` | only on `@resource/*` items |

## Module forms

| Prefix | Resolution |
| --- | --- |
| `@resource/<id>` | Built-in resource. See [resources reference](./resources.md). |
| `@operation/<id>` | Built-in operation. Only valid inside `on_change`. See [operations](../concepts/operations.md). |
| `./path.lusid` (or any other string) | Treated as a path relative to the current plan file. Loaded and planned recursively as a nested plan. |

## Param schema

The `params:` block declares what params a plan accepts. Each field has a `type:` and an optional `optional: true`.

### Type vocabulary

| `type` | Accepts | Notes |
| --- | --- | --- |
| `"boolean"` | `true` / `false` | |
| `"string"` | `"..."` | |
| `"number"` | integers, floats | |
| `"list"` | `[a, b, c]` | Requires `item: { type: ... }` for the element type. |
| `"object"` | `{ k: v }` | Requires `value: { type: ... }` for the value type; keys are always strings. |
| `"host-path"` | path string | Path on the operator's machine. Relative strings are resolved against the plan's source dir. |
| `"target-path"` | absolute path string | Path on the managed host. Must be absolute. |

### Examples

```yaml
params:
  enable:
    type: "boolean"

  greeting:
    type: "string"
    optional: true

  packages:
    type: "list"
    item:
      type: "string"

  ports:
    type: "object"
    value:
      type: "number"

  config_dir:
    type: "host-path"   # e.g. "./configs/" - resolved relative to this plan file

  install_path:
    type: "target-path" # must be absolute, e.g. "/etc/myapp"
```

### Union schemas

Instead of a single struct, `params:` can be a list - a union of structs, tried in declaration order:

```yaml
params:
  - mode:
      type: "string"
    packages:
      type: "list"
      item:
        type: "string"
  - mode:
      type: "string"
    package:
      type: "string"
```

The first case that matches wins. Use this when the same plan accepts a few shapes (`{ packages: [...] }` *or* `{ package: "..." }`).

## Rimu features useful in plans

- **String concatenation**: `system.user.home + "/.zshrc"`.
- **Conditional items**: standard list-comprehension and conditional value expressions.
- **List building**: comprehensions over `params.packages` to expand to per-package items.

See the [Rimu docs](https://rimu.dev) for the full language.

## See also

- [Plans concept](../concepts/plans.md) - narrative introduction.
- [Resources reference](./resources.md) - params for every `@resource/*`.
