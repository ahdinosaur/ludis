# lusid-params

Parameter schemas and validation for lusid plans.

Every plan declares a `params` schema. This crate defines the schema types,
the plan-boundary validator, and the typed parser used at the resource
boundary.

## The two halves

- **Schema (`ParamType`, `ParamField`, `ParamTypes`)** — what shape of value is
  accepted. `ParamTypes` is either a single `Struct` or a `Union` of struct
  cases. Parsed from the plan's Rimu source via `FromRimu`.
- **`validate()`** — checks a Rimu value object against a plan's schema *and*
  coerces string-shaped paths into Rimu's typed `Value::HostPath` /
  `Value::TargetPath` variants before forwarding to `setup`. For unions,
  first-match wins (cases tried in declaration order).
- **Parser (`ParseParams`, `StructFields`, the `parse_*` helpers)** —
  resource-boundary one-pass conversion from `Spanned<Value>` to a typed
  `Params` struct. Each `@resource/<id>` resource implements `ParseParams` for
  its `Params` type.

## Path-type conventions

- `HostPath` — a path on the local machine. `validate` accepts either a typed
  `Value::HostPath` or a relative `Value::String`, and rewrites the string
  into a typed `Value::HostPath` resolved against the value's span source (or
  `ParamsContext::root_path` when the span has no real source — e.g.
  CLI-supplied `--params`).
- `TargetPath` — an absolute path on the managed host. Accepts a typed
  `Value::TargetPath` or an absolute `Value::String`; the string is wrapped
  as a `Value::TargetPath` (no resolution — target paths live on the managed
  host, not the local filesystem).

## Spans

Schemas, values, and errors carry `Spanned<T>` so diagnostics can point at the
exact plan line. Preserve spans when adding new variants or errors — it's the
whole point of the rich error enums here.
