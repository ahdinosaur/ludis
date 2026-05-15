# lusid-params

Parameter schemas, validation, and typed parsing for lusid plans.

## Components

- **Schema** (`ParamType`, `ParamField`, `ParamTypes`) - what shape of value is accepted. `ParamTypes` is either a `Struct` or a `Union` of struct cases. Parsed from the plan's Rimu source via `FromRimu`.
- **`validate()`** - checks user-supplied values against a plan's schema *and* coerces string-shaped paths into typed `Value::HostPath` / `Value::TargetPath` before forwarding to `setup`. Unions use first-match (declaration order).
- **Parser** (`ParseParams`, `StructFields`, the `parse_*` helpers) - resource-boundary, one-pass conversion from `Spanned<Value>` to a typed `Params` struct. Each `@resource/<id>` resource implements `ParseParams`.

## Path types

- `HostPath` - local-machine path. Accepts a typed `Value::HostPath` or a relative `Value::String`; strings are rewritten to a typed `Value::HostPath` resolved against the value's span source (or `ParamsContext::root_path` if the span has none - e.g. CLI-supplied `--params`).
- `TargetPath` - absolute path on the managed host. Accepts a typed `Value::TargetPath` or an absolute `Value::String` (no resolution - target paths live on the managed host).

## Spans

Schemas, values, and errors carry `Spanned<T>` so diagnostics point at the offending plan line. When adding error variants, prefer `Spanned<Error>` over bare `Error`.
