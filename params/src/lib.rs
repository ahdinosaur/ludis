//! Parameter schemas, validation, and typed parsing. See the crate README
//! for components, path-type conventions, and union semantics.
//!
//! Spans are load-bearing: schemas, values, and errors are all `Spanned<T>`
//! so diagnostics can point at the offending plan line. When adding a new
//! type or error variant, keep the span all the way through.

pub mod parse;

pub use crate::parse::{
    ParseError, ParseParams, StructFields, parse_bool, parse_host_path, parse_list, parse_number,
    parse_string, parse_target_path, parse_u32,
};

use std::path::{Path, PathBuf};

use displaydoc::Display;
use indexmap::IndexMap;
use rimu::{Span, Spanned, Value, ValueObject};
use rimu_interop::FromRimu;
use thiserror::Error;

/// Coercion context for [`validate`].
///
/// Carries the **fallback root path** used to resolve relative `host-path` strings
/// whose value span doesn't point at a real source file — e.g. CLI-supplied
/// `--params` JSON, where the `SourceId` is empty. Strings whose span carries a
/// real `.lusid` source resolve against that file's parent directory instead,
/// so a literal `"./rel"` written in a plan resolves the way the plan author
/// wrote it — independent of which plan's `validate` happens to run.
///
/// Sub-plans share their parent's `ParamsContext`. By the time forwarded values
/// reach a sub-plan they're already typed (`Value::HostPath`) — `validate`
/// rewrote them at the parent boundary — so the root path is only consulted for
/// literal strings that arrive at the sub-plan boundary. That makes parent →
/// child forwarding behave consistently regardless of how deep the recursion
/// runs.
///
/// `forbid_cli_relative_host_paths` rejects a relative `host-path` string with
/// an empty span source rather than synthesising a fallback path. Set this on
/// guest-side `lusid-apply` runs (`remote apply` / `dev apply`): the operator's
/// root path is meaningless on the target, so silently resolving against it
/// would produce a path that doesn't exist and surface as a confusing
/// "host-path not found" later in the pipeline.
///
/// TODO(cc): a future remote/dev apply could detect operator-side
/// `host-path` strings on the host, upload the referenced bytes alongside
/// the plan, and rewrite the params JSON to point at the guest copy
/// before exec. That removes the restriction here. Until then we
/// fail loudly at the boundary.
#[derive(Debug, Clone)]
pub struct ParamsContext {
    root_path: PathBuf,
    forbid_cli_relative_host_paths: bool,
}

impl ParamsContext {
    pub fn new(root_path: impl Into<PathBuf>) -> Self {
        Self {
            root_path: root_path.into(),
            forbid_cli_relative_host_paths: false,
        }
    }

    /// Reject relative `host-path` strings whose span has no real source
    /// (i.e. CLI-supplied `--params`). See [`ParamsContext`] for the
    /// motivation.
    pub fn forbid_cli_relative_host_paths(mut self) -> Self {
        self.forbid_cli_relative_host_paths = true;
        self
    }

    pub fn root_path(&self) -> &Path {
        &self.root_path
    }
}

/// Schema node: the allowed shape of a single value.
///
/// - `List` / `Object` are homogeneous containers — every element/value
///   matches the inner type.
/// - `HostPath` / `TargetPath` carry stricter semantics than `String` (see
///   the module-level docs).
#[derive(Debug, Clone)]
pub enum ParamType {
    Boolean,

    String,

    Number,

    List { item: Box<Spanned<ParamType>> },

    Object { value: Box<Spanned<ParamType>> },

    HostPath,

    TargetPath,
}

#[derive(Debug, Clone)]
pub struct ParamField {
    typ: ParamType,
    optional: bool,
}

impl ParamField {
    pub const fn new(typ: ParamType) -> Self {
        Self {
            typ,
            optional: false,
        }
    }

    pub fn with_optional(self) -> Self {
        Self {
            typ: self.typ,
            optional: true,
        }
    }

    pub fn typ(&self) -> &ParamType {
        &self.typ
    }

    pub fn optional(&self) -> &bool {
        &self.optional
    }
}

/// Ordered map of field name → field schema. `IndexMap` is deliberate — we
/// preserve declaration order for stable diagnostics and rendering.
pub type ParamsStruct = IndexMap<String, Spanned<ParamField>>;

/// Top-level schema: either a single struct, or a union of candidate structs.
#[derive(Debug, Clone)]
pub enum ParamTypes {
    /// A single object structure: keys -> fields
    Struct(ParamsStruct),

    /// A union of possible object structures. Validation tries cases in order
    /// and returns the first that matches (see module docs).
    Union(Vec<ParamsStruct>),
}

#[derive(Debug, Clone, Error, Display)]
pub enum ParamTypeFromRimuError {
    /// Expected an object for parameter type
    NotAnObject,

    /// Missing property: "type"
    HasNoType,

    /// The "type" property must be a string
    TypeNotAString { span: Span },

    /// Unknown parameter type: {0}
    UnknownType(String),

    /// List type is missing required "item" property
    ListMissingItem,

    /// Invalid "item" type in list: {0:?}
    ListItem(Box<Spanned<ParamTypeFromRimuError>>),

    /// Object type is missing required "value" property
    ObjectMissingValue,

    /// Invalid "value" type in object: {0:?}
    ObjectValue(Box<Spanned<ParamTypeFromRimuError>>),
}

impl FromRimu for ParamType {
    type Error = ParamTypeFromRimuError;

    fn from_rimu(value: Value) -> Result<Self, Self::Error> {
        let Value::Object(mut object) = value else {
            return Err(ParamTypeFromRimuError::NotAnObject);
        };

        let Some(typ) = object.get("type") else {
            return Err(ParamTypeFromRimuError::HasNoType);
        };

        let (typ, typ_span) = typ.clone().take();
        let Value::String(typ) = typ else {
            return Err(ParamTypeFromRimuError::TypeNotAString { span: typ_span });
        };

        match typ.as_str() {
            "boolean" => Ok(ParamType::Boolean),
            "string" => Ok(ParamType::String),
            "number" => Ok(ParamType::Number),
            "host-path" => Ok(ParamType::HostPath),
            "target-path" => Ok(ParamType::TargetPath),
            "list" => {
                let item = object
                    .swap_remove("item")
                    .ok_or(ParamTypeFromRimuError::ListMissingItem)?;
                let item = ParamType::from_rimu_spanned(item)
                    .map_err(|error| ParamTypeFromRimuError::ListItem(Box::new(error)))?;
                Ok(ParamType::List {
                    item: Box::new(item),
                })
            }
            "object" => {
                let value = object
                    .swap_remove("value")
                    .ok_or(ParamTypeFromRimuError::ObjectMissingValue)?;
                let value = ParamType::from_rimu_spanned(value)
                    .map_err(|error| ParamTypeFromRimuError::ObjectValue(Box::new(error)))?;
                Ok(ParamType::Object {
                    value: Box::new(value),
                })
            }
            other => Err(ParamTypeFromRimuError::UnknownType(other.to_string())),
        }
    }
}

#[derive(Debug, Clone, Error, Display)]
pub enum ParamFieldFromRimuError {
    /// Expected an object for parameter field
    NotAnObject,

    /// The "optional" property must be a boolean
    OptionalNotABoolean { span: Span },

    /// Invalid field type: {0:?}
    FieldType(#[from] ParamTypeFromRimuError),
}

impl FromRimu for ParamField {
    type Error = ParamFieldFromRimuError;

    fn from_rimu(value: Value) -> Result<Self, Self::Error> {
        let Value::Object(mut object) = value else {
            return Err(ParamFieldFromRimuError::NotAnObject);
        };

        let optional = if let Some(optional_value) = object.swap_remove("optional") {
            let (inner, span) = optional_value.take();
            match inner {
                Value::Boolean(b) => b,
                _ => {
                    return Err(ParamFieldFromRimuError::OptionalNotABoolean { span });
                }
            }
        } else {
            false
        };

        let typ = ParamType::from_rimu(Value::Object(object))?;
        Ok(ParamField { typ, optional })
    }
}

#[derive(Debug, Clone, Error, Display)]
pub enum ParamTypesFromRimuError {
    /// Expected an object (struct) or a list (union) for parameter types
    NotAnObjectOrList,

    /// Invalid struct entry for key "{key}": {error:?}
    StructEntry {
        key: String,
        error: Box<Spanned<ParamFieldFromRimuError>>,
    },

    /// Union item at index {index} is not an object
    UnionItemNotAnObject { index: usize, span: Span },

    /// Invalid union item entry for key "{key}" at index {index}: {error:?}
    UnionItemEntry {
        index: usize,
        key: String,
        error: Box<Spanned<ParamFieldFromRimuError>>,
    },
}

impl FromRimu for ParamTypes {
    type Error = ParamTypesFromRimuError;

    /// Parse a schema declaration in the plan:
    /// - An **object** defines a [`ParamTypes::Struct`] — the map of fields.
    /// - A **list** defines a [`ParamTypes::Union`] — each item is an object
    ///   defining one candidate case (first-match wins during validation).
    fn from_rimu(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Object(map) => {
                let mut out: IndexMap<String, Spanned<ParamField>> =
                    IndexMap::with_capacity(map.len());

                for (key, value) in map {
                    let field = match ParamField::from_rimu_spanned(value) {
                        Ok(field) => field,
                        Err(error) => {
                            return Err(ParamTypesFromRimuError::StructEntry {
                                key: key.clone(),
                                error: Box::new(error),
                            });
                        }
                    };
                    out.insert(key, field);
                }

                Ok(ParamTypes::Struct(out))
            }
            Value::List(items) => {
                let mut cases: Vec<IndexMap<String, Spanned<ParamField>>> =
                    Vec::with_capacity(items.len());

                for (index, spanned_item) in items.into_iter().enumerate() {
                    let (inner, span) = spanned_item.clone().take();
                    let Value::Object(case_map) = inner else {
                        return Err(ParamTypesFromRimuError::UnionItemNotAnObject { index, span });
                    };

                    let mut case_out: IndexMap<String, Spanned<ParamField>> =
                        IndexMap::with_capacity(case_map.len());

                    for (key, value) in case_map {
                        let field = match ParamField::from_rimu_spanned(value) {
                            Ok(field) => field,
                            Err(error) => {
                                return Err(ParamTypesFromRimuError::UnionItemEntry {
                                    index,
                                    key: key.clone(),
                                    error: Box::new(error),
                                });
                            }
                        };
                        case_out.insert(key, field);
                    }

                    cases.push(case_out);
                }

                Ok(ParamTypes::Union(cases))
            }
            _ => Err(ParamTypesFromRimuError::NotAnObjectOrList),
        }
    }
}

#[derive(Debug, Clone, Error, Display)]
pub enum ValidateValueError {
    /// Value does not match expected type
    TypeMismatch {
        expected_type: Box<Spanned<ParamType>>,
        got_value: Box<Spanned<Value>>,
    },

    /// Invalid list item at index {index}: {error:?}
    ListItem {
        index: usize,
        #[source]
        error: Box<ValidateValueError>,
    },

    /// Invalid object entry for key "{key}": {error:?}
    ObjectEntry {
        key: String,
        #[source]
        error: Box<ValidateValueError>,
    },

    /// Relative host-path {value:?} supplied via --params is not allowed for guest applies (operator-side paths don't exist on the target); pass an absolute path or anchor host-paths in the plan source
    HostPathRelativeFromCli { value: String },
}

#[derive(Debug, Clone, Error, Display)]
pub enum ParamValidationError {
    /// Missing required parameter "{key}"
    MissingParam {
        key: String,
        expected_type: Box<Spanned<ParamType>>,
    },

    /// Unknown parameter "{key}"
    UnknownParam {
        key: String,
        value: Box<Spanned<Value>>,
    },

    /// Invalid parameter "{key}": {error:?}
    InvalidParam {
        key: String,
        error: Box<ValidateValueError>,
    },
}

#[derive(Debug, Clone, Error, Display)]
#[displaydoc("Parameters struct did not match all fields")]
pub struct ParamsStructValidationError {
    errors: Vec<ParamValidationError>,
}

#[derive(Debug, Clone, Error, Display)]
pub enum ParamsValidationError {
    /// Parameter values without parameter types
    ValuesWithoutTypes,

    /// Parameter types without parameter values
    TypesWithoutValues,

    /// Expected an object for parameter values
    ValuesNotAnObject,

    /// Parameter struct did not match all fields: {0}
    Struct(#[from] Box<ParamsStructValidationError>),

    /// Parameter union did not match any case: {case_errors:?}
    Union {
        case_errors: Vec<ParamsStructValidationError>,
    },

    /// Parameter union type is empty
    EmptyUnion,
}

fn mismatch(typ: &Spanned<ParamType>, value: &Spanned<Value>) -> ValidateValueError {
    ValidateValueError::TypeMismatch {
        expected_type: Box::new(typ.clone()),
        got_value: Box::new(value.clone()),
    }
}

/// Pick a directory to resolve a relative path-typed string against.
///
/// Prefer the value's own span source (the `.lusid` file the literal was
/// written in) — that way a literal string keeps the same meaning whether the
/// parent or a sub-plan happens to validate it. Fall back to `ctx.root_path`
/// when the span carries no real source (CLI-supplied `--params` have an empty
/// `SourceId`) or when the source has no parent directory to anchor against.
fn coerce_origin(span: &Span, ctx: &ParamsContext) -> PathBuf {
    let source_id = span.source();
    let source = source_id.as_str();
    if source.is_empty() {
        return ctx.root_path.to_path_buf();
    }
    match Path::new(source).parent() {
        Some(parent) if !parent.as_os_str().is_empty() => parent.to_path_buf(),
        _ => ctx.root_path.to_path_buf(),
    }
}

fn coerce_type(
    param_type: &Spanned<ParamType>,
    value: Spanned<Value>,
    ctx: &ParamsContext,
) -> Result<Spanned<Value>, ValidateValueError> {
    let typ_inner = param_type.inner();
    let (value_inner, span) = value.take();

    match (typ_inner, value_inner) {
        (ParamType::Boolean, val @ Value::Boolean(_)) => Ok(Spanned::new(val, span)),

        (ParamType::String, val @ Value::String(_)) => Ok(Spanned::new(val, span)),

        (ParamType::Number, val @ Value::Number(_)) => Ok(Spanned::new(val, span)),

        // HostPath: typed values pass through unchanged; relative strings get
        // resolved against the value-span's source dir (or `ctx.root_path` if
        // the span has no real source) and re-emitted as a typed `Value::HostPath`.
        // The rewrite is what fixes parent → sub-plan forwarding: the parent's
        // validate produces a typed path, and rimu's `Environment` stores
        // typed `SpannedValue` directly, so the sub-plan never sees a string.
        (ParamType::HostPath, val @ Value::HostPath(_)) => Ok(Spanned::new(val, span)),
        (ParamType::HostPath, Value::String(s)) => {
            let path = Path::new(&s);
            if !path.is_relative() {
                return Err(mismatch(param_type, &Spanned::new(Value::String(s), span)));
            }
            // CLI-supplied params have an empty span source; on guest applies
            // (`remote apply` / `dev apply`) the operator's fallback root
            // doesn't exist on the target, so silently synthesising a path
            // there would surface as a confusing "host-path not found" later.
            // Fail at the boundary instead.
            if ctx.forbid_cli_relative_host_paths && span.source().as_str().is_empty() {
                return Err(ValidateValueError::HostPathRelativeFromCli { value: s });
            }
            let origin = coerce_origin(&span, ctx);
            Ok(Spanned::new(Value::HostPath(origin.join(path)), span))
        }

        // TargetPath: typed → pass-through; absolute string → wrap as typed.
        // No resolution needed — target paths live on the managed host.
        (ParamType::TargetPath, val @ Value::TargetPath(_)) => Ok(Spanned::new(val, span)),
        (ParamType::TargetPath, Value::String(s)) => {
            if !Path::new(&s).is_absolute() {
                return Err(mismatch(param_type, &Spanned::new(Value::String(s), span)));
            }
            Ok(Spanned::new(Value::TargetPath(s.into()), span))
        }

        (ParamType::List { item }, Value::List(items)) => {
            let mut out = Vec::with_capacity(items.len());
            for (index, item_value) in items.into_iter().enumerate() {
                match coerce_type(item, item_value, ctx) {
                    Ok(coerced) => out.push(coerced),
                    Err(error) => {
                        return Err(ValidateValueError::ListItem {
                            index,
                            error: Box::new(error),
                        });
                    }
                }
            }
            Ok(Spanned::new(Value::List(out), span))
        }

        (ParamType::Object { value: value_type }, Value::Object(map)) => {
            let mut out = ValueObject::with_capacity(map.len());
            for (key, entry_value) in map {
                match coerce_type(value_type, entry_value, ctx) {
                    Ok(coerced) => {
                        out.insert(key, coerced);
                    }
                    Err(error) => {
                        return Err(ValidateValueError::ObjectEntry {
                            key,
                            error: Box::new(error),
                        });
                    }
                }
            }
            Ok(Spanned::new(Value::Object(out), span))
        }

        (_, value_inner) => Err(mismatch(param_type, &Spanned::new(value_inner, span))),
    }
}

fn coerce_struct(
    fields: &IndexMap<String, Spanned<ParamField>>,
    mut values: ValueObject,
    span: Span,
    ctx: &ParamsContext,
) -> Result<Spanned<Value>, ParamsStructValidationError> {
    let mut errors: Vec<ParamValidationError> = Vec::new();
    let mut coerced: ValueObject = IndexMap::with_capacity(fields.len());

    // Walk the schema in declaration order: take each declared field out of
    // `values`, coerce it, and insert into `coerced`. Anything still in
    // `values` after this loop is an unknown key.
    for (key, spanned_field) in fields.iter() {
        let (field, field_span) = spanned_field.clone().take();
        let spanned_type = Spanned::new(field.typ().clone(), field_span);

        match values.swap_remove(key) {
            Some(spanned_value) => match coerce_type(&spanned_type, spanned_value, ctx) {
                Ok(coerced_value) => {
                    coerced.insert(key.clone(), coerced_value);
                }
                Err(error) => {
                    errors.push(ParamValidationError::InvalidParam {
                        key: key.clone(),
                        error: Box::new(error),
                    });
                }
            },
            None => {
                if !field.optional {
                    errors.push(ParamValidationError::MissingParam {
                        key: key.clone(),
                        expected_type: Box::new(spanned_type),
                    });
                }
            }
        }
    }

    for (key, spanned_value) in values {
        errors.push(ParamValidationError::UnknownParam {
            key,
            value: Box::new(spanned_value),
        });
    }

    if errors.is_empty() {
        Ok(Spanned::new(Value::Object(coerced), span))
    } else {
        Err(ParamsStructValidationError { errors })
    }
}

/// Validate parameter values against a plan-declared schema, returning the
/// **coerced** values.
///
/// Validation walks the schema and the value tree together. Path-typed leaves
/// (`host-path`, `target-path`) are rewritten in place: a relative
/// `Value::String` for a `host-path` becomes a `Value::HostPath` whose
/// `PathBuf` has been resolved against the appropriate origin (see
/// [`ParamsContext`]). Everything else passes through unchanged.
///
/// - `Struct` schemas must match all fields exactly (required fields present,
///   unknown fields rejected, each value the right type).
/// - `Union` schemas try cases in order and return the first that validates;
///   if none match, all per-case errors are returned together.
///
/// Resource params don't go through this — they parse straight to typed Rust
/// values via [`ParseParams`]. This function is plan-only: it catches user
/// `--params` mistakes against the plan's declared schema before `setup`
/// runs, *and* turns string-shaped paths into the typed Rimu variants so
/// downstream sub-plans see a uniform value shape.
pub fn validate(
    param_types: Option<&Spanned<ParamTypes>>,
    param_values: Option<Spanned<Value>>,
    ctx: &ParamsContext,
) -> Result<Option<Spanned<Value>>, ParamsValidationError> {
    let (param_types, param_values) = match (param_types, param_values) {
        (Some(param_types), Some(param_values)) => (param_types, param_values),
        (Some(_), None) => {
            return Err(ParamsValidationError::TypesWithoutValues);
        }
        (None, Some(_)) => {
            return Err(ParamsValidationError::ValuesWithoutTypes);
        }
        (None, None) => {
            return Ok(None);
        }
    };

    let (values_inner, values_span) = param_values.take();
    let Value::Object(values_map) = values_inner else {
        return Err(ParamsValidationError::ValuesNotAnObject);
    };

    match param_types.inner() {
        ParamTypes::Struct(map) => {
            let coerced = coerce_struct(map, values_map, values_span, ctx).map_err(Box::new)?;
            Ok(Some(coerced))
        }
        ParamTypes::Union(cases) => {
            if cases.is_empty() {
                return Err(ParamsValidationError::EmptyUnion);
            }

            // Try each case in declaration order. Each attempt consumes a
            // clone of the values map — first match wins. We can't peek the
            // discriminant cheaply because cases share the same `ValueObject`
            // shape, so the cost is N clones for an N-case union. In
            // practice unions are short (the `file` resource has three
            // cases) so this is fine.
            let mut case_errors: Vec<ParamsStructValidationError> = Vec::with_capacity(cases.len());

            for case in cases {
                match coerce_struct(case, values_map.clone(), values_span.clone(), ctx) {
                    Ok(coerced) => return Ok(Some(coerced)),
                    Err(error) => case_errors.push(error),
                }
            }

            Err(ParamsValidationError::Union { case_errors })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rimu::{Number, SourceId};

    fn ctx() -> ParamsContext {
        ParamsContext::new("/project/root")
    }

    fn empty_span() -> Span {
        Span::new(SourceId::empty(), 0, 0)
    }

    fn file_span(path: &str) -> Span {
        Span::new(SourceId::from(path.to_string()), 0, 0)
    }

    fn struct_schema(fields: Vec<(&str, ParamType, bool)>) -> Spanned<ParamTypes> {
        let mut out = ParamsStruct::new();
        for (name, ty, optional) in fields {
            let mut field = ParamField::new(ty);
            if optional {
                field = field.with_optional();
            }
            out.insert(name.to_string(), Spanned::new(field, empty_span()));
        }
        Spanned::new(ParamTypes::Struct(out), empty_span())
    }

    fn obj(entries: Vec<(&str, Value)>, span: Span) -> Spanned<Value> {
        let mut map = ValueObject::with_capacity(entries.len());
        for (k, v) in entries {
            map.insert(k.to_string(), Spanned::new(v, span.clone()));
        }
        Spanned::new(Value::Object(map), span)
    }

    fn unwrap_object(value: Spanned<Value>) -> ValueObject {
        match value.into_inner() {
            Value::Object(o) => o,
            other => panic!("expected object, got {other:?}"),
        }
    }

    #[test]
    fn returns_none_when_no_schema_and_no_values() {
        assert!(validate(None, None, &ctx()).expect("ok").is_none());
    }

    #[test]
    fn errors_when_types_present_but_no_values() {
        let schema = struct_schema(vec![("path", ParamType::HostPath, false)]);
        let err = validate(Some(&schema), None, &ctx()).unwrap_err();
        assert!(matches!(err, ParamsValidationError::TypesWithoutValues));
    }

    #[test]
    fn errors_when_values_present_but_no_types() {
        let value = obj(vec![("path", Value::String("./foo".into()))], empty_span());
        let err = validate(None, Some(value), &ctx()).unwrap_err();
        assert!(matches!(err, ParamsValidationError::ValuesWithoutTypes));
    }

    #[test]
    fn passes_through_typed_host_path() {
        let schema = struct_schema(vec![("path", ParamType::HostPath, false)]);
        let typed_path = PathBuf::from("/already/absolute");
        let value = obj(
            vec![("path", Value::HostPath(typed_path.clone()))],
            empty_span(),
        );
        let coerced = validate(Some(&schema), Some(value), &ctx())
            .expect("ok")
            .expect("some");
        let map = unwrap_object(coerced);
        match map.get("path").expect("path field").inner() {
            Value::HostPath(p) => assert_eq!(p, &typed_path),
            other => panic!("expected HostPath, got {other:?}"),
        }
    }

    #[test]
    fn coerces_relative_string_using_file_source_dir() {
        let schema = struct_schema(vec![("path", ParamType::HostPath, false)]);
        let span = file_span("/plans/foo.lusid");
        let value = obj(vec![("path", Value::String("bar".into()))], span);
        let coerced = validate(Some(&schema), Some(value), &ctx())
            .expect("ok")
            .expect("some");
        let map = unwrap_object(coerced);
        match map.get("path").expect("path field").inner() {
            Value::HostPath(p) => assert_eq!(p, &PathBuf::from("/plans/bar")),
            other => panic!("expected HostPath, got {other:?}"),
        }
    }

    #[test]
    fn coerces_relative_string_using_ctx_root_path_for_empty_source() {
        let schema = struct_schema(vec![("path", ParamType::HostPath, false)]);
        let value = obj(vec![("path", Value::String("bar".into()))], empty_span());
        let coerced = validate(Some(&schema), Some(value), &ctx())
            .expect("ok")
            .expect("some");
        let map = unwrap_object(coerced);
        match map.get("path").expect("path field").inner() {
            Value::HostPath(p) => assert_eq!(p, &PathBuf::from("/project/root/bar")),
            other => panic!("expected HostPath, got {other:?}"),
        }
    }

    #[test]
    fn rejects_absolute_string_for_host_path() {
        // Forwarded host paths arrive typed as `Value::HostPath` — an
        // absolute *string* for a `host-path` field is a bug
        // (no source-dir to anchor against, no path-typed sender to credit).
        let schema = struct_schema(vec![("path", ParamType::HostPath, false)]);
        let value = obj(
            vec![("path", Value::String("/abs/path".into()))],
            empty_span(),
        );
        let err = validate(Some(&schema), Some(value), &ctx()).unwrap_err();
        let ParamsValidationError::Struct(boxed) = err else {
            panic!("expected Struct error");
        };
        // Pin down the *cause* — `TypeMismatch` (absolute string failed
        // host-path coercion), not e.g. a structural `MissingParam`.
        match boxed.errors.first() {
            Some(ParamValidationError::InvalidParam { error, .. }) => {
                assert!(
                    matches!(error.as_ref(), ValidateValueError::TypeMismatch { .. }),
                    "expected TypeMismatch, got {error:?}"
                );
            }
            other => panic!("expected InvalidParam, got {other:?}"),
        }
    }

    #[test]
    fn forbid_cli_relative_host_paths_rejects_empty_source_relative_string() {
        // `forbid_cli_relative_host_paths` is the guest-apply guard: a
        // relative host-path with no real span source would otherwise be
        // resolved against the operator's `root_path`, which doesn't exist
        // on the target. Pin the rejection so a future relaxation is
        // intentional.
        let schema = struct_schema(vec![("path", ParamType::HostPath, false)]);
        let value = obj(vec![("path", Value::String("./foo".into()))], empty_span());
        let ctx = ctx().forbid_cli_relative_host_paths();
        let err = validate(Some(&schema), Some(value), &ctx).unwrap_err();
        let ParamsValidationError::Struct(boxed) = err else {
            panic!("expected Struct error");
        };
        match boxed.errors.first() {
            Some(ParamValidationError::InvalidParam { error, .. }) => assert!(
                matches!(
                    error.as_ref(),
                    ValidateValueError::HostPathRelativeFromCli { value } if value == "./foo"
                ),
                "expected HostPathRelativeFromCli, got {error:?}"
            ),
            other => panic!("expected InvalidParam, got {other:?}"),
        }
    }

    #[test]
    fn forbid_cli_relative_host_paths_allows_file_source_relative_string() {
        // A relative host-path written in a `.lusid` file has a real span
        // source — the guard targets only CLI-supplied (empty-source)
        // values, so plan-anchored host-paths must still coerce normally
        // even when the flag is set.
        let schema = struct_schema(vec![("path", ParamType::HostPath, false)]);
        let span = file_span("/plans/foo.lusid");
        let value = obj(vec![("path", Value::String("bar".into()))], span);
        let ctx = ctx().forbid_cli_relative_host_paths();
        let coerced = validate(Some(&schema), Some(value), &ctx)
            .expect("ok")
            .expect("some");
        let map = unwrap_object(coerced);
        match map.get("path").expect("path field").inner() {
            Value::HostPath(p) => assert_eq!(p, &PathBuf::from("/plans/bar")),
            other => panic!("expected HostPath, got {other:?}"),
        }
    }

    #[test]
    fn passes_through_typed_target_path() {
        let schema = struct_schema(vec![("path", ParamType::TargetPath, false)]);
        let value = obj(
            vec![("path", Value::TargetPath("/abs".into()))],
            empty_span(),
        );
        let coerced = validate(Some(&schema), Some(value), &ctx())
            .expect("ok")
            .expect("some");
        let map = unwrap_object(coerced);
        match map.get("path").expect("path field").inner() {
            Value::TargetPath(s) => assert_eq!(s, "/abs"),
            other => panic!("expected TargetPath, got {other:?}"),
        }
    }

    #[test]
    fn wraps_absolute_string_as_target_path() {
        let schema = struct_schema(vec![("path", ParamType::TargetPath, false)]);
        let value = obj(vec![("path", Value::String("/abs".into()))], empty_span());
        let coerced = validate(Some(&schema), Some(value), &ctx())
            .expect("ok")
            .expect("some");
        let map = unwrap_object(coerced);
        match map.get("path").expect("path field").inner() {
            Value::TargetPath(s) => assert_eq!(s, "/abs"),
            other => panic!("expected TargetPath, got {other:?}"),
        }
    }

    #[test]
    fn rejects_relative_string_for_target_path() {
        let schema = struct_schema(vec![("path", ParamType::TargetPath, false)]);
        let value = obj(vec![("path", Value::String("rel".into()))], empty_span());
        let err = validate(Some(&schema), Some(value), &ctx()).unwrap_err();
        assert!(matches!(err, ParamsValidationError::Struct(_)));
    }

    #[test]
    fn missing_required_field_is_an_error() {
        let schema = struct_schema(vec![("path", ParamType::HostPath, false)]);
        let value = obj(vec![], empty_span());
        let err = validate(Some(&schema), Some(value), &ctx()).unwrap_err();
        let ParamsValidationError::Struct(boxed) = err else {
            panic!("expected Struct error");
        };
        assert!(matches!(
            boxed.errors.first(),
            Some(ParamValidationError::MissingParam { .. })
        ));
    }

    #[test]
    fn optional_missing_field_is_ok() {
        let schema = struct_schema(vec![("path", ParamType::HostPath, true)]);
        let value = obj(vec![], empty_span());
        let coerced = validate(Some(&schema), Some(value), &ctx())
            .expect("ok")
            .expect("some");
        assert!(unwrap_object(coerced).is_empty());
    }

    #[test]
    fn unknown_field_is_an_error() {
        let schema = struct_schema(vec![("path", ParamType::HostPath, true)]);
        let value = obj(
            vec![
                ("path", Value::HostPath("/abs".into())),
                ("extra", Value::String("oops".into())),
            ],
            empty_span(),
        );
        let err = validate(Some(&schema), Some(value), &ctx()).unwrap_err();
        let ParamsValidationError::Struct(boxed) = err else {
            panic!("expected Struct error");
        };
        assert!(boxed.errors.iter().any(|e| matches!(
            e,
            ParamValidationError::UnknownParam { key, .. } if key == "extra"
        )));
    }

    #[test]
    fn union_first_match_wins() {
        let mut a = ParamsStruct::new();
        a.insert(
            "name".into(),
            Spanned::new(ParamField::new(ParamType::String), empty_span()),
        );
        let mut b = ParamsStruct::new();
        b.insert(
            "id".into(),
            Spanned::new(ParamField::new(ParamType::Number), empty_span()),
        );
        let schema = Spanned::new(ParamTypes::Union(vec![a, b]), empty_span());
        let value = obj(
            vec![("id", Value::Number(Number::from(42u32)))],
            empty_span(),
        );
        let coerced = validate(Some(&schema), Some(value), &ctx())
            .expect("ok")
            .expect("some");
        assert!(unwrap_object(coerced).contains_key("id"));
    }

    #[test]
    fn empty_union_is_an_error() {
        let schema = Spanned::new(ParamTypes::Union(Vec::new()), empty_span());
        let value = obj(vec![], empty_span());
        let err = validate(Some(&schema), Some(value), &ctx()).unwrap_err();
        assert!(matches!(err, ParamsValidationError::EmptyUnion));
    }
}
