//! One-pass parse-and-validate from a Rimu [`Value`] into a resource's typed
//! `Params`.
//!
//! This is the lusid-side of "Option C" in the params roadmap: the schema
//! ([`ParamType`] / [`ParamTypes`]) and the typed extraction live in the same
//! pass. Resources implement [`ParseParams`] for their `Params` types and pull
//! fields out via [`StructFields`].
//!
//! # Path types
//!
//! Rimu carries first-class [`Value::HostPath`] / [`Value::TargetPath`]
//! variants (built via the `host_path("./...")` / `target_path("/...")`
//! stdlib functions). The parsers here accept those variants directly, and
//! also accept plain [`Value::String`]s for plans that pre-date typed paths:
//!
//! - [`parse_host_path`] resolves a relative string against the source
//!   span's parent directory. Absolute strings are rejected - they have no
//!   anchoring source dir to credit, and forwarded host paths arrive
//!   typed.
//! - [`parse_target_path`] requires absolute strings.
//!
//! Once typed paths are universal in plans, the string-fallback arms can go.
//!
//! # Extending
//!
//! New scalar types: write a `parse_<name>(value: Spanned<Value>) -> Result<T, _>`
//! function. New container types compose via [`parse_list`].
//! The convenience `required_*` / `optional_*` methods on [`StructFields`]
//! are sugar - anything they don't cover is one [`StructFields::required`]
//! / [`StructFields::optional`] call away with a custom parser closure.
//!
//! # Span propagation
//!
//! Every parse error is wrapped in a [`Spanned<ParseError>`], so diagnostics
//! point back at the offending value in the source `.lusid` file. Helpers
//! attach the field's span where the field-level error originated, not the
//! enclosing struct.

use std::path::{Path, PathBuf};

use displaydoc::Display;
use indexmap::IndexMap;
use rimu::{Number, Span, Spanned, Value};
use thiserror::Error;

/// Failures while parsing a Rimu value into a typed Rust value.
#[derive(Debug, Clone, Error, Display)]
pub enum ParseError {
    /// Expected {expected}, got value {got:?}
    TypeMismatch {
        expected: &'static str,
        got: Box<Value>,
    },

    /// Number {value} is not a non-negative integer that fits in u32
    NumberOutOfRangeU32 { value: Number },

    /// Host-path string \"{value}\" must be relative
    HostPathNotRelative { value: String },

    /// Cannot resolve relative host-path \"{value}\": its value-span source ({source_id:?}) has no parent directory to anchor against. This usually means the value came from CLI `--params` JSON (empty source) or a synthesised string. Declare the field as `host-path` in the plan's params schema so [`crate::validate`] coerces it once at the plan boundary against `ctx.root_path`.
    HostPathNoSourceDir { value: String, source_id: String },

    /// Target-path string \"{value}\" must be absolute
    TargetPathNotAbsolute { value: String },

    /// Failed to parse list at index {index}: {error}
    ListItem {
        index: usize,
        error: Box<Spanned<ParseError>>,
    },

    /// Failed to parse field \"{key}\": {error}
    Field {
        key: String,
        error: Box<Spanned<ParseError>>,
    },

    /// Missing required field \"{key}\"
    MissingField { key: String },

    /// Unknown field \"{key}\"
    UnknownField { key: String, value: Box<Value> },

    /// Discriminator field \"{key}\" had unexpected value {got:?}; expected one of: {expected:?}
    UnknownDiscriminator {
        key: &'static str,
        got: Box<Value>,
        expected: Vec<&'static str>,
    },

    /// Value {got:?} is invalid: {reason}
    InvalidValue {
        reason: &'static str,
        got: Box<Value>,
    },
}

/// Parse a typed Rust value from a Rimu [`Spanned<Value>`].
///
/// Implementors decide how to read their shape out of the dynamic Rimu value.
/// This is the resource-boundary trait - every `@resource/<id>` resource impls it
/// for its `Params` type.
pub trait ParseParams: Sized {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>>;
}

/// Helper that consumes a Rimu object's fields by name, returning typed values
/// and tracking which keys haven't been read.
///
/// Construct with [`StructFields::new`], call `required_*` / `optional_*` for
/// each declared field, then [`StructFields::finish`] to assert no unknown
/// fields remain.
///
/// `finish` must be called even on the optional path - otherwise extra keys
/// the user passed go undetected.
pub struct StructFields {
    map: IndexMap<String, Spanned<Value>>,
    span: Span,
}

impl StructFields {
    /// Take the value apart as an object. Errors if the value isn't an object.
    pub fn new(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let (value, span) = value.take();
        match value {
            Value::Object(map) => Ok(StructFields { map, span }),
            other => Err(Spanned::new(
                ParseError::TypeMismatch {
                    expected: "object",
                    got: Box::new(other),
                },
                span,
            )),
        }
    }

    /// Peek at whether a key is present without consuming it. Useful for
    /// untagged-union dispatch (e.g. apt's `package` vs `packages`).
    pub fn has(&self, key: &str) -> bool {
        self.map.contains_key(key)
    }

    fn take(&mut self, key: &str) -> Option<Spanned<Value>> {
        self.map.swap_remove(key)
    }

    fn missing(&self, key: &str) -> Spanned<ParseError> {
        Spanned::new(
            ParseError::MissingField {
                key: key.to_string(),
            },
            self.span.clone(),
        )
    }

    fn wrap_field(key: &str, error: Spanned<ParseError>) -> Spanned<ParseError> {
        let span = error.span().clone();
        Spanned::new(
            ParseError::Field {
                key: key.to_string(),
                error: Box::new(error),
            },
            span,
        )
    }

    /// Read a required field via the supplied `parse` function.
    pub fn required<T, F>(&mut self, key: &str, parse: F) -> Result<T, Spanned<ParseError>>
    where
        F: FnOnce(Spanned<Value>) -> Result<T, Spanned<ParseError>>,
    {
        let value = self.take(key).ok_or_else(|| self.missing(key))?;
        parse(value).map_err(|error| Self::wrap_field(key, error))
    }

    /// Read an optional field. Returns `Ok(None)` when the key is absent or
    /// when its value is `Null` (so plans can write `field: null` to mean
    /// "use the default").
    pub fn optional<T, F>(&mut self, key: &str, parse: F) -> Result<Option<T>, Spanned<ParseError>>
    where
        F: FnOnce(Spanned<Value>) -> Result<T, Spanned<ParseError>>,
    {
        let Some(value) = self.take(key) else {
            return Ok(None);
        };
        if matches!(value.inner(), Value::Null) {
            return Ok(None);
        }
        parse(value)
            .map(Some)
            .map_err(|error| Self::wrap_field(key, error))
    }

    /// Read a tagged-union discriminator: a required string field whose value
    /// must match one of `expected` (typically the case tags `"present"`,
    /// `"absent"`, etc.). Returns the matched `&'static str` so the caller
    /// can dispatch on it.
    ///
    /// On a mismatch, emits [`ParseError::UnknownDiscriminator`] listing the
    /// allowed tags - better than a generic literal-mismatch when the user
    /// typo'd a tag name.
    pub fn take_discriminator(
        &mut self,
        key: &'static str,
        expected: &[&'static str],
    ) -> Result<&'static str, Spanned<ParseError>> {
        let value = self.take(key).ok_or_else(|| self.missing(key))?;
        let (inner, span) = value.take();
        let Value::String(got) = inner else {
            return Err(Self::wrap_field(
                key,
                Spanned::new(
                    ParseError::TypeMismatch {
                        expected: "string discriminator",
                        got: Box::new(inner),
                    },
                    span,
                ),
            ));
        };
        match expected.iter().find(|tag| **tag == got).copied() {
            Some(tag) => Ok(tag),
            None => Err(Spanned::new(
                ParseError::UnknownDiscriminator {
                    key,
                    got: Box::new(Value::String(got)),
                    expected: expected.to_vec(),
                },
                span,
            )),
        }
    }

    /// Convenience: read a required `String` field.
    pub fn required_string(&mut self, key: &str) -> Result<String, Spanned<ParseError>> {
        self.required(key, parse_string)
    }

    /// Convenience: read an optional `String` field.
    pub fn optional_string(&mut self, key: &str) -> Result<Option<String>, Spanned<ParseError>> {
        self.optional(key, parse_string)
    }

    /// Convenience: read a required boolean field.
    pub fn required_bool(&mut self, key: &str) -> Result<bool, Spanned<ParseError>> {
        self.required(key, parse_bool)
    }

    /// Convenience: read an optional boolean field.
    pub fn optional_bool(&mut self, key: &str) -> Result<Option<bool>, Spanned<ParseError>> {
        self.optional(key, parse_bool)
    }

    /// Convenience: read a required u32 (non-negative integer) field.
    pub fn required_u32(&mut self, key: &str) -> Result<u32, Spanned<ParseError>> {
        self.required(key, parse_u32)
    }

    /// Convenience: read an optional u32 (non-negative integer) field.
    pub fn optional_u32(&mut self, key: &str) -> Result<Option<u32>, Spanned<ParseError>> {
        self.optional(key, parse_u32)
    }

    /// Convenience: read a required host-path field, returning a fully resolved
    /// absolute [`PathBuf`].
    pub fn required_host_path(&mut self, key: &str) -> Result<PathBuf, Spanned<ParseError>> {
        self.required(key, parse_host_path)
    }

    /// Same as [`required_host_path`](Self::required_host_path) but also
    /// returns the [`Span`] of the source value, so downstream validation
    /// failures (e.g. "source missing on disk") can point at the offending
    /// `.lusid` line.
    pub fn required_host_path_spanned(
        &mut self,
        key: &str,
    ) -> Result<Spanned<PathBuf>, Spanned<ParseError>> {
        self.required(key, |value| {
            let span = value.span();
            parse_host_path(value).map(|path| Spanned::new(path, span))
        })
    }

    /// Convenience: read a required target-path field, returning the absolute
    /// path string verbatim.
    pub fn required_target_path(&mut self, key: &str) -> Result<String, Spanned<ParseError>> {
        self.required(key, parse_target_path)
    }

    /// Convenience: read an optional target-path field.
    pub fn optional_target_path(
        &mut self,
        key: &str,
    ) -> Result<Option<String>, Spanned<ParseError>> {
        self.optional(key, parse_target_path)
    }

    /// Convenience: read a required `Vec<String>` field.
    pub fn required_string_list(&mut self, key: &str) -> Result<Vec<String>, Spanned<ParseError>> {
        self.required(key, |value| parse_list(value, parse_string))
    }

    /// Convenience: read an optional `Vec<String>` field.
    pub fn optional_string_list(
        &mut self,
        key: &str,
    ) -> Result<Option<Vec<String>>, Spanned<ParseError>> {
        self.optional(key, |value| parse_list(value, parse_string))
    }

    /// Read a required list of host-path fields, returning each as a
    /// [`Spanned<PathBuf>`] so per-element diagnostics (e.g. "this file is
    /// missing on disk") can point at the offending list entry rather than
    /// the list as a whole.
    pub fn required_host_path_spanned_list(
        &mut self,
        key: &str,
    ) -> Result<Vec<Spanned<PathBuf>>, Spanned<ParseError>> {
        self.required(key, |value| {
            parse_list(value, |item| {
                let span = item.span();
                parse_host_path(item).map(|path| Spanned::new(path, span))
            })
        })
    }

    /// Assert no unknown fields remain. Call once after consuming every
    /// declared field.
    ///
    /// Fails fast on the first unknown key (unlike the historic
    /// `validate_struct` which collected all of them). Multi-error reporting
    /// would let plan authors see every typo at once, but means the rest of
    /// the parse must keep going after a hard failure - not worth the
    /// complexity for the resource boundary, where the immediate first
    /// unknown is usually enough to point at.
    pub fn finish(self) -> Result<(), Spanned<ParseError>> {
        if let Some((key, value)) = self.map.into_iter().next() {
            let (inner, span) = value.take();
            return Err(Spanned::new(
                ParseError::UnknownField {
                    key,
                    value: Box::new(inner),
                },
                span,
            ));
        }
        Ok(())
    }
}

/// Parse a Rimu string value.
pub fn parse_string(value: Spanned<Value>) -> Result<String, Spanned<ParseError>> {
    let (value, span) = value.take();
    match value {
        Value::String(s) => Ok(s),
        other => Err(Spanned::new(
            ParseError::TypeMismatch {
                expected: "string",
                got: Box::new(other),
            },
            span,
        )),
    }
}

/// Parse a Rimu boolean value.
pub fn parse_bool(value: Spanned<Value>) -> Result<bool, Spanned<ParseError>> {
    let (value, span) = value.take();
    match value {
        Value::Boolean(b) => Ok(b),
        other => Err(Spanned::new(
            ParseError::TypeMismatch {
                expected: "boolean",
                got: Box::new(other),
            },
            span,
        )),
    }
}

/// Parse a Rimu number into the schema's [`Number`] type.
pub fn parse_number(value: Spanned<Value>) -> Result<Number, Spanned<ParseError>> {
    let (value, span) = value.take();
    match value {
        Value::Number(n) => Ok(n),
        other => Err(Spanned::new(
            ParseError::TypeMismatch {
                expected: "number",
                got: Box::new(other),
            },
            span,
        )),
    }
}

/// Parse a Rimu number as a non-negative `u32`.
pub fn parse_u32(value: Spanned<Value>) -> Result<u32, Spanned<ParseError>> {
    let span = value.span().clone();
    let number = parse_number(value)?;
    number
        .to_u32()
        .ok_or_else(|| Spanned::new(ParseError::NumberOutOfRangeU32 { value: number }, span))
}

/// Parse a host-path field into a [`PathBuf`].
///
/// Accepts either:
/// - a [`Value::HostPath`] produced by Rimu's `host_path("./rel")` stdlib
///   (already resolved against its source dir at construction time), or
/// - a [`Value::String`] holding a *relative* path, which is resolved here
///   against the source span's parent directory - preserving behaviour for
///   plans that wrote `source: "./gitconfig"` rather than
///   `source: host_path("./gitconfig")`.
///
/// Whether the resulting `PathBuf` is filesystem-absolute depends on the
/// source itself: a plan loaded from an absolute path produces an absolute
/// result; a plan loaded from a relative path produces a CWD-relative
/// result. Downstream code that needs an absolute path should canonicalise.
pub fn parse_host_path(value: Spanned<Value>) -> Result<PathBuf, Spanned<ParseError>> {
    let (value, span) = value.take();
    match value {
        Value::HostPath(path) => Ok(path),
        Value::String(s) => {
            let value_path = PathBuf::from(&s);
            // Forwarded host paths arrive typed as `Value::HostPath` -
            // an absolute *string* for a `host-path` field is
            // a bug at the call site (no source-dir to anchor against, no
            // path-typed sender to credit). Reject up front.
            if value_path.is_absolute() {
                return Err(Spanned::new(
                    ParseError::HostPathNotRelative { value: s },
                    span,
                ));
            }
            let source_id = span.source().as_str().to_owned();
            let Some(source_dir) = Path::new(&source_id).parent() else {
                return Err(Spanned::new(
                    ParseError::HostPathNoSourceDir {
                        value: s,
                        source_id,
                    },
                    span,
                ));
            };
            Ok(source_dir.join(value_path))
        }
        other => Err(Spanned::new(
            ParseError::TypeMismatch {
                expected: "host-path or relative string",
                got: Box::new(other),
            },
            span,
        )),
    }
}

/// Parse a target-path field into an absolute path string on the managed host.
///
/// Accepts a [`Value::TargetPath`] directly, or a [`Value::String`] that
/// already starts with `/`. Relative strings are rejected - target paths must
/// be absolute on the target machine.
pub fn parse_target_path(value: Spanned<Value>) -> Result<String, Spanned<ParseError>> {
    let (value, span) = value.take();
    match value {
        Value::TargetPath(s) => Ok(s.to_string()),
        Value::String(s) => {
            if Path::new(&s).is_absolute() {
                Ok(s)
            } else {
                Err(Spanned::new(
                    ParseError::TargetPathNotAbsolute { value: s },
                    span,
                ))
            }
        }
        other => Err(Spanned::new(
            ParseError::TypeMismatch {
                expected: "target-path or absolute string",
                got: Box::new(other),
            },
            span,
        )),
    }
}

/// Parse a homogeneous list, applying `parse_item` to each element.
pub fn parse_list<T, F>(value: Spanned<Value>, parse_item: F) -> Result<Vec<T>, Spanned<ParseError>>
where
    F: Fn(Spanned<Value>) -> Result<T, Spanned<ParseError>>,
{
    let (value, span) = value.take();
    let Value::List(items) = value else {
        return Err(Spanned::new(
            ParseError::TypeMismatch {
                expected: "list",
                got: Box::new(value),
            },
            span,
        ));
    };

    let mut out = Vec::with_capacity(items.len());
    for (index, item) in items.into_iter().enumerate() {
        let item_span = item.span().clone();
        match parse_item(item) {
            Ok(value) => out.push(value),
            Err(error) => {
                return Err(Spanned::new(
                    ParseError::ListItem {
                        index,
                        error: Box::new(error),
                    },
                    item_span,
                ));
            }
        }
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rimu::SourceId;

    fn span(source: &str) -> Span {
        Span::new(SourceId::from(source.to_string()), 0, 0)
    }

    fn empty_span() -> Span {
        Span::new(SourceId::empty(), 0, 0)
    }

    #[test]
    fn parse_host_path_passes_through_typed_value() {
        let value = Spanned::new(Value::HostPath(PathBuf::from("/abs")), empty_span());
        let result = parse_host_path(value).expect("ok");
        assert_eq!(result, PathBuf::from("/abs"));
    }

    #[test]
    fn parse_host_path_resolves_relative_string_against_file_source_dir() {
        let value = Spanned::new(Value::String("bar".into()), span("/plans/foo.lusid"));
        let result = parse_host_path(value).expect("ok");
        assert_eq!(result, PathBuf::from("/plans/bar"));
    }

    #[test]
    fn parse_host_path_rejects_absolute_string() {
        // An absolute string for a `host-path` field cannot be a flattened forwarded value.
        let value = Spanned::new(Value::String("/abs".into()), span("/plans/foo.lusid"));
        let err = parse_host_path(value).unwrap_err();
        assert!(matches!(
            err.inner(),
            ParseError::HostPathNotRelative { .. }
        ));
    }

    /// Empty span source means the value has no anchoring file (CLI params,
    /// synthesised values). Surface a dedicated error that points at the fix
    /// - declaring the field as `host-path` in the plan's schema.
    #[test]
    fn parse_host_path_empty_source_returns_no_source_dir_error() {
        let value = Spanned::new(Value::String("bar".into()), empty_span());
        let err = parse_host_path(value).unwrap_err();
        match err.inner() {
            ParseError::HostPathNoSourceDir { value, source_id } => {
                assert_eq!(value, "bar");
                assert_eq!(source_id, "");
            }
            other => panic!("expected HostPathNoSourceDir, got {other:?}"),
        }
    }

    #[test]
    fn parse_target_path_passes_through_typed_value() {
        let value = Spanned::new(Value::TargetPath("/abs".into()), empty_span());
        assert_eq!(parse_target_path(value).expect("ok"), "/abs");
    }

    #[test]
    fn parse_target_path_accepts_absolute_string() {
        let value = Spanned::new(Value::String("/abs".into()), empty_span());
        assert_eq!(parse_target_path(value).expect("ok"), "/abs");
    }

    #[test]
    fn parse_target_path_rejects_relative_string() {
        let value = Spanned::new(Value::String("rel".into()), empty_span());
        let err = parse_target_path(value).unwrap_err();
        assert!(matches!(
            err.inner(),
            ParseError::TargetPathNotAbsolute { .. }
        ));
    }

    #[test]
    fn parse_list_of_host_paths_preserves_per_element_spans() {
        let span_a = span("/plans/a.lusid");
        let span_b = span("/plans/b.lusid");
        let items = Value::List(vec![
            Spanned::new(Value::HostPath(PathBuf::from("/host/a")), span_a.clone()),
            Spanned::new(Value::HostPath(PathBuf::from("/host/b")), span_b.clone()),
        ]);
        let parsed = parse_list(Spanned::new(items, empty_span()), |item| {
            let span = item.span();
            parse_host_path(item).map(|path| Spanned::new(path, span))
        })
        .expect("ok");
        assert_eq!(parsed.len(), 2);
        assert_eq!(parsed[0].inner(), &PathBuf::from("/host/a"));
        assert_eq!(parsed[0].span().source().as_str(), "/plans/a.lusid");
        assert_eq!(parsed[1].inner(), &PathBuf::from("/host/b"));
        assert_eq!(parsed[1].span().source().as_str(), "/plans/b.lusid");
    }
}
