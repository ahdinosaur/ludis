//! Parameter schemas and values.

use displaydoc::Display;
use indexmap::IndexMap;
use rimu::{from_serde_value, SerdeValue, SerdeValueError, SourceId, Span, Spanned, Value};
use rimu_interop::{to_rimu, FromRimu, ToRimuError};
use serde::{de::DeserializeOwned, Serialize};
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum ParamType {
    Boolean,
    String,
    Number,
    List { item: Box<Spanned<ParamType>> },
    Object { value: Box<Spanned<ParamType>> },
    Path,
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

    pub fn typ(&self) -> &ParamType {
        &self.typ
    }

    pub fn optional(&self) -> &bool {
        &self.optional
    }
}

#[derive(Debug, Clone)]
pub enum ParamTypes {
    // A single object structure: keys -> fields
    Struct(IndexMap<String, Spanned<ParamField>>),
    // A union of possible object structures.
    Union(Vec<IndexMap<String, Spanned<ParamField>>>),
}

#[derive(Debug, Clone, Default)]
pub struct ParamValues(IndexMap<String, Spanned<Value>>);

#[derive(Debug, Clone, Error, Display)]
pub enum ParamValuesFromTypeError {
    /// Failed to convert serializable value to Rimu
    ToRimu(#[from] ToRimuError),
    /// Failed to convert Rimu value into parameter values
    FromRimu(#[from] ParamValuesFromRimuError),
}

impl ParamValues {
    pub fn from_type<T>(
        value: T,
        source_id: SourceId,
    ) -> Result<Spanned<Self>, ParamValuesFromTypeError>
    where
        T: Serialize,
    {
        let rimu_value = to_rimu(value, source_id)?;
        let param_values =
            ParamValues::from_rimu_spanned(rimu_value).map_err(Spanned::into_inner)?;
        Ok(param_values)
    }
}

#[derive(Debug, Clone, Error, Display)]
pub enum ParamValuesFromRimuError {
    /// Expected an object mapping parameter names to values
    NotAnObject,
}

impl FromRimu for ParamValues {
    type Error = ParamValuesFromRimuError;

    fn from_rimu(value: Value) -> Result<Self, Self::Error> {
        let Value::Object(object) = value else {
            return Err(ParamValuesFromRimuError::NotAnObject);
        };
        Ok(ParamValues(object))
    }
}

impl ParamValues {
    pub fn into_rimu(self) -> Value {
        Value::Object(self.0)
    }

    pub fn get(&self, key: &str) -> Option<&Spanned<Value>> {
        self.0.get(key)
    }

    pub fn into_type<T>(self) -> Result<T, SerdeValueError>
    where
        T: DeserializeOwned,
    {
        let value = Value::Object(self.0);
        let serde_value = SerdeValue::from(value);
        from_serde_value(serde_value)
    }
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
            "path" => Ok(ParamType::Path),
            "host-path" => Ok(ParamType::Number),
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

    // In Rimu:
    // - An object defines a Struct (map of fields).
    // - A list defines a Union; each list item is an object defining one case.
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
                            })
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
                                })
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
    /// Parameter struct did not match all fields
    Struct(#[from] Box<ParamsStructValidationError>),
    /// Parameter union did not match any case
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

fn validate_type(
    param_type: &Spanned<ParamType>,
    value: &Spanned<Value>,
) -> Result<(), ValidateValueError> {
    let typ_inner = param_type.inner();
    let value_inner = value.inner();

    match typ_inner {
        ParamType::Boolean => match value_inner {
            Value::Boolean(_) => Ok(()),
            _ => Err(mismatch(param_type, value)),
        },

        ParamType::String => match value_inner {
            Value::String(_) => Ok(()),
            _ => Err(mismatch(param_type, value)),
        },

        ParamType::Number => match value_inner {
            Value::Number(_) => Ok(()),
            _ => Err(mismatch(param_type, value)),
        },

        ParamType::Path => {
            #[allow(clippy::collapsible_if)]
            if let Value::String(path) = value_inner {
                if is_like_unix_path(path) {
                    return Ok(());
                }
            }
            Err(mismatch(param_type, value))
        }

        ParamType::List { item } => {
            let Value::List(items) = value_inner else {
                return Err(mismatch(param_type, value));
            };

            for (index, item_value) in items.iter().enumerate() {
                if let Err(error) = validate_type(item, item_value) {
                    return Err(ValidateValueError::ListItem {
                        index,
                        error: Box::new(error),
                    });
                }
            }

            Ok(())
        }

        ParamType::Object { value: value_type } => {
            let Value::Object(map) = value_inner else {
                return Err(mismatch(param_type, value));
            };

            for (key, entry_value) in map.iter() {
                if let Err(error) = validate_type(value_type, entry_value) {
                    return Err(ValidateValueError::ObjectEntry {
                        key: key.clone(),
                        error: Box::new(error),
                    });
                }
            }

            Ok(())
        }
    }
}

fn is_like_unix_path(path: &str) -> bool {
    !path.is_empty() && !path.contains('\\') && !path.contains(':') && path.contains('/')
}

fn validate_struct(
    fields: &IndexMap<String, Spanned<ParamField>>,
    values: &ParamValues,
) -> Result<(), ParamsStructValidationError> {
    let mut errors: Vec<ParamValidationError> = Vec::new();

    // Requiredness and per-field validation.
    for (key, spanned_field) in fields.iter() {
        let (field, field_span) = spanned_field.clone().take();
        let spanned_type = Spanned::new(field.typ().clone(), field_span);

        match values.0.get(key) {
            Some(spanned_value) => {
                if let Err(error) = validate_type(&spanned_type, spanned_value) {
                    errors.push(ParamValidationError::InvalidParam {
                        key: key.clone(),
                        error: Box::new(error),
                    });
                }
            }
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

    // Unknown keys.
    for (key, spanned_value) in values.0.iter() {
        if !fields.contains_key(key) {
            errors.push(ParamValidationError::UnknownParam {
                key: key.clone(),
                value: Box::new(spanned_value.clone()),
            });
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(ParamsStructValidationError { errors })
    }
}

// For Struct: validate all fields.
// For Union: succeed if any one case validates; otherwise return all case errors.
pub fn validate(
    param_types: Option<&Spanned<ParamTypes>>,
    param_values: Option<&Spanned<ParamValues>>,
) -> Result<(), ParamsValidationError> {
    let (param_types, param_values) = match (param_types, param_values) {
        (Some(param_types), Some(param_values)) => (param_types, param_values),
        (Some(_), None) => {
            return Err(ParamsValidationError::TypesWithoutValues);
        }
        (None, Some(_)) => {
            return Err(ParamsValidationError::ValuesWithoutTypes);
        }
        (None, None) => {
            return Ok(());
        }
    };

    let param_types = param_types.inner();
    let param_values = param_values.inner();

    match param_types {
        ParamTypes::Struct(map) => {
            validate_struct(map, param_values).map_err(Box::new)?;

            Ok(())
        }
        ParamTypes::Union(cases) => {
            if cases.is_empty() {
                return Err(ParamsValidationError::EmptyUnion);
            }

            let mut case_errors: Vec<ParamsStructValidationError> = Vec::with_capacity(cases.len());

            for case in cases {
                match validate_struct(case, param_values) {
                    Ok(()) => return Ok(()),
                    Err(error) => case_errors.push(error),
                }
            }

            Err(ParamsValidationError::Union { case_errors })
        }
    }
}
