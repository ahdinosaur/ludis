#![allow(dead_code)]

use displaydoc::Display;
use lusid_params::{ParamTypes, ParamTypesFromRimuError};
use rimu::{Function, Span, Spanned, Value};
use rimu_interop::FromRimu;
use thiserror::Error;

#[derive(Debug, Clone)]
pub struct Name(pub String);

#[derive(Debug, Clone, Error, Display)]
pub enum NameFromRimuError {
    /// Expected a string for plan name
    NotAString,
}

impl FromRimu for Name {
    type Error = NameFromRimuError;

    fn from_rimu(value: Value) -> Result<Self, Self::Error> {
        let Value::String(string) = value else {
            return Err(NameFromRimuError::NotAString);
        };
        Ok(Name(string))
    }
}

#[derive(Debug, Clone)]
pub struct Version(pub String);

#[derive(Debug, Clone, Error, Display)]
pub enum VersionFromRimuError {
    /// Expected a string for plan version
    NotAString,
}

impl FromRimu for Version {
    type Error = VersionFromRimuError;

    fn from_rimu(value: Value) -> Result<Self, Self::Error> {
        let Value::String(string) = value else {
            return Err(VersionFromRimuError::NotAString);
        };
        Ok(Version(string))
    }
}

/// An item from setup's returned list.
/// Example:
///   { module: "@resource/pkg", id: "install-nvim", params: { package: "nvim" } }
#[derive(Debug, Clone)]
pub struct PlanItem {
    pub id: Option<Spanned<String>>,
    pub module: Spanned<String>,
    pub params: Option<Spanned<Value>>,
    pub requires: Vec<Spanned<String>>,
    pub required_by: Vec<Spanned<String>>,
    /// Inline `@operation/<id>` items to run when this plan item's resource has
    /// any change to apply. Must be empty unless `module` is `@resource/<id>`;
    /// rejected at lowering time on nested plans and `@operation/*` items.
    pub on_change: Vec<Spanned<InlineOperation>>,
}

/// One entry inside a plan item's `on_change` list.
///
/// Shape mirrors [`PlanItem`] but is restricted: only `module` and `params`
/// are accepted (and `module` must be `@operation/<id>`). Declaring `id`,
/// `requires`, or `required_by` on an inline operation is rejected with a
/// v1-specific error pointing to the offending field's span.
#[derive(Debug, Clone)]
pub struct InlineOperation {
    pub module: Spanned<String>,
    pub params: Option<Spanned<Value>>,
}

#[derive(Debug, Clone, Error, Display)]
pub enum IntoPlanItemError {
    /// Expected an object for plan action
    NotAnObject,
    /// Missing property: "module"
    ModuleMissing,
    /// Property "module" must be a string
    ModuleNotAString { span: Span },
    /// Property "id" must be a string
    IdNotAString { span: Span },
    /// Property "requires" must be a list
    RequiresNotAList { span: Span },
    /// "requires" list item must be a string
    RequiresItemNotAString { item_span: Span },
    /// Property "required_by" must be a list
    RequiredByNotAList { span: Span },
    /// "required_by" list item must be a string
    RequiredByItemNotAString { item_span: Span },

    /// Property "on_change" must be a list
    OnChangeNotAList { span: Span },
    /// "on_change" list item must be an object
    OnChangeItemNotAnObject { item_span: Span },
    /// "on_change" item is missing required property "module"
    OnChangeItemModuleMissing { item_span: Span },
    /// "on_change" item property "module" must be a string
    OnChangeItemModuleNotAString { item_span: Span },
    /// "on_change" items cannot declare "id" — hooks are anonymous in v1 and cannot be referenced from elsewhere. If you need a named, reusable action, declare a separate `@resource/command` with an `is_installed` probe.
    InlineOperationHasId { span: Span },
    /// "on_change" items cannot declare "requires" — handlers run after the resource they're attached to. To order one hook before another, combine them into a single shell operation, or attach the second hook to a downstream resource.
    InlineOperationHasRequires { span: Span },
    /// "on_change" items cannot declare "required_by" — see `InlineOperationHasRequires`.
    InlineOperationHasRequiredBy { span: Span },
}

impl FromRimu for PlanItem {
    type Error = IntoPlanItemError;

    fn from_rimu(value: Value) -> Result<Self, Self::Error> {
        let Value::Object(mut object) = value else {
            return Err(IntoPlanItemError::NotAnObject);
        };

        let module = match object.swap_remove("module") {
            Some(sp) => {
                let (value, span) = sp.clone().take();
                match value {
                    Value::String(s) => Spanned::new(s, span),
                    _ => {
                        return Err(IntoPlanItemError::ModuleNotAString { span });
                    }
                }
            }
            None => return Err(IntoPlanItemError::ModuleMissing),
        };

        let id = object
            .swap_remove("id")
            .map(|sp| {
                let (value, span) = sp.clone().take();
                match value {
                    Value::String(s) => Ok(Spanned::new(s, span)),
                    _ => Err(IntoPlanItemError::IdNotAString { span }),
                }
            })
            .transpose()?;

        let params = object.swap_remove("params");

        let requires = match object.swap_remove("requires") {
            None => Vec::new(),
            Some(value) => {
                let (value, span) = value.clone().take();
                match value {
                    Value::List(items) => {
                        let mut out = Vec::with_capacity(items.len());
                        for item in items {
                            let (item_value, item_span) = item.clone().take();
                            match item_value {
                                Value::String(s) => out.push(Spanned::new(s, item_span)),
                                _ => {
                                    return Err(IntoPlanItemError::RequiresItemNotAString {
                                        item_span,
                                    });
                                }
                            }
                        }
                        out
                    }
                    _ => return Err(IntoPlanItemError::RequiresNotAList { span }),
                }
            }
        };

        let required_by = match object.swap_remove("required_by") {
            None => Vec::new(),
            Some(value) => {
                let (value, span) = value.clone().take();
                match value {
                    Value::List(items) => {
                        let mut out = Vec::with_capacity(items.len());
                        for item in items {
                            let (item_value, item_span) = item.clone().take();
                            match item_value {
                                Value::String(s) => out.push(Spanned::new(s, item_span)),
                                _ => {
                                    return Err(IntoPlanItemError::RequiredByItemNotAString {
                                        item_span,
                                    });
                                }
                            }
                        }
                        out
                    }
                    _ => return Err(IntoPlanItemError::RequiredByNotAList { span }),
                }
            }
        };

        let on_change = match object.swap_remove("on_change") {
            None => Vec::new(),
            Some(value) => {
                let (value, span) = value.clone().take();
                match value {
                    Value::List(items) => {
                        let mut out = Vec::with_capacity(items.len());
                        for item in items {
                            let (item_value, item_span) = item.clone().take();
                            let op = InlineOperation::from_rimu(item_value)?;
                            out.push(Spanned::new(op, item_span));
                        }
                        out
                    }
                    _ => return Err(IntoPlanItemError::OnChangeNotAList { span }),
                }
            }
        };

        Ok(PlanItem {
            id,
            module,
            params,
            requires,
            required_by,
            on_change,
        })
    }
}

impl FromRimu for InlineOperation {
    type Error = IntoPlanItemError;

    fn from_rimu(value: Value) -> Result<Self, Self::Error> {
        let Value::Object(mut object) = value else {
            // Caller wraps with the proper item_span; we don't have one here.
            // The list-iteration path above attaches the per-item span via
            // Spanned, but the error itself needs a span — fabricate from
            // first-found field's span or fall back to a "no span" marker.
            // Easier: surface as OnChangeItemNotAnObject with span set by
            // the iteration code. Here we synthesize a placeholder.
            return Err(IntoPlanItemError::OnChangeItemNotAnObject {
                item_span: Span::default(),
            });
        };

        // Reject v1 disallowed fields with span pointing at the offending key's value.
        if let Some(sp) = object.swap_remove("id") {
            let (_, span) = sp.take();
            return Err(IntoPlanItemError::InlineOperationHasId { span });
        }
        if let Some(sp) = object.swap_remove("requires") {
            let (_, span) = sp.take();
            return Err(IntoPlanItemError::InlineOperationHasRequires { span });
        }
        if let Some(sp) = object.swap_remove("required_by") {
            let (_, span) = sp.take();
            return Err(IntoPlanItemError::InlineOperationHasRequiredBy { span });
        }

        let module = match object.swap_remove("module") {
            Some(sp) => {
                let (value, span) = sp.clone().take();
                match value {
                    Value::String(s) => Spanned::new(s, span),
                    _ => {
                        return Err(IntoPlanItemError::OnChangeItemModuleNotAString {
                            item_span: span,
                        });
                    }
                }
            }
            None => {
                return Err(IntoPlanItemError::OnChangeItemModuleMissing {
                    item_span: Span::default(),
                });
            }
        };

        let params = object.swap_remove("params");

        Ok(InlineOperation { module, params })
    }
}

#[derive(Debug, Clone)]
pub struct SetupFunction(pub Function);

#[derive(Debug, Clone, Error, Display)]
pub enum SetupFunctionFromRimuError {
    /// Expected a function for "setup"
    NotAFunction,
}

impl FromRimu for SetupFunction {
    type Error = SetupFunctionFromRimuError;

    fn from_rimu(value: Value) -> Result<Self, Self::Error> {
        let Value::Function(func) = value else {
            return Err(SetupFunctionFromRimuError::NotAFunction);
        };
        Ok(SetupFunction(func))
    }
}

#[derive(Debug, Clone)]
pub struct Plan {
    pub name: Option<Spanned<Name>>,
    pub version: Option<Spanned<Version>>,
    pub params: Option<Spanned<ParamTypes>>,
    /// setup: (params, system) => list of PlanItem
    pub setup: Spanned<SetupFunction>,
}

#[derive(Debug, Clone, Error, Display)]
pub enum PlanFromRimuError {
    /// Expected an object for plan
    NotAnObject,
    /// Invalid plan name: {0:?}
    Name(Spanned<NameFromRimuError>),
    /// Invalid plan version: {0:?}
    Version(Spanned<VersionFromRimuError>),
    /// Invalid plan params: {0:?}
    Params(Spanned<ParamTypesFromRimuError>),
    /// Missing property: "setup"
    SetupMissing,
    /// "setup" is not a function: {0:?}
    SetupNotAFunction(Spanned<SetupFunctionFromRimuError>),
}

impl FromRimu for Plan {
    type Error = PlanFromRimuError;

    fn from_rimu(value: Value) -> Result<Self, Self::Error> {
        let Value::Object(mut object) = value else {
            return Err(PlanFromRimuError::NotAnObject);
        };

        let name = object
            .swap_remove("name")
            .map(|name| Name::from_rimu_spanned(name).map_err(PlanFromRimuError::Name))
            .transpose()?;

        let version = object
            .swap_remove("version")
            .map(|v| Version::from_rimu_spanned(v).map_err(PlanFromRimuError::Version))
            .transpose()?;

        let params = object
            .swap_remove("params")
            .map(|params| ParamTypes::from_rimu_spanned(params).map_err(PlanFromRimuError::Params))
            .transpose()?;

        let setup_sp = object
            .swap_remove("setup")
            .ok_or(PlanFromRimuError::SetupMissing)?;
        let setup = SetupFunction::from_rimu_spanned(setup_sp)
            .map_err(PlanFromRimuError::SetupNotAFunction)?;

        Ok(Plan {
            name,
            version,
            params,
            setup,
        })
    }
}
