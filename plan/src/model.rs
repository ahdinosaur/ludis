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

        Ok(PlanItem {
            id,
            module,
            params,
            requires,
            required_by,
        })
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
