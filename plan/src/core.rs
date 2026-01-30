use lusid_params::{validate, ParamValues};
use lusid_resource::{apt::Apt, file::File, pacman::Pacman, ResourceParams, ResourceType};
use rimu::{Spanned, Value};

use crate::PlanItemToResourceError;

pub fn is_core_module(module: &Spanned<String>) -> Option<&str> {
    module.inner().strip_prefix("@core/")
}

pub fn core_module(
    core_module_id: &str,
    params: Option<Spanned<Value>>,
) -> Result<ResourceParams, PlanItemToResourceError> {
    match core_module_id {
        Apt::ID => core_module_for_resource::<Apt>(params).map(ResourceParams::Apt),
        File::ID => core_module_for_resource::<File>(params).map(ResourceParams::File),
        Pacman::ID => core_module_for_resource::<Pacman>(params).map(ResourceParams::Pacman),
        other => Err(PlanItemToResourceError::UnsupportedCoreModuleId {
            id: other.to_string(),
        }),
    }
}

fn core_module_for_resource<R: ResourceType>(
    params_value: Option<Spanned<Value>>,
) -> Result<R::Params, PlanItemToResourceError> {
    let params_value = params_value.ok_or(PlanItemToResourceError::MissingParams)?;
    let param_types = R::param_types();

    let params_struct = validate(param_types.as_ref(), Some(&params_value))?;
    let params_struct = params_struct.expect("params struc should exist for core module");

    let param_values = ParamValues::from_rimu_spanned(params_value, params_struct)
        .map_err(PlanItemToResourceError::ParamsValueFromRimu)?;

    let params: R::Params = param_values.into_inner().into_type()?;
    Ok(params)
}
