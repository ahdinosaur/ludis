//! "Core modules" are the built-in resource types exposed to plans under the
//! `@core/<id>` namespace (e.g. `@core/apt`, `@core/file`). This module routes a plan
//! item's module string to the matching [`ResourceType`] impl.

use lusid_params::ParseParams;
use lusid_resource::{
    ResourceParams, ResourceType, apt::Apt, apt_repo::AptRepo, aur::Aur, command::Command,
    directory::Directory, file::File, git::Git, group::Group, pacman::Pacman, podman::Podman,
    secret::Secret, systemd::Systemd, ufw::Ufw, user::User,
};
use rimu::{Spanned, Value};

use crate::PlanItemToResourceError;

/// Returns the core id (e.g. `"apt"`) if `module` uses the `@core/<id>` prefix,
/// otherwise `None` — meaning the module should be resolved as a nested plan.
pub fn is_core_module(module: &Spanned<String>) -> Option<&str> {
    module.inner().strip_prefix("@core/")
}

/// Parse `params` directly into the matching core module's typed [`ResourceParams`]
/// variant. Errors if `id` is unknown or the params don't fit the resource's shape.
pub fn core_module(
    core_module_id: &str,
    params: Option<Spanned<Value>>,
) -> Result<ResourceParams, PlanItemToResourceError> {
    match core_module_id {
        Apt::ID => core_module_for_resource::<Apt>(params).map(ResourceParams::Apt),
        AptRepo::ID => core_module_for_resource::<AptRepo>(params).map(ResourceParams::AptRepo),
        Aur::ID => core_module_for_resource::<Aur>(params).map(ResourceParams::Aur),
        File::ID => core_module_for_resource::<File>(params).map(ResourceParams::File),
        Directory::ID => {
            core_module_for_resource::<Directory>(params).map(ResourceParams::Directory)
        }
        Pacman::ID => core_module_for_resource::<Pacman>(params).map(ResourceParams::Pacman),
        Podman::ID => core_module_for_resource::<Podman>(params).map(ResourceParams::Podman),
        Command::ID => core_module_for_resource::<Command>(params).map(ResourceParams::Command),
        Git::ID => core_module_for_resource::<Git>(params).map(ResourceParams::Git),
        Secret::ID => core_module_for_resource::<Secret>(params).map(ResourceParams::Secret),
        Systemd::ID => core_module_for_resource::<Systemd>(params).map(ResourceParams::Systemd),
        Ufw::ID => core_module_for_resource::<Ufw>(params).map(ResourceParams::Ufw),
        User::ID => core_module_for_resource::<User>(params).map(ResourceParams::User),
        Group::ID => core_module_for_resource::<Group>(params).map(ResourceParams::Group),
        other => Err(PlanItemToResourceError::UnsupportedCoreModuleId {
            id: other.to_string(),
        }),
    }
}

fn core_module_for_resource<R: ResourceType>(
    params_value: Option<Spanned<Value>>,
) -> Result<R::Params, PlanItemToResourceError> {
    let params_value = params_value.ok_or(PlanItemToResourceError::MissingParams)?;
    R::Params::parse_params(params_value).map_err(PlanItemToResourceError::Parse)
}
