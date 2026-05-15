//! Resource modules are the built-in declarative state types exposed to plans
//! under the `@resource/<id>` namespace (e.g. `@resource/apt`, `@resource/file`).
//! This module routes a plan item's module string to the matching [`ResourceType`] impl.

use lusid_params::ParseParams;
use lusid_resource::{
    ResourceParams, ResourceType, apt::Apt, apt_repo::AptRepo, aur::Aur, command::Command,
    directory::Directory, file::File, flatpak::Flatpak, flatpak_remote::FlatpakRemote, git::Git,
    group::Group, pacman::Pacman, podman::Podman, secret::Secret, systemd::Systemd, user::User,
};
use rimu::{Spanned, Value};

use crate::PlanItemToResourceError;

/// Returns the resource id (e.g. `"apt"`) if `module` uses the `@resource/<id>`
/// prefix, otherwise `None` - meaning the module should be resolved as a nested
/// plan (or, if it uses `@operation/`, rejected as a top-level item elsewhere).
pub fn is_resource_module(module: &Spanned<String>) -> Option<&str> {
    module.inner().strip_prefix("@resource/")
}

pub fn resource_module(
    resource_module_id: &str,
    params: Option<Spanned<Value>>,
) -> Result<ResourceParams, PlanItemToResourceError> {
    match resource_module_id {
        Apt::ID => resource_module_for_resource::<Apt>(params).map(ResourceParams::Apt),
        AptRepo::ID => resource_module_for_resource::<AptRepo>(params).map(ResourceParams::AptRepo),
        Aur::ID => resource_module_for_resource::<Aur>(params).map(ResourceParams::Aur),
        File::ID => resource_module_for_resource::<File>(params).map(ResourceParams::File),
        Directory::ID => {
            resource_module_for_resource::<Directory>(params).map(ResourceParams::Directory)
        }
        Flatpak::ID => resource_module_for_resource::<Flatpak>(params).map(ResourceParams::Flatpak),
        FlatpakRemote::ID => {
            resource_module_for_resource::<FlatpakRemote>(params).map(ResourceParams::FlatpakRemote)
        }
        Pacman::ID => resource_module_for_resource::<Pacman>(params).map(ResourceParams::Pacman),
        Podman::ID => resource_module_for_resource::<Podman>(params).map(ResourceParams::Podman),
        Command::ID => resource_module_for_resource::<Command>(params).map(ResourceParams::Command),
        Git::ID => resource_module_for_resource::<Git>(params).map(ResourceParams::Git),
        Secret::ID => resource_module_for_resource::<Secret>(params).map(ResourceParams::Secret),
        Systemd::ID => resource_module_for_resource::<Systemd>(params).map(ResourceParams::Systemd),
        User::ID => resource_module_for_resource::<User>(params).map(ResourceParams::User),
        Group::ID => resource_module_for_resource::<Group>(params).map(ResourceParams::Group),
        other => Err(PlanItemToResourceError::UnsupportedResourceModuleId {
            id: other.to_string(),
        }),
    }
}

fn resource_module_for_resource<R: ResourceType>(
    params_value: Option<Spanned<Value>>,
) -> Result<R::Params, PlanItemToResourceError> {
    let params_value = params_value.ok_or(PlanItemToResourceError::MissingParams)?;
    R::Params::parse_params(params_value).map_err(PlanItemToResourceError::Parse)
}
