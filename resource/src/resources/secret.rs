//! `@core/secret`: materialise an age-decrypted plaintext onto the target
//! filesystem, referenced by name (agenix-style — the plan names the secret,
//! the plaintext is resolved at apply time against the decrypted secrets
//! bundle on [`Context`]).
//!
//! Differences from `@core/file`:
//!
//! - `name` names a `*.age` secret by its file stem (e.g. `api_key` →
//!   `secrets/api_key.age`). Plaintext never flows through the plan.
//! - `path` is **optional**; when omitted, defaults to
//!   [`DEFAULT_PATH_ROOT`]`/<name>` (typically `/run/lusid/secrets/<name>`,
//!   tmpfs on systemd distros). Plans that need the bytes on persistent
//!   disk (config files in `/etc`, etc.) opt in by passing an explicit
//!   absolute `path`.
//! - `mode` defaults to `0o600` (owner read/write, nothing for group/world)
//!   when omitted. `@core/file` leaves mode to the umask.
//!
//! Under the hood this delegates to `@core/file`'s state/change/operation
//! machinery — the atoms produced are ordinary [`FileResource::Secret`]
//! variants, so downstream scheduling and application are identical.
//!
//! Note(cc): the parent directory of `path` must exist before apply.
//! For the default tmpfs root, declare `@core/directory` for
//! `/run/lusid/secrets` (mode `0700`) once per machine. A future
//! refactor could auto-emit that, but it would require either widening
//! the resource type to mix file+directory atoms or restructuring
//! `@core/secret` as a nested plan rather than a leaf module.

use std::fmt::{self, Display};

use async_trait::async_trait;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_ctx::Context;
use lusid_operation::{
    Operation,
    operations::file::{FileGroup, FileMode, FilePath, FileUser},
};
use lusid_params::{ParseError, ParseParams, StructFields};
use lusid_view::impl_display_render;
use rimu::{Spanned, Value};

use crate::ResourceType;
use crate::resources::file::{File, FileChange, FileResource, FileState, FileStateError};

/// Default mode applied when the plan omits `mode`. `0o600` = read/write
/// for the owner only. Overridable by the plan (e.g. a secret that is
/// deliberately group-readable for a multi-user service).
pub const DEFAULT_MODE: u32 = 0o600;

/// Default parent directory when the plan omits `path`. Resolves to a
/// per-secret path of `<DEFAULT_PATH_ROOT>/<name>`. `/run` is tmpfs on
/// every distro lusid targets (Debian, Arch, Fedora, openSUSE, …) — so
/// the default puts plaintext in volatile memory, not on the SD card or
/// hard disk. Backups that snapshot `/etc` won't sweep up the secret.
///
/// Operators who need the secret on persistent disk (a config file in
/// `/etc/...`, a credential the service expects at a specific path) opt
/// in by passing an explicit absolute `path` in the plan.
pub const DEFAULT_PATH_ROOT: &str = "/run/lusid/secrets";

#[derive(Debug, Clone)]
pub struct SecretParams {
    pub name: String,
    pub path: FilePath,
    pub mode: Option<FileMode>,
    pub user: Option<FileUser>,
    pub group: Option<FileGroup>,
}

impl ParseParams for SecretParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let name = fields.required_string("name")?;
        let path = match fields.optional_target_path("path")? {
            Some(p) => FilePath::new(p),
            None => FilePath::new(format!("{DEFAULT_PATH_ROOT}/{name}")),
        };
        let mode = fields.optional_u32("mode")?.map(FileMode::new);
        let user = fields.optional_string("user")?.map(FileUser::new);
        let group = fields.optional_string("group")?.map(FileGroup::new);
        fields.finish()?;
        Ok(SecretParams {
            name,
            path,
            mode,
            user,
            group,
        })
    }
}

impl Display for SecretParams {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Secret(name={}, path={})", self.name, self.path)
    }
}

impl_display_render!(SecretParams);

#[derive(Debug, Clone)]
pub struct Secret;

#[async_trait]
impl ResourceType for Secret {
    const ID: &'static str = "secret";

    type Params = SecretParams;
    type Resource = FileResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        let SecretParams {
            name,
            path,
            mode,
            user,
            group,
        } = params;
        let mode = mode.unwrap_or_else(|| FileMode::new(DEFAULT_MODE));

        let mut nodes = vec![
            CausalityTree::leaf(
                CausalityMeta::id("file".into()),
                FileResource::Secret {
                    name,
                    path: path.clone(),
                },
            ),
            // Always emit a Mode atom: the default mode is a guarantee of this
            // module, not a suggestion. A no-op (already-correct mode) collapses
            // to no change at the change() layer.
            CausalityTree::leaf(
                CausalityMeta::requires(vec!["file".into()]),
                FileResource::Mode {
                    path: path.clone(),
                    mode,
                },
            ),
        ];

        if let Some(user) = user {
            nodes.push(CausalityTree::leaf(
                CausalityMeta::requires(vec!["file".into()]),
                FileResource::User {
                    path: path.clone(),
                    user,
                },
            ));
        }

        if let Some(group) = group {
            nodes.push(CausalityTree::leaf(
                CausalityMeta::requires(vec!["file".into()]),
                FileResource::Group { path, group },
            ));
        }

        nodes
    }

    type State = FileState;
    type StateError = FileStateError;

    async fn state(
        ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        <File as ResourceType>::state(ctx, resource).await
    }

    type Change = FileChange;

    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        <File as ResourceType>::change(resource, state)
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        <File as ResourceType>::operations(change)
    }
}
