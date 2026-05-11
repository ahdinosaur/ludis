//! `@core/secret`: materialise an age-decrypted plaintext onto the target
//! filesystem, referenced by name (agenix-style — the plan names the secret,
//! the plaintext is resolved at apply time against the decrypted secrets
//! bundle on [`Context`]).
//!
//! Differences from `@core/file`:
//!
//! - `name` names a `*.age` secret by its file stem (e.g. `api_key` →
//!   `secrets/api_key.age`). Plaintext never flows through the plan.
//! - `mode` defaults to `0o600` (owner read/write, nothing for group/world)
//!   when omitted. `@core/file` leaves mode to the umask.
//!
//! Under the hood this delegates to `@core/file`'s state/change/operation
//! machinery — the atoms produced are ordinary [`FileResource::Secret`]
//! variants, so downstream scheduling and application are identical.
//!
//! `path` is required and must be an absolute path on the target. Prefer
//! a `/run/...` (tmpfs) location when the consumer doesn't need the
//! plaintext to survive reboots — that keeps plaintext out of backups
//! and off persistent disk.

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
        let path = FilePath::new(fields.required_target_path("path")?);
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
