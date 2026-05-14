use std::{collections::BTreeSet, fmt::Display};

use async_trait::async_trait;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_operation::{Operation, operations::group::GroupOperation};
use lusid_params::{ParseError, ParseParams, StructFields};
use lusid_view::impl_display_render;
use rimu::{Spanned, Value};
use thiserror::Error;

use crate::ResourceType;

/// Plan-level parameters for the `@resource/group` resource.
///
/// Tagged by `state: "present" | "absent"`. Mirrors the shape used by Salt
/// (`group.present`) and Ansible (`ansible.builtin.group`), with an additional
/// `append_users` field to declaratively guarantee supplementary group membership.
#[derive(Debug, Clone)]
pub enum GroupParams {
    Present {
        name: String,
        gid: Option<u32>,
        system: Option<bool>,
        /// Users that must belong to this group as supplementary members. Missing
        /// users are added; users already in the group are left alone, and users
        /// not listed here are *not* removed — this is append-only, not an exact
        /// list. `None` or an empty list means "don't touch membership". Users
        /// whose *primary* group is this one are unaffected — primary membership
        /// is set on the `user` resource, not here.
        append_users: Option<Vec<String>>,
    },
    Absent {
        name: String,
    },
}

impl ParseParams for GroupParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let state = fields.take_discriminator("state", &["present", "absent"])?;
        let out = match state {
            "present" => GroupParams::Present {
                name: fields.required_string("name")?,
                gid: fields.optional_u32("gid")?,
                system: fields.optional_bool("system")?,
                append_users: fields.optional_string_list("append_users")?,
            },
            "absent" => GroupParams::Absent {
                name: fields.required_string("name")?,
            },
            _ => unreachable!(),
        };
        fields.finish()?;
        Ok(out)
    }
}

impl Display for GroupParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GroupParams::Present { name, .. } => write!(f, "Group::Present(name = {name})"),
            GroupParams::Absent { name } => write!(f, "Group::Absent(name = {name})"),
        }
    }
}

impl_display_render!(GroupParams);

#[derive(Debug, Clone)]
pub enum GroupResource {
    Present {
        name: String,
        gid: Option<u32>,
        system: bool,
        append_users: Option<Vec<String>>,
    },
    Absent {
        name: String,
    },
}

impl Display for GroupResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GroupResource::Present { name, .. } => write!(f, "Group::Present(name = {name})"),
            GroupResource::Absent { name } => write!(f, "Group::Absent(name = {name})"),
        }
    }
}

impl_display_render!(GroupResource);

#[derive(Debug, Clone)]
pub enum GroupState {
    Absent,
    Present { gid: u32, members: Vec<String> },
}

impl Display for GroupState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GroupState::Absent => write!(f, "Group::Absent"),
            GroupState::Present { gid, members } => write!(
                f,
                "Group::Present(gid = {gid}, members = [{}])",
                members.join(", ")
            ),
        }
    }
}

impl_display_render!(GroupState);

#[derive(Error, Debug)]
pub enum GroupStateError {
    #[error(transparent)]
    Command(#[from] CommandError),

    #[error("failed to parse getent group output: {output}")]
    ParseGroup { output: String },

    #[error("failed to parse gid `{value}`: {source}")]
    ParseGid {
        value: String,
        #[source]
        source: std::num::ParseIntError,
    },
}

#[derive(Debug, Clone)]
pub enum GroupChange {
    Create {
        name: String,
        gid: Option<u32>,
        system: bool,
        /// Users to append as supplementary members after the group is created.
        /// Empty means "no `gpasswd` step".
        append_users: Vec<String>,
    },
    Modify {
        name: String,
        gid: Option<u32>,
        /// Declared users that are not currently members and need to be appended.
        /// Empty means membership is already fine.
        append_users: Vec<String>,
    },
    Delete {
        name: String,
    },
}

impl Display for GroupChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GroupChange::Create { name, .. } => write!(f, "Group::Create(name = {name})"),
            GroupChange::Modify { name, .. } => write!(f, "Group::Modify(name = {name})"),
            GroupChange::Delete { name } => write!(f, "Group::Delete(name = {name})"),
        }
    }
}

impl_display_render!(GroupChange);

#[derive(Debug, Clone)]
pub struct Group;

#[async_trait]
impl ResourceType for Group {
    const ID: &'static str = "group";

    type Params = GroupParams;
    type Resource = GroupResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        let resource = match params {
            GroupParams::Present {
                name,
                gid,
                system,
                append_users,
            } => GroupResource::Present {
                name,
                gid,
                system: system.unwrap_or(false),
                append_users,
            },
            GroupParams::Absent { name } => GroupResource::Absent { name },
        };
        vec![CausalityTree::leaf(CausalityMeta::default(), resource)]
    }

    type State = GroupState;
    type StateError = GroupStateError;

    async fn state(
        _ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        let name = match resource {
            GroupResource::Present { name, .. } | GroupResource::Absent { name } => name,
        };

        let Some(entry) = get_group_entry(name).await? else {
            return Ok(GroupState::Absent);
        };

        Ok(GroupState::Present {
            gid: entry.gid,
            members: entry.members,
        })
    }

    type Change = GroupChange;

    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        match (resource, state) {
            (GroupResource::Absent { .. }, GroupState::Absent) => None,

            (GroupResource::Absent { name }, GroupState::Present { .. }) => {
                Some(GroupChange::Delete { name: name.clone() })
            }

            (
                GroupResource::Present {
                    name,
                    gid,
                    system,
                    append_users,
                },
                GroupState::Absent,
            ) => Some(GroupChange::Create {
                name: name.clone(),
                gid: *gid,
                system: *system,
                append_users: append_users.clone().unwrap_or_default(),
            }),

            (
                GroupResource::Present {
                    name,
                    gid,
                    system: _,
                    append_users,
                },
                GroupState::Present {
                    gid: current_gid,
                    members: current_members,
                },
            ) => {
                let gid_change = gid.filter(|declared| *declared != *current_gid);

                let append_users_change: Vec<String> = append_users
                    .as_ref()
                    .map(|declared| {
                        let current: BTreeSet<&str> =
                            current_members.iter().map(String::as_str).collect();
                        let mut seen: BTreeSet<&str> = BTreeSet::new();
                        declared
                            .iter()
                            .filter(|user| {
                                !current.contains(user.as_str()) && seen.insert(user.as_str())
                            })
                            .cloned()
                            .collect()
                    })
                    .unwrap_or_default();

                if gid_change.is_none() && append_users_change.is_empty() {
                    None
                } else {
                    Some(GroupChange::Modify {
                        name: name.clone(),
                        gid: gid_change,
                        append_users: append_users_change,
                    })
                }
            }
        }
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        match change {
            GroupChange::Create {
                name,
                gid,
                system,
                append_users,
            } => {
                let mut ops: Vec<CausalityTree<Operation>> = Vec::new();
                let needs_add_users = !append_users.is_empty();

                let add_meta = if needs_add_users {
                    CausalityMeta::id("add".into())
                } else {
                    CausalityMeta::default()
                };
                ops.push(CausalityTree::leaf(
                    add_meta,
                    Operation::Group(GroupOperation::Add {
                        name: name.clone(),
                        gid,
                        system,
                    }),
                ));

                for user in append_users {
                    ops.push(CausalityTree::leaf(
                        CausalityMeta::requires(vec!["add".into()]),
                        Operation::Group(GroupOperation::AddUser {
                            name: name.clone(),
                            user,
                        }),
                    ));
                }

                ops
            }
            GroupChange::Modify {
                name,
                gid,
                append_users,
            } => {
                let mut ops: Vec<CausalityTree<Operation>> = Vec::new();
                let has_gid = gid.is_some();
                let has_users = !append_users.is_empty();

                if has_gid {
                    // Order groupmod before gpasswd when both are emitted: both edit
                    // /etc/group, and while shadow-utils serializes them via the
                    // password-file lock, making the dependency explicit keeps the
                    // plan correct regardless of intra-epoch scheduling.
                    let meta = if has_users {
                        CausalityMeta::id("modify".into())
                    } else {
                        CausalityMeta::default()
                    };
                    ops.push(CausalityTree::leaf(
                        meta,
                        Operation::Group(GroupOperation::Modify {
                            name: name.clone(),
                            gid,
                        }),
                    ));
                }
                for user in append_users {
                    let meta = if has_gid {
                        CausalityMeta::requires(vec!["modify".into()])
                    } else {
                        CausalityMeta::default()
                    };
                    ops.push(CausalityTree::leaf(
                        meta,
                        Operation::Group(GroupOperation::AddUser {
                            name: name.clone(),
                            user,
                        }),
                    ));
                }
                ops
            }
            GroupChange::Delete { name } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                Operation::Group(GroupOperation::Delete { name }),
            )],
        }
    }
}

/// One entry from `/etc/group`, surfaced as typed fields.
struct GroupEntry {
    gid: u32,
    members: Vec<String>,
}

/// Read a single group from the NSS group database. `None` if the group doesn't
/// exist (which `getent` signals by exiting non-zero).
async fn get_group_entry(name: &str) -> Result<Option<GroupEntry>, GroupStateError> {
    let outcome = Command::new("getent")
        .args(["group", name])
        .outcome()
        .await?;
    if !outcome.status.success() {
        return Ok(None);
    }

    let stdout = String::from_utf8_lossy(&outcome.stdout);

    // group format: name:password:gid:member1,member2
    // Note(cc): the member list after the third colon may contain commas but not
    // further colons, so a 4-field split is safe.
    let line = stdout.lines().next().unwrap_or("");
    let parts: Vec<&str> = line.splitn(4, ':').collect();
    if parts.len() < 4 {
        return Err(GroupStateError::ParseGroup {
            output: stdout.into_owned(),
        });
    }

    let gid = parts[2]
        .parse::<u32>()
        .map_err(|source| GroupStateError::ParseGid {
            value: parts[2].to_string(),
            source,
        })?;

    let members: Vec<String> = parts[3]
        .trim_end()
        .split(',')
        .filter(|member| !member.is_empty())
        .map(str::to_string)
        .collect();

    Ok(Some(GroupEntry { gid, members }))
}
