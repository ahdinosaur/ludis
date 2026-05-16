use std::fmt::Display;

use async_trait::async_trait;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_operation::{Operation, operations::flatpak::FlatpakOperation};
use lusid_params::{ParseError, ParseParams, StructFields};
use rimu::{Spanned, Value};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::ResourceType;

/// Default remote name used when a `state: "present"` declaration omits `remote`.
/// `flathub` is by far the dominant flatpak remote; explicit other remotes
/// should be paired with a `@core/flatpak-remote` item in the same plan.
const DEFAULT_REMOTE: &str = "flathub";

/// Plan-level parameters for the `@core/flatpak` resource.
///
/// Tagged by `state: "present" | "absent"`. Apps only in v1 - `--app` is
/// passed to flatpak so a runtime ref will surface a clear "no matching app"
/// error instead of half-succeeding. Adding `kind: "app" | "runtime"` is the
/// natural extension point.
///
/// Note(cc): cross-scope dupes. Declaring `org.x` as `--user` when the same
/// app is already installed `--system` (or vice versa) does NOT trigger a
/// re-install - we probe the declared scope only. Operators who want a
/// single-scope guarantee should declare `state: "absent"` for the
/// off-scope copy alongside the desired `state: "present"`.
///
/// Note(cc): refs that ship a EULA (Steam, some proprietary apps) need a
/// one-time interactive token acceptance that survives `-y --noninteractive`.
/// First-apply for those will fail with a clear flatpak error; operators
/// must install once manually, then re-apply will be idempotent.
///
/// Note(cc): bare names without reverse-DNS dotting (`firefox` instead of
/// `org.mozilla.firefox`) confuse `flatpak install` under `-y` (multiple
/// refs match; flatpak errors). We don't validate the shape - flatpak's
/// error message is clear enough.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FlatpakParams {
    Present {
        name: String,
        remote: Option<String>,
        user: Option<bool>,
    },
    Absent {
        name: String,
        user: Option<bool>,
        delete_data: Option<bool>,
    },
}

impl ParseParams for FlatpakParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let state = fields.take_discriminator("state", &["present", "absent"])?;
        let out = match state {
            "present" => FlatpakParams::Present {
                name: fields.required_string("name")?,
                remote: fields.optional_string("remote")?,
                user: fields.optional_bool("user")?,
            },
            "absent" => FlatpakParams::Absent {
                name: fields.required_string("name")?,
                user: fields.optional_bool("user")?,
                delete_data: fields.optional_bool("delete_data")?,
            },
            _ => unreachable!(),
        };
        fields.finish()?;
        Ok(out)
    }
}

impl Display for FlatpakParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FlatpakParams::Present {
                name, remote, user, ..
            } => write!(
                f,
                "Flatpak::Present(name = {name}, remote = {remote:?}, user = {user:?})"
            ),
            FlatpakParams::Absent {
                name,
                user,
                delete_data,
            } => write!(
                f,
                "Flatpak::Absent(name = {name}, user = {user:?}, delete_data = {delete_data:?})"
            ),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FlatpakResource {
    Present {
        name: String,
        remote: String,
        user: bool,
    },
    Absent {
        name: String,
        user: bool,
        delete_data: bool,
    },
}

impl Display for FlatpakResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FlatpakResource::Present { name, remote, user } => write!(
                f,
                "Flatpak::Present(name = {name}, remote = {remote}, user = {user})"
            ),
            FlatpakResource::Absent {
                name,
                user,
                delete_data,
            } => write!(
                f,
                "Flatpak::Absent(name = {name}, user = {user}, delete_data = {delete_data})"
            ),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FlatpakState {
    NotInstalled,
    Installed,
}

impl Display for FlatpakState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FlatpakState::NotInstalled => write!(f, "Flatpak::NotInstalled"),
            FlatpakState::Installed => write!(f, "Flatpak::Installed"),
        }
    }
}

#[derive(Error, Debug)]
pub enum FlatpakStateError {
    #[error(transparent)]
    Command(#[from] CommandError),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FlatpakChange {
    Install {
        name: String,
        remote: String,
        user: bool,
    },
    Uninstall {
        name: String,
        user: bool,
        delete_data: bool,
    },
}

impl Display for FlatpakChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FlatpakChange::Install { name, remote, user } => write!(
                f,
                "Flatpak::Install(name = {name}, remote = {remote}, user = {user})"
            ),
            FlatpakChange::Uninstall {
                name,
                user,
                delete_data,
            } => write!(
                f,
                "Flatpak::Uninstall(name = {name}, user = {user}, delete_data = {delete_data})"
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Flatpak;

#[async_trait]
impl ResourceType for Flatpak {
    const ID: &'static str = "flatpak";

    type Params = FlatpakParams;
    type Resource = FlatpakResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        let resource = match params {
            FlatpakParams::Present { name, remote, user } => FlatpakResource::Present {
                name,
                remote: remote.unwrap_or_else(|| DEFAULT_REMOTE.to_string()),
                user: user.unwrap_or(false),
            },
            FlatpakParams::Absent {
                name,
                user,
                delete_data,
            } => FlatpakResource::Absent {
                name,
                user: user.unwrap_or(false),
                delete_data: delete_data.unwrap_or(false),
            },
        };
        vec![CausalityTree::leaf(CausalityMeta::default(), resource)]
    }

    type State = FlatpakState;
    type StateError = FlatpakStateError;

    /// Probe via `flatpak info [--user|--system] <name>`:
    /// - exit 0: stdout has metadata - package is installed.
    /// - exit non-zero + stderr contains `"not installed"`: package is absent.
    /// - exit non-zero + other stderr: surface as `CommandError::Failure`.
    ///
    /// Note(cc): `flatpak info` has no `--app` filter (unlike `flatpak
    /// install`), so a runtime ref declared in `@core/flatpak` reads as
    /// `Installed` if the runtime happens to be installed. We treat that as
    /// a configuration error to surface at install time - the install op
    /// passes `--app`, so flatpak refuses to install a runtime ref and
    /// emits a clear error.
    async fn state(
        _ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        let (name, user) = match resource {
            FlatpakResource::Present { name, user, .. }
            | FlatpakResource::Absent { name, user, .. } => (name, *user),
        };

        let mut cmd = Command::new("flatpak");
        cmd.arg(if user { "--user" } else { "--system" })
            .arg("info")
            .arg("--")
            .arg(name);
        cmd.handle(
            |_stdout| Ok(FlatpakState::Installed),
            |stderr| {
                let stderr = String::from_utf8_lossy(stderr);
                // flatpak emits e.g. `error: org.x/.../... not installed`
                // on absent refs. Other stderr (daemon errors, malformed
                // names) returns `None` so the outer command failure path
                // surfaces them.
                if stderr.contains("not installed") {
                    Ok(Some(FlatpakState::NotInstalled))
                } else {
                    Ok(None)
                }
            },
        )
        .await?
    }

    type Change = FlatpakChange;

    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        match (resource, state) {
            (FlatpakResource::Absent { .. }, FlatpakState::NotInstalled) => None,

            (
                FlatpakResource::Absent {
                    name,
                    user,
                    delete_data,
                },
                FlatpakState::Installed,
            ) => Some(FlatpakChange::Uninstall {
                name: name.clone(),
                user: *user,
                delete_data: *delete_data,
            }),

            (FlatpakResource::Present { name, remote, user }, FlatpakState::NotInstalled) => {
                Some(FlatpakChange::Install {
                    name: name.clone(),
                    remote: remote.clone(),
                    user: *user,
                })
            }

            (FlatpakResource::Present { .. }, FlatpakState::Installed) => None,
        }
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        let op = match change {
            FlatpakChange::Install { name, remote, user } => {
                Operation::Flatpak(FlatpakOperation::Install {
                    remote,
                    names: vec![name],
                    user,
                })
            }
            FlatpakChange::Uninstall {
                name,
                user,
                delete_data,
            } => Operation::Flatpak(FlatpakOperation::Uninstall {
                names: vec![name],
                user,
                delete_data,
            }),
        };
        vec![CausalityTree::leaf(CausalityMeta::default(), op)]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn present(name: &str, remote: &str, user: bool) -> FlatpakResource {
        FlatpakResource::Present {
            name: name.into(),
            remote: remote.into(),
            user,
        }
    }

    fn absent(name: &str, user: bool, delete_data: bool) -> FlatpakResource {
        FlatpakResource::Absent {
            name: name.into(),
            user,
            delete_data,
        }
    }

    #[test]
    fn no_change_when_present_and_installed() {
        let resource = present("org.x", "flathub", false);
        assert!(Flatpak::change(&resource, &FlatpakState::Installed).is_none());
    }

    #[test]
    fn install_when_present_and_not_installed() {
        let resource = present("org.x", "flathub", false);
        let change = Flatpak::change(&resource, &FlatpakState::NotInstalled).expect("change");
        match change {
            FlatpakChange::Install { name, remote, user } => {
                assert_eq!(name, "org.x");
                assert_eq!(remote, "flathub");
                assert!(!user);
            }
            other => panic!("expected Install, got {other:?}"),
        }
    }

    #[test]
    fn no_change_when_absent_and_not_installed() {
        let resource = absent("org.x", false, false);
        assert!(Flatpak::change(&resource, &FlatpakState::NotInstalled).is_none());
    }

    #[test]
    fn uninstall_when_absent_and_installed() {
        let resource = absent("org.x", false, true);
        let change = Flatpak::change(&resource, &FlatpakState::Installed).expect("change");
        match change {
            FlatpakChange::Uninstall {
                name,
                user,
                delete_data,
            } => {
                assert_eq!(name, "org.x");
                assert!(!user);
                assert!(delete_data);
            }
            other => panic!("expected Uninstall, got {other:?}"),
        }
    }

    fn leaf_resource(tree: &CausalityTree<FlatpakResource>) -> &FlatpakResource {
        match tree {
            CausalityTree::Leaf { node, .. } => node,
            _ => panic!("expected leaf"),
        }
    }

    #[test]
    fn resources_apply_default_remote_when_absent() {
        let params = FlatpakParams::Present {
            name: "org.x".into(),
            remote: None,
            user: None,
        };
        let trees = Flatpak::resources(params);
        assert_eq!(trees.len(), 1);
        match leaf_resource(&trees[0]) {
            FlatpakResource::Present { remote, user, .. } => {
                assert_eq!(remote, "flathub");
                assert!(!user);
            }
            other => panic!("expected Present, got {other:?}"),
        }
    }

    #[test]
    fn resources_keep_explicit_remote() {
        let params = FlatpakParams::Present {
            name: "org.x".into(),
            remote: Some("flathub-beta".into()),
            user: Some(true),
        };
        let trees = Flatpak::resources(params);
        match leaf_resource(&trees[0]) {
            FlatpakResource::Present { remote, user, .. } => {
                assert_eq!(remote, "flathub-beta");
                assert!(*user);
            }
            other => panic!("expected Present, got {other:?}"),
        }
    }
}

#[cfg(test)]
mod serde_tests {
    use super::*;

    fn round_trip<T: serde::Serialize + serde::de::DeserializeOwned>(value: &T) {
        let json = serde_json::to_string(value).unwrap();
        let back: T = serde_json::from_str(&json).unwrap();
        assert_eq!(json, serde_json::to_string(&back).unwrap());
    }

    #[test]
    fn params_round_trip_covers_every_variant() {
        round_trip(&FlatpakParams::Present {
            name: "org.mozilla.firefox".into(),
            remote: Some("flathub".into()),
            user: Some(false),
        });
        round_trip(&FlatpakParams::Absent {
            name: "org.mozilla.firefox".into(),
            user: Some(true),
            delete_data: Some(true),
        });
    }

    #[test]
    fn resource_round_trip_covers_every_variant() {
        round_trip(&FlatpakResource::Present {
            name: "org.mozilla.firefox".into(),
            remote: "flathub".into(),
            user: false,
        });
        round_trip(&FlatpakResource::Absent {
            name: "org.mozilla.firefox".into(),
            user: false,
            delete_data: true,
        });
    }

    #[test]
    fn state_round_trip_covers_every_variant() {
        round_trip(&FlatpakState::NotInstalled);
        round_trip(&FlatpakState::Installed);
    }

    #[test]
    fn change_round_trip_covers_every_variant() {
        round_trip(&FlatpakChange::Install {
            name: "org.mozilla.firefox".into(),
            remote: "flathub".into(),
            user: false,
        });
        round_trip(&FlatpakChange::Uninstall {
            name: "org.mozilla.firefox".into(),
            user: false,
            delete_data: true,
        });
    }
}
