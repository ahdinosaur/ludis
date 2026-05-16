use async_trait::async_trait;
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_view::impl_display_render;
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, collections::BTreeSet, fmt::Display, pin::Pin};
use thiserror::Error;
use tokio::process::{ChildStderr, ChildStdout};
use tracing::info;

use crate::OperationType;

/// Operations against the `flatpak(1)` CLI.
///
/// Scope (`user: true|false`) maps to flatpak's `--user`/`--system` flags and
/// determines whether we wrap the invocation in `sudo`: the system installation
/// lives under `/var/lib/flatpak`, the per-user installation under
/// `$XDG_DATA_HOME/flatpak` (which falls back to `~/.local/share/flatpak`).
///
/// Note(cc): `--user` installs over SSH inherit the operator's `$HOME` /
/// `$XDG_DATA_HOME` from the apply environment. If a future remote-apply scrubs
/// env vars, `--user` installs will silently land in the wrong directory or
/// fail outright.
///
/// Note(cc): concurrent flatpak invocations (e.g. a GNOME Software update timer
/// or a parallel `flatpak update`) hold the system lock, which makes this
/// command block rather than error. Within one lusid epoch operations run
/// serially, so we don't deadlock against ourselves, but apply against a
/// machine with active flatpak daemons can stall.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FlatpakOperation {
    /// Install one or more refs from `remote` into the named scope. Batched at
    /// merge time when multiple `@core/flatpak` resources share the same
    /// `(user, remote)`. Apps only - `--app` is passed explicitly so a runtime
    /// ref declared by mistake fails fast with flatpak's own error.
    Install {
        remote: String,
        names: Vec<String>,
        user: bool,
    },

    /// Uninstall one or more refs from the named scope. `delete_data: true`
    /// adds `--delete-data`, which removes `~/.var/app/<id>` for `--user`
    /// installs (or `/var/lib/flatpak/app/<id>` for `--system`). Default
    /// (`false`) preserves user data, mirroring `userdel` without `-r`.
    Uninstall {
        names: Vec<String>,
        user: bool,
        delete_data: bool,
    },

    /// Add a remote with the given name and URL. No `--if-not-exists` - the
    /// resource layer only emits `Add` when state is `Absent`.
    AddRemote {
        name: String,
        url: String,
        user: bool,
    },

    /// Update an existing remote's URL. Other remote properties (title,
    /// gpg-key, collection-id) are not refreshed.
    ///
    /// Note(cc): `flatpak remote-modify --url` refuses when the new URL's
    /// `collection-id` differs from the configured one. Stderr surfaces the
    /// error; operators wanting a full re-add should declare
    /// `state: "absent"` then `state: "present"`.
    ModifyRemote {
        name: String,
        url: String,
        user: bool,
    },

    /// Remove a remote. Deliberately not `--force`: that would also uninstall
    /// every ref originating from the remote, which is destructive and not
    /// what "make this remote absent" should imply. Operators should declare
    /// dependent packages `absent` first via plan-level `requires`.
    RemoveRemote { name: String, user: bool },
}

impl Display for FlatpakOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Render the actual scope flag the command runs with so the user-
        // facing diff matches the executed argv (the operation always
        // passes `--user` or `--system` - never neither).
        let scope = |user: bool| if user { " --user" } else { " --system" };
        match self {
            FlatpakOperation::Install {
                remote,
                names,
                user,
            } => write!(
                f,
                "Flatpak::Install(remote = {remote}, names = [{}]){}",
                names.join(", "),
                scope(*user),
            ),
            FlatpakOperation::Uninstall {
                names,
                user,
                delete_data,
            } => write!(
                f,
                "Flatpak::Uninstall(names = [{}], delete_data = {delete_data}){}",
                names.join(", "),
                scope(*user),
            ),
            FlatpakOperation::AddRemote { name, url, user } => write!(
                f,
                "Flatpak::AddRemote(name = {name}, url = {url}){}",
                scope(*user),
            ),
            FlatpakOperation::ModifyRemote { name, url, user } => write!(
                f,
                "Flatpak::ModifyRemote(name = {name}, url = {url}){}",
                scope(*user),
            ),
            FlatpakOperation::RemoveRemote { name, user } => {
                write!(f, "Flatpak::RemoveRemote({name}){}", scope(*user))
            }
        }
    }
}

impl_display_render!(FlatpakOperation);

#[derive(Error, Debug)]
pub enum FlatpakApplyError {
    #[error(transparent)]
    Command(#[from] CommandError),
}

#[derive(Debug, Clone)]
pub struct Flatpak;

#[async_trait]
impl OperationType for Flatpak {
    type Operation = FlatpakOperation;

    /// Coalesce installs/uninstalls within an epoch. Remote-management ops
    /// pass through untouched (each targets a single remote by name).
    ///
    /// Installs group by `(user, remote)`: a single `flatpak install <remote>
    /// a b c` invocation requires every ref to be on the same remote, in the
    /// same scope.
    ///
    /// Uninstalls group by `(user, delete_data)`: `--delete-data` is per-
    /// invocation, so two uninstalls with different `delete_data` cannot be
    /// merged.
    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation> {
        let mut installs: BTreeMap<(bool, String), BTreeSet<String>> = BTreeMap::new();
        let mut uninstalls: BTreeMap<(bool, bool), BTreeSet<String>> = BTreeMap::new();
        let mut others: Vec<FlatpakOperation> = Vec::new();

        for operation in operations {
            match operation {
                FlatpakOperation::Install {
                    remote,
                    names,
                    user,
                } => {
                    let bucket = installs.entry((user, remote)).or_default();
                    for name in names {
                        bucket.insert(name);
                    }
                }
                FlatpakOperation::Uninstall {
                    names,
                    user,
                    delete_data,
                } => {
                    let bucket = uninstalls.entry((user, delete_data)).or_default();
                    for name in names {
                        bucket.insert(name);
                    }
                }
                other => others.push(other),
            }
        }

        let mut out: Vec<FlatpakOperation> = Vec::new();
        for ((user, remote), names) in installs {
            out.push(FlatpakOperation::Install {
                remote,
                names: names.into_iter().collect(),
                user,
            });
        }
        for ((user, delete_data), names) in uninstalls {
            out.push(FlatpakOperation::Uninstall {
                names: names.into_iter().collect(),
                user,
                delete_data,
            });
        }
        out.extend(others);
        out
    }

    type ApplyOutput = Pin<Box<dyn Future<Output = Result<(), Self::ApplyError>> + Send + 'static>>;
    type ApplyError = FlatpakApplyError;
    type ApplyStdout = ChildStdout;
    type ApplyStderr = ChildStderr;

    async fn apply(
        _ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError> {
        match operation {
            FlatpakOperation::Install {
                remote,
                names,
                user,
            } => {
                info!(user, remote = %remote, "[flatpak] install: {}", names.join(", "));
                // `-y` (assumeyes) covers install confirmations; `--noninteractive`
                // (flatpak ≥ 1.10) additionally suppresses authenticator and
                // "unverified author" prompts that `-y` alone misses. EULA-
                // gated refs still fail under both - that's by design upstream.
                //
                // `--app` is explicit so a runtime ref declared in the plan
                // fails fast with flatpak's own error rather than half-
                // succeeding via the install-default.
                //
                // Both `remote` and `names` go AFTER the `--` separator so
                // a user-supplied value beginning with `-` cannot be
                // misparsed as an option.
                let mut cmd = Command::new("flatpak");
                scope_arg(&mut cmd, *user);
                cmd.arg("install")
                    .arg("-y")
                    .arg("--noninteractive")
                    .arg("--app")
                    .arg("--")
                    .arg(remote)
                    .args(names);
                let output = run(cmd, *user).await?;
                Ok((
                    Box::pin(async move {
                        output.status.await?;
                        Ok(())
                    }),
                    output.stdout,
                    output.stderr,
                ))
            }
            FlatpakOperation::Uninstall {
                names,
                user,
                delete_data,
            } => {
                info!(
                    user,
                    delete_data,
                    "[flatpak] uninstall: {}",
                    names.join(", ")
                );
                // `--app` mirrors the install path - runtimes are out of
                // scope for `@core/flatpak`, so an uninstall request for a
                // name that flatpak might resolve as a runtime fails loudly
                // rather than removing something we didn't manage.
                let mut cmd = Command::new("flatpak");
                scope_arg(&mut cmd, *user);
                cmd.arg("uninstall")
                    .arg("-y")
                    .arg("--noninteractive")
                    .arg("--app");
                if *delete_data {
                    cmd.arg("--delete-data");
                }
                cmd.arg("--").args(names);
                let output = run(cmd, *user).await?;
                Ok((
                    Box::pin(async move {
                        output.status.await?;
                        Ok(())
                    }),
                    output.stdout,
                    output.stderr,
                ))
            }
            FlatpakOperation::AddRemote { name, url, user } => {
                info!(user, name = %name, url = %url, "[flatpak] add remote");
                let mut cmd = Command::new("flatpak");
                scope_arg(&mut cmd, *user);
                cmd.arg("remote-add").arg("--").arg(name).arg(url);
                let output = run(cmd, *user).await?;
                Ok((
                    Box::pin(async move {
                        output.status.await?;
                        Ok(())
                    }),
                    output.stdout,
                    output.stderr,
                ))
            }
            FlatpakOperation::ModifyRemote { name, url, user } => {
                info!(user, name = %name, url = %url, "[flatpak] modify remote");
                let mut cmd = Command::new("flatpak");
                scope_arg(&mut cmd, *user);
                cmd.arg("remote-modify")
                    .arg(format!("--url={url}"))
                    .arg("--")
                    .arg(name);
                let output = run(cmd, *user).await?;
                Ok((
                    Box::pin(async move {
                        output.status.await?;
                        Ok(())
                    }),
                    output.stdout,
                    output.stderr,
                ))
            }
            FlatpakOperation::RemoveRemote { name, user } => {
                info!(user, name = %name, "[flatpak] remove remote");
                let mut cmd = Command::new("flatpak");
                scope_arg(&mut cmd, *user);
                cmd.arg("remote-delete").arg("--").arg(name);
                let output = run(cmd, *user).await?;
                Ok((
                    Box::pin(async move {
                        output.status.await?;
                        Ok(())
                    }),
                    output.stdout,
                    output.stderr,
                ))
            }
        }
    }
}

/// Push `--user` or `--system` immediately after the program name. flatpak
/// accepts options in any order, but keeping `--user`/`--system` first
/// matches the form used in flatpak's own docs and our `Display` impl.
fn scope_arg(cmd: &mut Command, user: bool) {
    cmd.arg(if user { "--user" } else { "--system" });
}

/// Wrap in `sudo -n` for `--system` operations. `--user` operations run as
/// the invoking operator and writing under `~/.local/share/flatpak` doesn't
/// need privilege escalation.
async fn run(cmd: Command, user: bool) -> Result<lusid_cmd::CommandOutput, CommandError> {
    let mut cmd = if user { cmd } else { cmd.sudo() };
    cmd.output().await
}

#[cfg(test)]
mod tests {
    use super::*;

    fn install(remote: &str, names: &[&str], user: bool) -> FlatpakOperation {
        FlatpakOperation::Install {
            remote: remote.into(),
            names: names.iter().map(|s| s.to_string()).collect(),
            user,
        }
    }

    fn uninstall(names: &[&str], user: bool, delete_data: bool) -> FlatpakOperation {
        FlatpakOperation::Uninstall {
            names: names.iter().map(|s| s.to_string()).collect(),
            user,
            delete_data,
        }
    }

    #[test]
    fn merge_same_user_same_remote_collapses_to_one_install() {
        let ops = vec![
            install("flathub", &["org.a"], false),
            install("flathub", &["org.b"], false),
        ];
        let merged = Flatpak::merge(ops);
        assert_eq!(merged.len(), 1);
        match &merged[0] {
            FlatpakOperation::Install {
                remote,
                names,
                user,
            } => {
                assert_eq!(remote, "flathub");
                // BTreeSet ordering means names come back sorted.
                assert_eq!(names, &vec!["org.a".to_string(), "org.b".to_string()]);
                assert!(!user);
            }
            other => panic!("expected Install, got {other:?}"),
        }
    }

    #[test]
    fn merge_dedupes_duplicate_package_names() {
        let ops = vec![
            install("flathub", &["org.a"], false),
            install("flathub", &["org.a"], false),
        ];
        let merged = Flatpak::merge(ops);
        assert_eq!(merged.len(), 1);
        if let FlatpakOperation::Install { names, .. } = &merged[0] {
            assert_eq!(names, &vec!["org.a".to_string()]);
        } else {
            panic!("expected Install");
        }
    }

    #[test]
    fn merge_keeps_different_remotes_separate() {
        let ops = vec![
            install("flathub", &["org.a"], false),
            install("flathub-beta", &["org.b"], false),
        ];
        let merged = Flatpak::merge(ops);
        assert_eq!(merged.len(), 2);
        // BTreeMap orders by key - `(false, "flathub")` sorts before
        // `(false, "flathub-beta")` lexicographically? Actually "flathub" <
        // "flathub-beta", so flathub comes first.
        let remotes: Vec<&str> = merged
            .iter()
            .filter_map(|op| match op {
                FlatpakOperation::Install { remote, .. } => Some(remote.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(remotes, vec!["flathub", "flathub-beta"]);
    }

    #[test]
    fn merge_keeps_different_user_scopes_separate() {
        let ops = vec![
            install("flathub", &["org.a"], false),
            install("flathub", &["org.b"], true),
        ];
        let merged = Flatpak::merge(ops);
        assert_eq!(merged.len(), 2);
        let user_flags: Vec<bool> = merged
            .iter()
            .filter_map(|op| match op {
                FlatpakOperation::Install { user, .. } => Some(*user),
                _ => None,
            })
            .collect();
        // BTreeMap key (user, remote): (false, "flathub") < (true, "flathub").
        assert_eq!(user_flags, vec![false, true]);
    }

    #[test]
    fn merge_uninstalls_group_by_user_and_delete_data() {
        let ops = vec![
            uninstall(&["org.a"], false, false),
            uninstall(&["org.b"], false, false),
            uninstall(&["org.c"], false, true),
        ];
        let merged = Flatpak::merge(ops);
        // Two buckets: (false, false) gets a+b, (false, true) gets c.
        assert_eq!(merged.len(), 2);
        let mut saw_merged = false;
        let mut saw_purge = false;
        for op in merged {
            if let FlatpakOperation::Uninstall {
                names,
                delete_data,
                user,
            } = op
            {
                assert!(!user);
                if delete_data {
                    assert_eq!(names, vec!["org.c".to_string()]);
                    saw_purge = true;
                } else {
                    assert_eq!(names, vec!["org.a".to_string(), "org.b".to_string()]);
                    saw_merged = true;
                }
            } else {
                panic!("expected Uninstall");
            }
        }
        assert!(saw_merged && saw_purge);
    }

    #[test]
    fn merge_remote_ops_pass_through() {
        let ops = vec![
            FlatpakOperation::AddRemote {
                name: "flathub".into(),
                url: "https://dl.flathub.org/repo/".into(),
                user: false,
            },
            FlatpakOperation::RemoveRemote {
                name: "old".into(),
                user: false,
            },
        ];
        let merged = Flatpak::merge(ops);
        assert_eq!(merged.len(), 2);
    }

    #[test]
    fn merge_empty_in_empty_out() {
        assert!(Flatpak::merge(Vec::new()).is_empty());
    }
}

#[cfg(test)]
mod serde_tests {
    use super::*;

    fn round_trip(op: FlatpakOperation) {
        let json = serde_json::to_string(&op).unwrap();
        let back: FlatpakOperation = serde_json::from_str(&json).unwrap();
        assert_eq!(json, serde_json::to_string(&back).unwrap());
    }

    #[test]
    fn round_trip_install_uninstall() {
        round_trip(FlatpakOperation::Install {
            remote: "flathub".into(),
            names: vec!["org.gnome.Calculator".into()],
            user: true,
        });
        round_trip(FlatpakOperation::Uninstall {
            names: vec!["org.gnome.Calculator".into()],
            user: false,
            delete_data: true,
        });
    }

    #[test]
    fn round_trip_remotes() {
        round_trip(FlatpakOperation::AddRemote {
            name: "flathub".into(),
            url: "https://dl.flathub.org/repo/flathub.flatpakrepo".into(),
            user: false,
        });
        round_trip(FlatpakOperation::ModifyRemote {
            name: "flathub".into(),
            url: "https://dl.flathub.org/repo/".into(),
            user: false,
        });
        round_trip(FlatpakOperation::RemoveRemote {
            name: "old".into(),
            user: true,
        });
    }
}
