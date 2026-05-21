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

use crate::{ChangeKind, ResourceChangeTrait, ResourceType};

/// Plan-level parameters for the `@core/flatpak-remote` resource.
///
/// Tagged by `state: "present" | "absent"`. URL is the only mutable field
/// today: titles, gpg keys, and collection-ids are not surfaced.
///
/// TODO(cc): surface title / gpg-key / collection-id. Title and gpg-key
/// are straight additions (extend the Present variant + diff/apply); the
/// `collection-id` interaction noted below in the `.flatpakrepo` Note is
/// the hard part - any drift detection has to either resolve the
/// `.flatpakrepo` and compare, or document that `Modify` won't catch it.
///
/// Note(cc): `flatpak remote-modify --url` refuses when the new repo's
/// `collection-id` differs from the configured one; the stderr will surface
/// the issue, and operators wanting a full re-add should declare
/// `state: "absent"` then `state: "present"`.
///
/// Note(cc): `.flatpakrepo` URLs are interpreted by flatpak as pointers to a
/// metadata file; the URL stored in `flatpak remotes` is the canonical repo
/// URL parsed out of the `.flatpakrepo`, not the declared `.flatpakrepo` URL.
/// We can't compare those for drift, so a `.flatpakrepo`-declared `Present`
/// remote is `Add`-only: once added, we never re-emit `Modify`. Operators
/// who want URL-level drift detection should declare the canonical repo URL
/// directly.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FlatpakRemoteParams {
    Present {
        name: String,
        url: String,
        user: Option<bool>,
    },
    Absent {
        name: String,
        user: Option<bool>,
    },
}

impl ParseParams for FlatpakRemoteParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let state = fields.take_discriminator("state", &["present", "absent"])?;
        let out = match state {
            "present" => FlatpakRemoteParams::Present {
                name: fields.required_string("name")?,
                url: fields.required_string("url")?,
                user: fields.optional_bool("user")?,
            },
            "absent" => FlatpakRemoteParams::Absent {
                name: fields.required_string("name")?,
                user: fields.optional_bool("user")?,
            },
            _ => unreachable!(),
        };
        fields.finish()?;
        Ok(out)
    }
}

impl Display for FlatpakRemoteParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FlatpakRemoteParams::Present { name, url, user } => write!(
                f,
                "FlatpakRemote::Present(name = {name}, url = {url}, user = {user:?})"
            ),
            FlatpakRemoteParams::Absent { name, user } => {
                write!(f, "FlatpakRemote::Absent(name = {name}, user = {user:?})")
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FlatpakRemoteResource {
    Present {
        name: String,
        url: String,
        user: bool,
    },
    Absent {
        name: String,
        user: bool,
    },
}

impl Display for FlatpakRemoteResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FlatpakRemoteResource::Present { name, url, user } => write!(
                f,
                "FlatpakRemote::Present(name = {name}, url = {url}, user = {user})"
            ),
            FlatpakRemoteResource::Absent { name, user } => {
                write!(f, "FlatpakRemote::Absent(name = {name}, user = {user})")
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FlatpakRemoteState {
    Absent,
    Present { url: String },
}

impl Display for FlatpakRemoteState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FlatpakRemoteState::Absent => write!(f, "FlatpakRemote::Absent"),
            FlatpakRemoteState::Present { url } => {
                write!(f, "FlatpakRemote::Present(url = {url})")
            }
        }
    }
}

#[derive(Error, Debug)]
pub enum FlatpakRemoteStateError {
    #[error(transparent)]
    Command(#[from] CommandError),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FlatpakRemoteChange {
    Add {
        name: String,
        url: String,
        user: bool,
    },
    Modify {
        name: String,
        url: String,
        user: bool,
    },
    Remove {
        name: String,
        user: bool,
    },
}

impl Display for FlatpakRemoteChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FlatpakRemoteChange::Add { name, url, user } => write!(
                f,
                "FlatpakRemote::Add(name = {name}, url = {url}, user = {user})"
            ),
            FlatpakRemoteChange::Modify { name, url, user } => write!(
                f,
                "FlatpakRemote::Modify(name = {name}, url = {url}, user = {user})"
            ),
            FlatpakRemoteChange::Remove { name, user } => {
                write!(f, "FlatpakRemote::Remove(name = {name}, user = {user})")
            }
        }
    }
}

impl ResourceChangeTrait for FlatpakRemoteChange {
    fn kind(&self) -> ChangeKind {
        match self {
            FlatpakRemoteChange::Add { .. } => ChangeKind::Added,
            FlatpakRemoteChange::Modify { .. } => ChangeKind::Modified,
            FlatpakRemoteChange::Remove { .. } => ChangeKind::Removed,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FlatpakRemote;

#[async_trait]
impl ResourceType for FlatpakRemote {
    const ID: &'static str = "flatpak-remote";

    type Params = FlatpakRemoteParams;
    type Resource = FlatpakRemoteResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        let resource = match params {
            FlatpakRemoteParams::Present { name, url, user } => FlatpakRemoteResource::Present {
                name,
                url,
                user: user.unwrap_or(false),
            },
            FlatpakRemoteParams::Absent { name, user } => FlatpakRemoteResource::Absent {
                name,
                user: user.unwrap_or(false),
            },
        };
        vec![CausalityTree::leaf(CausalityMeta::default(), resource)]
    }

    type State = FlatpakRemoteState;
    type StateError = FlatpakRemoteStateError;

    /// Probe via `flatpak remotes [--user|--system] --columns=name,url`.
    /// Output is tab-separated, no header when stdout is non-tty (which is
    /// always our case under `Command`). We parse line-by-line and look for
    /// our `name`; absent means the row isn't there, present yields the URL.
    ///
    /// `--columns=name,url` keeps the output stable across flatpak versions
    /// (default columns include localised headers and richer formatting that
    /// can shift). We do not need `LANG=C` - `--columns` produces
    /// machine-friendly output independent of locale.
    ///
    /// We deliberately do not call `flatpak remote-info`: that contacts the
    /// remote over the network, which is slow and irrelevant to local state.
    ///
    /// Fresh-install corner case: `flatpak remotes` opens the OSTree repo
    /// backing this scope (`/var/lib/flatpak/repo` for `--system`,
    /// `$XDG_DATA_HOME/flatpak/repo` for `--user`). On a clean machine the
    /// repo isn't created until the first write (e.g. `remote-add`), and
    /// the probe exits non-zero. We detect that specific stderr and report
    /// Absent; other non-zero exits surface as probe failures. We do not
    /// fall back to "any non-zero exit = Absent" (cf. `podman.rs`) because
    /// flatpak's other failure modes - GPG trust problems, bad scope flags,
    /// broken sudo - carry information operators need.
    ///
    /// Note(cc): an alternative signal would be `tokio::fs::metadata` on
    /// the repo path. That's a more durable check than libostree's error
    /// wording, but requires resolving the user installation root
    /// (`XDG_DATA_HOME` ?? `$HOME/.local/share`) ourselves. Worth a switch
    /// if libostree ever rephrases the open-repo error.
    async fn state(
        _ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        let (name, user) = match resource {
            FlatpakRemoteResource::Present { name, user, .. }
            | FlatpakRemoteResource::Absent { name, user } => (name, *user),
        };

        let mut cmd = Command::new("flatpak");
        cmd.arg(if user { "--user" } else { "--system" })
            .arg("remotes")
            .arg("--columns=name,url");
        let outcome = cmd.outcome().await?;

        if !outcome.status.success() {
            let stderr = String::from_utf8_lossy(&outcome.stderr);
            if is_uninitialized_install_error(&stderr) {
                return Ok(FlatpakRemoteState::Absent);
            }
            return Err(FlatpakRemoteStateError::Command(CommandError::Failure {
                command: cmd.to_string(),
                stderr: stderr.into_owned(),
            }));
        }

        let stdout = String::from_utf8_lossy(&outcome.stdout);

        // Defensive parsing: skip blank lines and lines without a tab. The
        // current `flatpak remotes --columns=name,url` invocation in pipe
        // mode emits no header, but older or future builds might - we'd
        // rather treat an unexpected header line as "doesn't match my name"
        // than crash the probe.
        for line in stdout.lines() {
            let trimmed = line.trim_end();
            if trimmed.is_empty() {
                continue;
            }
            let Some((row_name, row_url)) = parse_remotes_line(trimmed) else {
                continue;
            };
            if row_name == name {
                return Ok(FlatpakRemoteState::Present {
                    url: row_url.to_string(),
                });
            }
        }
        Ok(FlatpakRemoteState::Absent)
    }

    type Change = FlatpakRemoteChange;

    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        match (resource, state) {
            (FlatpakRemoteResource::Absent { .. }, FlatpakRemoteState::Absent) => None,
            (FlatpakRemoteResource::Absent { name, user }, FlatpakRemoteState::Present { .. }) => {
                Some(FlatpakRemoteChange::Remove {
                    name: name.clone(),
                    user: *user,
                })
            }
            (FlatpakRemoteResource::Present { name, url, user }, FlatpakRemoteState::Absent) => {
                Some(FlatpakRemoteChange::Add {
                    name: name.clone(),
                    url: url.clone(),
                    user: *user,
                })
            }
            (
                FlatpakRemoteResource::Present { name, url, user },
                FlatpakRemoteState::Present { url: current_url },
            ) => {
                if url == current_url {
                    return None;
                }
                // `.flatpakrepo` URLs get normalised to the canonical repo
                // URL on first add - declared URL will never match stored
                // URL, so suppress the (otherwise spurious) `Modify`.
                if declared_url_is_flatpakrepo(url) {
                    return None;
                }
                Some(FlatpakRemoteChange::Modify {
                    name: name.clone(),
                    url: url.clone(),
                    user: *user,
                })
            }
        }
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        let op = match change {
            FlatpakRemoteChange::Add { name, url, user } => {
                Operation::Flatpak(FlatpakOperation::AddRemote { name, url, user })
            }
            FlatpakRemoteChange::Modify { name, url, user } => {
                Operation::Flatpak(FlatpakOperation::ModifyRemote { name, url, user })
            }
            FlatpakRemoteChange::Remove { name, user } => {
                Operation::Flatpak(FlatpakOperation::RemoveRemote { name, user })
            }
        };
        vec![CausalityTree::leaf(CausalityMeta::default(), op)]
    }
}

/// Split a `flatpak remotes --columns=name,url` line into `(name, url)`.
/// flatpak emits tab-separated output. Lines without a tab yield `None` -
/// callers (currently only [`FlatpakRemote::state`]) skip those rather than
/// erroring, so a stray header or banner doesn't crash the probe.
fn parse_remotes_line(line: &str) -> Option<(&str, &str)> {
    line.split_once('\t')
}

/// Whether `url` points at a `.flatpakrepo` metadata file rather than at a
/// raw repo. We strip `?query` and `#fragment` before testing so URLs like
/// `https://example.com/foo.flatpakrepo?v=2` are recognised.
fn declared_url_is_flatpakrepo(url: &str) -> bool {
    let url = url.split_once('#').map_or(url, |(p, _)| p);
    let url = url.split_once('?').map_or(url, |(p, _)| p);
    url.to_ascii_lowercase().ends_with(".flatpakrepo")
}

/// Whether a `flatpak remotes` stderr indicates that the OSTree repo backing
/// this scope hasn't been initialised yet. flatpak surfaces this from
/// libostree as `error: While opening repository <repo>: opening repo:
/// opendir(<repo>): No such file or directory`. The `opening repo: opendir(`
/// substring is distinctive to libostree's open-repo failure path
/// (`ostree_repo_open` -> `glnx_throw_errno_prefix`); libc's ENOENT string
/// is the second half. Both pieces are required so an unrelated ENOENT
/// mentioning the word "opendir" elsewhere in flatpak output cannot be
/// misread as a fresh-install signal.
///
/// Note(cc): pathological stderr containing two unrelated errors - one with
/// `opening repo: opendir(` and another with `No such file or directory` -
/// would yield a false positive. The two pieces only co-occur in
/// libostree's open-repo path in practice, but a strict regex would tighten
/// the contract if this ever fires.
fn is_uninitialized_install_error(stderr: &str) -> bool {
    stderr.contains("opening repo: opendir(") && stderr.contains("No such file or directory")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn present(name: &str, url: &str, user: bool) -> FlatpakRemoteResource {
        FlatpakRemoteResource::Present {
            name: name.into(),
            url: url.into(),
            user,
        }
    }

    fn absent(name: &str, user: bool) -> FlatpakRemoteResource {
        FlatpakRemoteResource::Absent {
            name: name.into(),
            user,
        }
    }

    fn state_present(url: &str) -> FlatpakRemoteState {
        FlatpakRemoteState::Present { url: url.into() }
    }

    #[test]
    fn no_change_when_absent_and_absent() {
        let resource = absent("flathub", false);
        assert!(FlatpakRemote::change(&resource, &FlatpakRemoteState::Absent).is_none());
    }

    #[test]
    fn remove_when_absent_but_present() {
        let resource = absent("flathub", false);
        let change =
            FlatpakRemote::change(&resource, &state_present("https://dl.flathub.org/repo/"))
                .expect("change");
        assert!(matches!(change, FlatpakRemoteChange::Remove { .. }));
    }

    #[test]
    fn add_when_present_but_absent() {
        let resource = present("flathub", "https://dl.flathub.org/repo/", false);
        let change = FlatpakRemote::change(&resource, &FlatpakRemoteState::Absent).expect("change");
        match change {
            FlatpakRemoteChange::Add { name, url, user } => {
                assert_eq!(name, "flathub");
                assert_eq!(url, "https://dl.flathub.org/repo/");
                assert!(!user);
            }
            other => panic!("expected Add, got {other:?}"),
        }
    }

    #[test]
    fn no_change_when_url_matches() {
        let resource = present("flathub", "https://dl.flathub.org/repo/", false);
        assert!(
            FlatpakRemote::change(&resource, &state_present("https://dl.flathub.org/repo/"))
                .is_none()
        );
    }

    #[test]
    fn modify_when_url_differs() {
        let resource = present("flathub", "https://example.com/new-repo/", false);
        let change =
            FlatpakRemote::change(&resource, &state_present("https://dl.flathub.org/repo/"))
                .expect("change");
        assert!(matches!(change, FlatpakRemoteChange::Modify { .. }));
    }

    #[test]
    fn suppress_modify_when_declared_url_is_flatpakrepo() {
        // `.flatpakrepo` URLs always look like drift because flatpak normalises
        // them on first add. Keep `Add` working but never re-emit Modify.
        let resource = present(
            "flathub",
            "https://flathub.org/repo/flathub.flatpakrepo",
            false,
        );
        assert!(
            FlatpakRemote::change(&resource, &state_present("https://dl.flathub.org/repo/"))
                .is_none()
        );
    }

    #[test]
    fn add_still_fires_for_flatpakrepo_when_absent() {
        let resource = present(
            "flathub",
            "https://flathub.org/repo/flathub.flatpakrepo",
            false,
        );
        let change = FlatpakRemote::change(&resource, &FlatpakRemoteState::Absent).expect("change");
        assert!(matches!(change, FlatpakRemoteChange::Add { .. }));
    }

    #[test]
    fn flatpakrepo_detection_handles_query_and_fragment() {
        assert!(declared_url_is_flatpakrepo(
            "https://example.com/foo.flatpakrepo"
        ));
        assert!(declared_url_is_flatpakrepo(
            "https://example.com/foo.flatpakrepo?v=2"
        ));
        assert!(declared_url_is_flatpakrepo(
            "https://example.com/foo.flatpakrepo#section"
        ));
        assert!(declared_url_is_flatpakrepo(
            "https://example.com/FOO.FLATPAKREPO"
        ));
        assert!(!declared_url_is_flatpakrepo("https://example.com/repo/"));
        assert!(!declared_url_is_flatpakrepo("https://example.com/repo"));
    }

    #[test]
    fn parse_remotes_line_splits_on_tab() {
        assert_eq!(
            parse_remotes_line("flathub\thttps://dl.flathub.org/repo/"),
            Some(("flathub", "https://dl.flathub.org/repo/"))
        );
    }

    #[test]
    fn parse_remotes_line_rejects_no_tab() {
        assert!(parse_remotes_line("flathub").is_none());
    }

    #[test]
    fn uninitialized_system_install_error_recognised() {
        let stderr = "error: While opening repository /var/lib/flatpak/repo: opening repo: opendir(/var/lib/flatpak/repo): No such file or directory\n";
        assert!(is_uninitialized_install_error(stderr));
    }

    #[test]
    fn uninitialized_user_install_error_recognised() {
        let stderr = "error: While opening repository /home/alice/.local/share/flatpak/repo: opening repo: opendir(/home/alice/.local/share/flatpak/repo): No such file or directory\n";
        assert!(is_uninitialized_install_error(stderr));
    }

    #[test]
    fn unrelated_error_not_treated_as_uninitialized() {
        assert!(!is_uninitialized_install_error(
            "error: GPG signatures found, but none are in trusted keyring\n"
        ));
        assert!(!is_uninitialized_install_error(""));
        // ENOENT mentioned outside the libostree open-repo path does not
        // count - both halves of the pair must be present.
        assert!(!is_uninitialized_install_error(
            "error: No such file or directory\n"
        ));
        assert!(!is_uninitialized_install_error("opening repo: opendir(\n"));
    }
}
