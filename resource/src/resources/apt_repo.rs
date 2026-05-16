use std::fmt::Display;
use std::path::PathBuf;

use async_trait::async_trait;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_ctx::Context;
use lusid_fs::{self as fs, FsError};
use lusid_operation::{
    Operation,
    operations::{apt_repo::AptRepoOperation, file::FilePath},
};
use lusid_params::{ParseError, ParseParams, StructFields};
use lusid_view::impl_display_render;
use rimu::{Spanned, Value};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::ResourceType;

const KEYRINGS_DIR: &str = "/etc/apt/keyrings";
const SOURCES_LIST_DIR: &str = "/etc/apt/sources.list.d";

// TODO(cc): accept `String | List<String>` for `uris` / `suites` / `components`.
// Today every list-shaped field requires a list literal; allowing a bare string
// would let single-entry plans skip the brackets. The hand-written parser in
// `ParseParams for AptRepoParams` would gain a `parse_string_or_list(value)` helper.
//
// TODO(cc): validate `name` at param-time with a filesystem-safe regex
// (`^[a-z0-9][a-z0-9._-]*$`). `name` is interpolated into `/etc/apt/keyrings/`
// and `/etc/apt/sources.list.d/`, so a path-traversing value would let a plan
// author write outside those directories.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AptRepoParams {
    /// Filesystem-safe stem reused as the basename of the sources file
    /// (`<name>.sources`) and keyring (`<name>.asc`).
    pub name: String,

    pub uris: Vec<String>,

    pub suites: Vec<String>,

    pub components: Vec<String>,

    pub key_url: String,

    pub types: Option<Vec<String>>,

    pub architectures: Option<Vec<String>>,

    pub enabled: Option<bool>,
}

impl ParseParams for AptRepoParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let name = fields.required_string("name")?;
        let uris = fields.required_string_list("uris")?;
        let suites = fields.required_string_list("suites")?;
        let components = fields.required_string_list("components")?;
        let key_url = fields.required_string("key_url")?;
        let types = fields.optional_string_list("types")?;
        let architectures = fields.optional_string_list("architectures")?;
        let enabled = fields.optional_bool("enabled")?;
        fields.finish()?;
        Ok(AptRepoParams {
            name,
            uris,
            suites,
            components,
            key_url,
            types,
            architectures,
            enabled,
        })
    }
}

impl Display for AptRepoParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "AptRepo(name = {}, uris = [{}], suites = [{}], components = [{}], key_url = {})",
            self.name,
            self.uris.join(", "),
            self.suites.join(", "),
            self.components.join(", "),
            self.key_url
        )
    }
}

impl_display_render!(AptRepoParams);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AptRepoResource {
    pub name: String,
    pub sources_path: FilePath,
    pub sources_content: String,
    pub key_url: String,
    pub key_path: FilePath,
}

impl Display for AptRepoResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "AptRepo(name = {}, sources_path = {}, key_path = {}, key_url = {})",
            self.name, self.sources_path, self.key_path, self.key_url
        )
    }
}

impl_display_render!(AptRepoResource);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AptRepoState {
    Absent,
    Present {
        sources_matches: bool,
        key_present: bool,
    },
}

impl Display for AptRepoState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AptRepoState::Absent => write!(f, "AptRepo::Absent"),
            AptRepoState::Present {
                sources_matches,
                key_present,
            } => write!(
                f,
                "AptRepo::Present(sources_matches = {sources_matches}, key_present = {key_present})"
            ),
        }
    }
}

impl_display_render!(AptRepoState);

#[derive(Error, Debug)]
pub enum AptRepoStateError {
    #[error(transparent)]
    Fs(#[from] FsError),
}

// TODO(cc): add a `Remove` variant mirroring the note on `AptChange`. Today
// removing an apt-repo from the plan leaves both files on the target.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AptRepoChange {
    Install {
        name: String,
        ensure_dir: bool,
        key: Option<(String, FilePath)>,
        sources: Option<(FilePath, String)>,
    },
}

impl Display for AptRepoChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AptRepoChange::Install {
                name,
                ensure_dir,
                key,
                sources,
            } => write!(
                f,
                "AptRepo::Install(name = {name}, ensure_dir = {ensure_dir}, key = {}, sources = {})",
                key.is_some(),
                sources.is_some()
            ),
        }
    }
}

impl_display_render!(AptRepoChange);

#[derive(Debug, Clone)]
pub struct AptRepo;

#[async_trait]
impl ResourceType for AptRepo {
    const ID: &'static str = "apt-repo";

    type Params = AptRepoParams;
    type Resource = AptRepoResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        let AptRepoParams {
            name,
            uris,
            suites,
            components,
            key_url,
            types,
            architectures,
            enabled,
        } = params;

        let sources_path = FilePath::new(format!("{SOURCES_LIST_DIR}/{name}.sources"));
        let key_path = FilePath::new(format!("{KEYRINGS_DIR}/{name}.asc"));

        let empty: Vec<String> = Vec::new();
        let sources_content = render_deb822_content(Deb822Inputs {
            types: types.as_deref().unwrap_or(&empty),
            uris: &uris,
            suites: &suites,
            components: &components,
            signed_by: &key_path,
            architectures: architectures.as_deref().unwrap_or(&empty),
            enabled: enabled.unwrap_or(true),
        });

        vec![CausalityTree::leaf(
            CausalityMeta::default(),
            AptRepoResource {
                name,
                sources_path,
                sources_content,
                key_url,
                key_path,
            },
        )]
    }

    type State = AptRepoState;
    type StateError = AptRepoStateError;

    async fn state(
        _ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        if !fs::path_exists(resource.sources_path.as_path()).await? {
            return Ok(AptRepoState::Absent);
        }

        let on_disk = fs::read_file_to_string(resource.sources_path.as_path()).await?;
        let sources_matches = on_disk == resource.sources_content;

        let key_present = key_file_present(resource.key_path.as_path()).await?;

        Ok(AptRepoState::Present {
            sources_matches,
            key_present,
        })
    }

    type Change = AptRepoChange;

    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        let (sources_matches, key_present) = match state {
            AptRepoState::Absent => (false, false),
            AptRepoState::Present {
                sources_matches,
                key_present,
            } => (*sources_matches, *key_present),
        };

        if sources_matches && key_present {
            return None;
        }

        let key = if key_present {
            None
        } else {
            Some((resource.key_url.clone(), resource.key_path.clone()))
        };
        let sources = if sources_matches {
            None
        } else {
            Some((
                resource.sources_path.clone(),
                resource.sources_content.clone(),
            ))
        };
        let ensure_dir = key.is_some();

        Some(AptRepoChange::Install {
            name: resource.name.clone(),
            ensure_dir,
            key,
            sources,
        })
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        match change {
            AptRepoChange::Install {
                name,
                ensure_dir,
                key,
                sources,
            } => {
                let mut ops: Vec<CausalityTree<Operation>> = Vec::new();

                if ensure_dir {
                    ops.push(CausalityTree::leaf(
                        CausalityMeta::id("keyrings-dir".into()),
                        Operation::AptRepo(AptRepoOperation::EnsureKeyringsDir {
                            path: FilePath::new(KEYRINGS_DIR),
                        }),
                    ));
                }

                let key_emitted = key.is_some();
                if let Some((url, path)) = key {
                    let meta = CausalityMeta {
                        id: Some("key".into()),
                        requires: if ensure_dir {
                            vec!["keyrings-dir".into()]
                        } else {
                            vec![]
                        },
                        required_by: vec![],
                    };
                    ops.push(CausalityTree::leaf(
                        meta,
                        Operation::AptRepo(AptRepoOperation::DownloadKey {
                            name: name.clone(),
                            url,
                            path,
                        }),
                    ));
                }

                if let Some((path, content)) = sources {
                    let meta = if key_emitted {
                        CausalityMeta::requires(vec!["key".into()])
                    } else {
                        CausalityMeta::default()
                    };
                    ops.push(CausalityTree::leaf(
                        meta,
                        Operation::AptRepo(AptRepoOperation::WriteSources {
                            name: name.clone(),
                            path,
                            content,
                        }),
                    ));
                }

                ops
            }
        }
    }
}

struct Deb822Inputs<'a> {
    types: &'a [String],
    uris: &'a [String],
    suites: &'a [String],
    components: &'a [String],
    signed_by: &'a FilePath,
    architectures: &'a [String],
    enabled: bool,
}

/// Render the deb822 sources file body. The output is fed verbatim to
/// `/etc/apt/sources.list.d/<name>.sources`.
///
/// Spec reference: <https://manpages.debian.org/bookworm/apt/sources.list.5.en.html>
fn render_deb822_content(inputs: Deb822Inputs<'_>) -> String {
    if inputs.uris.is_empty() {
        panic!("apt-repo: `uris` must contain at least one entry");
    }
    if inputs.suites.is_empty() {
        panic!("apt-repo: `suites` must contain at least one entry");
    }
    if inputs.components.is_empty() {
        panic!("apt-repo: `components` must contain at least one entry");
    }

    let types: Vec<&str> = if inputs.types.is_empty() {
        vec!["deb"]
    } else {
        inputs.types.iter().map(String::as_str).collect()
    };

    let mut out = String::new();
    out.push_str(&format!("Types: {}\n", types.join(" ")));
    out.push_str(&format!("URIs: {}\n", inputs.uris.join(" ")));
    out.push_str(&format!("Suites: {}\n", inputs.suites.join(" ")));
    out.push_str(&format!("Components: {}\n", inputs.components.join(" ")));
    out.push_str(&format!("Signed-By: {}\n", inputs.signed_by));
    if !inputs.architectures.is_empty() {
        out.push_str(&format!(
            "Architectures: {}\n",
            inputs.architectures.join(" ")
        ));
    }
    if !inputs.enabled {
        out.push_str("Enabled: no\n");
    }
    out
}

async fn key_file_present(path: &std::path::Path) -> Result<bool, FsError> {
    match tokio::fs::metadata(path).await {
        Ok(meta) => Ok(meta.is_file() && meta.len() > 0),
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(false),
        Err(source) => Err(FsError::Metadata {
            path: PathBuf::from(path),
            source,
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn key_path() -> FilePath {
        FilePath::new("/etc/apt/keyrings/docker.asc")
    }

    fn sources_path() -> FilePath {
        FilePath::new("/etc/apt/sources.list.d/docker.sources")
    }

    fn minimal_inputs<'a>(
        signed_by: &'a FilePath,
        suites: &'a [String],
        components: &'a [String],
        uris: &'a [String],
    ) -> Deb822Inputs<'a> {
        Deb822Inputs {
            types: &[],
            uris,
            suites,
            components,
            signed_by,
            architectures: &[],
            enabled: true,
        }
    }

    #[test]
    fn renders_minimal_deb822_with_default_type() {
        let key = key_path();
        let uris = vec!["https://download.docker.com/linux/debian".into()];
        let suites = vec!["bookworm".into()];
        let components = vec!["stable".into()];

        let out = render_deb822_content(minimal_inputs(&key, &suites, &components, &uris));

        let expected = "\
Types: deb
URIs: https://download.docker.com/linux/debian
Suites: bookworm
Components: stable
Signed-By: /etc/apt/keyrings/docker.asc
";
        assert_eq!(out, expected);
    }

    #[test]
    fn renders_with_architectures_overridden_types_and_disabled() {
        let key = key_path();
        let uris = vec!["https://example.com/repo".into()];
        let suites = vec!["bookworm".into()];
        let components = vec!["main".into()];
        let types = vec!["deb".to_string(), "deb-src".to_string()];
        let architectures = vec!["amd64".to_string(), "arm64".to_string()];

        let out = render_deb822_content(Deb822Inputs {
            types: &types,
            uris: &uris,
            suites: &suites,
            components: &components,
            signed_by: &key,
            architectures: &architectures,
            enabled: false,
        });

        let expected = "\
Types: deb deb-src
URIs: https://example.com/repo
Suites: bookworm
Components: main
Signed-By: /etc/apt/keyrings/docker.asc
Architectures: amd64 arm64
Enabled: no
";
        assert_eq!(out, expected);
    }

    /// deb822 multi-value fields are space-separated, not comma-separated. This
    /// is an easy regression to make if someone reads `Vec::join` examples.
    #[test]
    fn multiple_suites_are_space_joined() {
        let key = key_path();
        let uris = vec!["https://example.com/repo".into()];
        let suites = vec!["bookworm".into(), "bookworm-backports".into()];
        let components = vec!["main".into(), "contrib".into()];

        let out = render_deb822_content(minimal_inputs(&key, &suites, &components, &uris));

        assert!(out.contains("Suites: bookworm bookworm-backports\n"));
        assert!(out.contains("Components: main contrib\n"));
        assert!(!out.contains(","));
    }

    #[test]
    #[should_panic(expected = "`components` must contain at least one entry")]
    fn empty_components_panics() {
        let key = key_path();
        let uris = vec!["https://example.com/repo".into()];
        let suites = vec!["bookworm".into()];
        let components: Vec<String> = vec![];
        let _ = render_deb822_content(minimal_inputs(&key, &suites, &components, &uris));
    }

    fn resource_with_content(content: &str) -> AptRepoResource {
        AptRepoResource {
            name: "docker".into(),
            sources_path: sources_path(),
            sources_content: content.to_string(),
            key_url: "https://download.docker.com/linux/debian/gpg".into(),
            key_path: key_path(),
        }
    }

    #[test]
    fn change_returns_none_when_state_matches() {
        let resource = resource_with_content("Types: deb\n");
        let state = AptRepoState::Present {
            sources_matches: true,
            key_present: true,
        };
        assert!(AptRepo::change(&resource, &state).is_none());
    }

    #[test]
    fn change_for_absent_writes_both_and_ensures_dir() {
        let resource = resource_with_content("Types: deb\n");
        let change = AptRepo::change(&resource, &AptRepoState::Absent).expect("change");
        match change {
            AptRepoChange::Install {
                name: _,
                ensure_dir,
                key,
                sources,
            } => {
                assert!(ensure_dir);
                assert!(key.is_some());
                assert!(sources.is_some());
            }
        }
    }

    #[test]
    fn change_for_sources_only_skips_key_and_dir() {
        let resource = resource_with_content("Types: deb\n");
        let state = AptRepoState::Present {
            sources_matches: false,
            key_present: true,
        };
        let change = AptRepo::change(&resource, &state).expect("change");
        match change {
            AptRepoChange::Install {
                name: _,
                ensure_dir,
                key,
                sources,
            } => {
                assert!(!ensure_dir);
                assert!(key.is_none());
                assert!(sources.is_some());
            }
        }
    }

    #[test]
    fn change_for_key_only_skips_sources_and_keeps_dir() {
        let resource = resource_with_content("Types: deb\n");
        let state = AptRepoState::Present {
            sources_matches: true,
            key_present: false,
        };
        let change = AptRepo::change(&resource, &state).expect("change");
        match change {
            AptRepoChange::Install {
                name: _,
                ensure_dir,
                key,
                sources,
            } => {
                assert!(ensure_dir);
                assert!(key.is_some());
                assert!(sources.is_none());
            }
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
    fn params_round_trip() {
        // All-fields-set form exercises the `Some` arms of each `Option`.
        round_trip(&AptRepoParams {
            name: "docker".into(),
            uris: vec!["https://download.docker.com/linux/debian".into()],
            suites: vec!["bookworm".into()],
            components: vec!["stable".into()],
            key_url: "https://download.docker.com/linux/debian/gpg".into(),
            types: Some(vec!["deb".into()]),
            architectures: Some(vec!["amd64".into()]),
            enabled: Some(true),
        });
        // Bare-minimum form exercises the `None` arms.
        round_trip(&AptRepoParams {
            name: "docker".into(),
            uris: vec!["https://download.docker.com/linux/debian".into()],
            suites: vec!["bookworm".into()],
            components: vec!["stable".into()],
            key_url: "https://download.docker.com/linux/debian/gpg".into(),
            types: None,
            architectures: None,
            enabled: None,
        });
    }

    #[test]
    fn resource_round_trip() {
        round_trip(&AptRepoResource {
            name: "docker".into(),
            sources_path: FilePath::new("/etc/apt/sources.list.d/docker.sources"),
            sources_content: "Types: deb\n".into(),
            key_url: "https://download.docker.com/linux/debian/gpg".into(),
            key_path: FilePath::new("/etc/apt/keyrings/docker.asc"),
        });
    }

    #[test]
    fn state_round_trip_covers_every_variant() {
        round_trip(&AptRepoState::Absent);
        round_trip(&AptRepoState::Present {
            sources_matches: true,
            key_present: false,
        });
        round_trip(&AptRepoState::Present {
            sources_matches: false,
            key_present: true,
        });
    }

    #[test]
    fn change_round_trip_covers_every_variant() {
        // Full install: directory, key, and sources all emitted.
        round_trip(&AptRepoChange::Install {
            name: "docker".into(),
            ensure_dir: true,
            key: Some((
                "https://download.docker.com/linux/debian/gpg".into(),
                FilePath::new("/etc/apt/keyrings/docker.asc"),
            )),
            sources: Some((
                FilePath::new("/etc/apt/sources.list.d/docker.sources"),
                "Types: deb\n".into(),
            )),
        });
        // Sources-only install exercises the `None` arms of `key` /
        // `ensure_dir = false`.
        round_trip(&AptRepoChange::Install {
            name: "docker".into(),
            ensure_dir: false,
            key: None,
            sources: Some((
                FilePath::new("/etc/apt/sources.list.d/docker.sources"),
                "Types: deb\n".into(),
            )),
        });
        // Key-only install exercises the `None` arm of `sources`.
        round_trip(&AptRepoChange::Install {
            name: "docker".into(),
            ensure_dir: true,
            key: Some((
                "https://download.docker.com/linux/debian/gpg".into(),
                FilePath::new("/etc/apt/keyrings/docker.asc"),
            )),
            sources: None,
        });
    }
}
