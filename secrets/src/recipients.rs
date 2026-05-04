//! `<secrets_dir>/lusid-secrets.toml` — the project-level table mapping each
//! `*.age` file to the recipients that can decrypt it.
//!
//! Shape:
//!
//! ```toml
//! [operators]
//! mikey = "age1..."
//!
//! [machines]
//! rpi4b-1 = "ssh-ed25519 AAAA..."
//!
//! [groups]
//! operators = ["mikey"]
//!
//! [files]
//! "api_token" = { recipients = ["@operators", "rpi4b-1"] }
//! ```
//!
//! `@name` references in a file's `recipients` list expand via `[groups]`;
//! bare names look up in `[operators]` then `[machines]`. Expansion is shallow
//! (groups cannot reference groups) — keeps the model predictable without
//! meaningfully limiting usage.
//!
//! The operator / machine split is load-bearing for per-target re-encryption
//! done by `lusid remote apply`: the target machine's SSH host key (looked up
//! in `[machines]` by `machine_id`) is the sole recipient before ciphertext
//! is shipped to the guest. See [`Recipients::get_machine`].

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use displaydoc::Display;
use indexmap::IndexMap;
use serde::Deserialize;
use thiserror::Error;
use tokio::fs;

use crate::key::Key;

pub(crate) const SECRETS_FILE: &str = "lusid-secrets.toml";

/// Parsed `lusid-secrets.toml`. Order preserved so listing commands match
/// on-disk order. Operator and machine aliases share a single namespace at
/// resolve time; load-time validation rejects duplicates across the two.
///
/// Every reference in `[files]` and `[groups]` is validated at load time,
/// so `resolve` / `files_for_alias` never fail on unknown refs; only
/// `resolve` on a stem absent from `[files]` is a lookup-time error.
#[derive(Debug, Clone)]
pub struct Recipients {
    pub operators: IndexMap<String, Key>,
    pub machines: IndexMap<String, Key>,
    pub groups: IndexMap<String, Vec<String>>,
    pub files: IndexMap<String, FileEntry>,
}

#[derive(Debug, Clone, Default, Deserialize)]
struct RecipientsToml {
    #[serde(default)]
    operators: IndexMap<String, Key>,

    #[serde(default)]
    machines: IndexMap<String, Key>,

    #[serde(default)]
    groups: IndexMap<String, Vec<String>>,

    #[serde(default)]
    files: IndexMap<String, FileEntry>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct FileEntry {
    pub recipients: Vec<String>,
}

impl Recipients {
    /// Load `lusid-secrets.toml` from `<secrets_dir>/lusid-secrets.toml`.
    ///
    /// Performs all structural validation at load time:
    ///
    /// - alias collision between `[operators]` and `[machines]`;
    /// - empty recipients list for any `[files]` entry;
    /// - group members that reference unknown aliases;
    /// - group members that reference other groups (shallow expansion only);
    /// - `[files]` recipients that reference unknown aliases or unknown groups.
    ///
    /// A missing config file returns [`RecipientsError::Missing`] separately
    /// so callers (e.g. `lusid-apply`) can distinguish "no secrets set up"
    /// from "config present but broken".
    pub async fn load(secrets_dir: &Path) -> Result<Self, RecipientsError> {
        let path = secrets_dir.join(SECRETS_FILE);
        let text = match fs::read_to_string(&path).await {
            Ok(t) => t,
            Err(source) if source.kind() == std::io::ErrorKind::NotFound => {
                return Err(RecipientsError::Missing { path });
            }
            Err(source) => return Err(RecipientsError::Read { path, source }),
        };
        let raw: RecipientsToml =
            toml::from_str(&text).map_err(|source| RecipientsError::Parse { path, source })?;
        Self::from_toml(raw)
    }

    fn from_toml(raw: RecipientsToml) -> Result<Self, RecipientsError> {
        let RecipientsToml {
            operators,
            machines,
            groups,
            files,
        } = raw;

        for alias in operators.keys() {
            if machines.contains_key(alias) {
                return Err(RecipientsError::AliasCollision {
                    alias: alias.clone(),
                });
            }
        }

        let alias_known = |name: &str| operators.contains_key(name) || machines.contains_key(name);

        // Groups: members must be bare aliases (no nested `@group` references).
        for (group, members) in &groups {
            for member in members {
                if let Some(nested) = member.strip_prefix('@') {
                    return Err(RecipientsError::NestedGroup {
                        group: group.clone(),
                        nested: nested.to_owned(),
                    });
                }
                if !alias_known(member) {
                    return Err(RecipientsError::UnknownAliasInGroup {
                        group: group.clone(),
                        alias: member.clone(),
                    });
                }
            }
        }

        // Files: non-empty recipients; every ref resolves to a known alias or group.
        for (stem, entry) in &files {
            if entry.recipients.is_empty() {
                return Err(RecipientsError::EmptyRecipients { file: stem.clone() });
            }
            for name in &entry.recipients {
                if let Some(group) = name.strip_prefix('@') {
                    if !groups.contains_key(group) {
                        return Err(RecipientsError::UnknownGroup {
                            file: stem.clone(),
                            group: group.to_owned(),
                        });
                    }
                } else if !alias_known(name) {
                    return Err(RecipientsError::UnknownAlias {
                        file: stem.clone(),
                        alias: name.clone(),
                    });
                }
            }
        }

        Ok(Recipients {
            operators,
            machines,
            groups,
            files,
        })
    }

    /// Resolve a file stem's recipient list into concrete age recipients.
    ///
    /// Group references (`@name`) are expanded; duplicate aliases are
    /// deduplicated. Returns an error only when `stem` is not in `[files]` —
    /// all other references are validated at load time.
    pub(crate) fn resolve(&self, stem: &str) -> Result<Vec<ResolvedRecipient>, ResolveError> {
        let entry = self
            .files
            .get(stem)
            .ok_or_else(|| ResolveError::UnknownFile {
                stem: stem.to_owned(),
            })?;

        let mut seen: BTreeSet<String> = BTreeSet::new();
        let mut out = Vec::new();
        for name in &entry.recipients {
            if let Some(group) = name.strip_prefix('@') {
                let members = self.groups.get(group).expect("validated at load time");
                for member in members {
                    if seen.insert(member.clone()) {
                        out.push(self.lookup(member));
                    }
                }
            } else if seen.insert(name.clone()) {
                out.push(self.lookup(name));
            }
        }
        Ok(out)
    }

    fn lookup(&self, alias: &str) -> ResolvedRecipient {
        let key = self
            .operators
            .get(alias)
            .or_else(|| self.machines.get(alias))
            .expect("validated at load time");
        ResolvedRecipient {
            alias: alias.to_owned(),
            key: key.clone(),
        }
    }

    /// File stems this alias can decrypt, in declaration order.
    ///
    /// Includes every file whose `[files].recipients` list mentions `alias`
    /// directly, or names a group that contains `alias`. Used at apply time
    /// to pick the subset of `*.age` files the machine's identity should
    /// attempt to decrypt.
    pub fn files_for_alias(&self, alias: &str) -> Vec<&str> {
        let containing_groups: BTreeSet<&str> = self
            .groups
            .iter()
            .filter(|(_, members)| members.iter().any(|m| m == alias))
            .map(|(g, _)| g.as_str())
            .collect();

        self.files
            .iter()
            .filter(|(_, entry)| {
                entry.recipients.iter().any(|name| {
                    if let Some(group) = name.strip_prefix('@') {
                        containing_groups.contains(group)
                    } else {
                        name == alias
                    }
                })
            })
            .map(|(stem, _)| stem.as_str())
            .collect()
    }

    /// Every file stem listed in `[files]`, in declaration order.
    pub(crate) fn file_stems(&self) -> impl Iterator<Item = &str> {
        self.files.keys().map(String::as_str)
    }
}

/// Public-key string for a machine alias, suitable as the `machine_pubkey`
/// argument to [`reencrypt_for_machine`](crate::reencrypt_for_machine).
///
/// Loads `<secrets_dir>/lusid-secrets.toml`, looks up `machine_id` in
/// `[machines]`, and serializes the parsed key back to its canonical
/// `age1...` / `ssh-ed25519 ...` form. Used by `lusid remote apply` to
/// resolve the target machine's recipient before per-target re-encryption.
#[tracing::instrument(skip(secrets_dir))]
pub async fn machine_pubkey(
    secrets_dir: &Path,
    machine_id: &str,
) -> Result<String, MachinePubkeyError> {
    let recipients = Recipients::load(secrets_dir).await?;
    recipients
        .machines
        .get(machine_id)
        .map(|key| key.to_string())
        .ok_or_else(|| MachinePubkeyError::UnknownMachine {
            machine_id: machine_id.to_owned(),
        })
}

/// One recipient for a specific file, carrying its alias for display.
#[derive(Debug, Clone)]
pub(crate) struct ResolvedRecipient {
    pub alias: String,
    pub key: Key,
}

/// Convert a resolved recipient list into the boxed form `age` expects for
/// encryption. Cheap clones — both [`Key`] variants wrap small recipient
/// types (a public point or an SSH pubkey).
pub(crate) fn to_boxed_recipients(
    resolved: &[ResolvedRecipient],
) -> Vec<Box<dyn age::Recipient + Send>> {
    resolved
        .iter()
        .map(|r| -> Box<dyn age::Recipient + Send> {
            match &r.key {
                Key::X25519(k) => Box::new(k.clone()),
                Key::Ssh(k) => Box::new(k.clone()),
            }
        })
        .collect()
}

impl<'de> Deserialize<'de> for Key {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        let raw = String::deserialize(deserializer)?;
        Key::from_str(&raw).map_err(D::Error::custom)
    }
}

#[derive(Debug, Error, Display)]
pub enum RecipientsError {
    /// Missing {path}
    Missing { path: PathBuf },

    /// Failed to read {path}: {source}
    Read {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// Failed to parse {path}: {source}
    Parse {
        path: PathBuf,
        #[source]
        source: toml::de::Error,
    },

    /// Alias {alias:?} declared in both [operators] and [machines]
    AliasCollision { alias: String },

    /// Group {group:?} references unknown alias {alias:?}
    UnknownAliasInGroup { group: String, alias: String },

    /// Group {group:?} references nested group @{nested}; groups cannot reference other groups
    NestedGroup { group: String, nested: String },

    /// File {file:?} has an empty recipients list
    EmptyRecipients { file: String },

    /// File {file:?} references unknown alias {alias:?}
    UnknownAlias { file: String, alias: String },

    /// File {file:?} references unknown group @{group}
    UnknownGroup { file: String, group: String },
}

#[derive(Debug, Error, Display)]
pub enum ResolveError {
    /// No [files] entry for {stem:?}
    UnknownFile { stem: String },
}

#[derive(Debug, Error, Display)]
pub enum MachinePubkeyError {
    /// {0}
    Recipients(#[from] RecipientsError),

    /// No [machines] entry for {machine_id:?} in lusid-secrets.toml
    UnknownMachine { machine_id: String },
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r#"
[operators]
mikey = "age1t7rxyev2z3rw82stdlrrepyc39nvn86l5078zqkf5uasdy86jp6svpy7pa"

[machines]
rpi = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHsKLqeplhpW+uObz5dvMgjz1OxfM/XXUB+VHtZ6isGN alice@rust"

[groups]
operators = ["mikey"]

[files]
"api_token" = { recipients = ["@operators", "rpi"] }
"db_pw" = { recipients = ["@operators"] }
"#;

    fn parse_toml(s: &str) -> Result<Recipients, RecipientsError> {
        let raw: RecipientsToml = toml::from_str(s).unwrap();
        Recipients::from_toml(raw)
    }

    fn parse() -> Recipients {
        parse_toml(SAMPLE).unwrap()
    }

    #[test]
    fn parses_operators_machines_groups_files() {
        let r = parse();
        assert_eq!(r.operators.len(), 1);
        assert_eq!(r.machines.len(), 1);
        assert!(matches!(r.operators["mikey"], Key::X25519(_)));
        assert!(matches!(r.machines["rpi"], Key::Ssh(_)));
        assert_eq!(r.groups["operators"], vec!["mikey"]);
        assert_eq!(r.files.len(), 2);
    }

    #[test]
    fn resolves_file_with_group_and_alias() {
        let r = parse();
        let recipients = r.resolve("api_token").unwrap();
        let aliases: Vec<&str> = recipients.iter().map(|x| x.alias.as_str()).collect();
        assert_eq!(aliases, vec!["mikey", "rpi"]);
    }

    #[test]
    fn deduplicates_across_expansion() {
        let r = parse_toml(
            r#"
[operators]
a = "age1t7rxyev2z3rw82stdlrrepyc39nvn86l5078zqkf5uasdy86jp6svpy7pa"

[groups]
g = ["a"]

[files]
"f" = { recipients = ["a", "@g", "a"] }
"#,
        )
        .unwrap();
        let recipients = r.resolve("f").unwrap();
        assert_eq!(recipients.len(), 1);
    }

    #[test]
    fn unknown_file_at_resolve() {
        let r = parse();
        assert!(matches!(
            r.resolve("nope").unwrap_err(),
            ResolveError::UnknownFile { .. }
        ));
    }

    #[test]
    fn unknown_alias_is_load_error() {
        let err = parse_toml(
            r#"
[operators]
a = "age1t7rxyev2z3rw82stdlrrepyc39nvn86l5078zqkf5uasdy86jp6svpy7pa"
[files]
"f" = { recipients = ["b"] }
"#,
        )
        .unwrap_err();
        assert!(matches!(err, RecipientsError::UnknownAlias { .. }));
    }

    #[test]
    fn unknown_group_is_load_error() {
        let err = parse_toml(
            r#"
[operators]
a = "age1t7rxyev2z3rw82stdlrrepyc39nvn86l5078zqkf5uasdy86jp6svpy7pa"
[files]
"f" = { recipients = ["@bogus"] }
"#,
        )
        .unwrap_err();
        assert!(matches!(err, RecipientsError::UnknownGroup { .. }));
    }

    #[test]
    fn alias_collision_errors() {
        let err = parse_toml(
            r#"
[operators]
dup = "age1t7rxyev2z3rw82stdlrrepyc39nvn86l5078zqkf5uasdy86jp6svpy7pa"

[machines]
dup = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHsKLqeplhpW+uObz5dvMgjz1OxfM/XXUB+VHtZ6isGN alice@rust"
"#,
        )
        .unwrap_err();
        assert!(matches!(err, RecipientsError::AliasCollision { .. }));
    }

    #[test]
    fn nested_group_is_load_error() {
        let err = parse_toml(
            r#"
[operators]
a = "age1t7rxyev2z3rw82stdlrrepyc39nvn86l5078zqkf5uasdy86jp6svpy7pa"

[groups]
g1 = ["a"]
g2 = ["@g1"]
"#,
        )
        .unwrap_err();
        assert!(matches!(err, RecipientsError::NestedGroup { .. }));
    }

    #[test]
    fn empty_recipients_is_load_error() {
        let err = parse_toml(
            r#"
[operators]
a = "age1t7rxyev2z3rw82stdlrrepyc39nvn86l5078zqkf5uasdy86jp6svpy7pa"
[files]
"f" = { recipients = [] }
"#,
        )
        .unwrap_err();
        assert!(matches!(err, RecipientsError::EmptyRecipients { .. }));
    }

    /// Two real x25519 public keys derived at runtime — some of the
    /// hand-rolled `age1...` strings in early drafts failed bech32 checksum.
    fn two_pubkeys() -> (String, String) {
        let a = age::x25519::Identity::generate().to_public().to_string();
        let b = age::x25519::Identity::generate().to_public().to_string();
        (a, b)
    }

    #[test]
    fn files_for_alias_direct() {
        let (a, b) = two_pubkeys();
        let toml = format!(
            r#"
[operators]
a = "{a}"
b = "{b}"
[files]
"a_only" = {{ recipients = ["a"] }}
"b_only" = {{ recipients = ["b"] }}
"#
        );
        let r = parse_toml(&toml).unwrap();
        assert_eq!(r.files_for_alias("a"), vec!["a_only"]);
        assert_eq!(r.files_for_alias("b"), vec!["b_only"]);
    }

    #[test]
    fn files_for_alias_via_group() {
        let r = parse();
        assert_eq!(r.files_for_alias("mikey"), vec!["api_token", "db_pw"]);
    }

    #[test]
    fn files_for_alias_multiple_groups() {
        let (a, b) = two_pubkeys();
        let toml = format!(
            r#"
[operators]
a = "{a}"
b = "{b}"

[groups]
g1 = ["a"]
g2 = ["a", "b"]

[files]
"only_g1" = {{ recipients = ["@g1"] }}
"only_g2" = {{ recipients = ["@g2"] }}
"both" = {{ recipients = ["@g1", "@g2"] }}
"#
        );
        let r = parse_toml(&toml).unwrap();
        assert_eq!(r.files_for_alias("a"), vec!["only_g1", "only_g2", "both"]);
        assert_eq!(r.files_for_alias("b"), vec!["only_g2", "both"]);
    }

    #[test]
    fn files_for_alias_excluded() {
        let (a, b) = two_pubkeys();
        let toml = format!(
            r#"
[operators]
a = "{a}"
b = "{b}"
[files]
"only_a" = {{ recipients = ["a"] }}
"#
        );
        let r = parse_toml(&toml).unwrap();
        assert!(r.files_for_alias("b").is_empty());
    }

    /// Two SSH public keys, one with a trailing comment. Used to confirm the
    /// `Display`/`from_str` round-trip is canonical (comment dropped).
    const SSH_PUBKEY_NO_COMMENT: &str =
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHsKLqeplhpW+uObz5dvMgjz1OxfM/XXUB+VHtZ6isGN";
    const SSH_PUBKEY_WITH_COMMENT: &str =
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHsKLqeplhpW+uObz5dvMgjz1OxfM/XXUB+VHtZ6isGN alice@rust";

    #[tokio::test]
    async fn machine_pubkey_returns_ssh_key() {
        let dir = tempfile::TempDir::new().unwrap();
        std::fs::write(
            dir.path().join("lusid-secrets.toml"),
            format!(r#"[machines]
rpi = "{SSH_PUBKEY_WITH_COMMENT}"
"#),
        )
        .unwrap();
        let key = machine_pubkey(dir.path(), "rpi").await.unwrap();
        // SSH key Display drops the comment but preserves type+body.
        assert!(key.starts_with("ssh-ed25519 "));
        // Round-trip: result must parse back into the same Key variant.
        let _: Key = key.parse().unwrap();
    }

    #[tokio::test]
    async fn machine_pubkey_returns_age_key() {
        let id = age::x25519::Identity::generate();
        let pubkey = id.to_public().to_string();
        let dir = tempfile::TempDir::new().unwrap();
        std::fs::write(
            dir.path().join("lusid-secrets.toml"),
            format!(r#"[machines]
host-a = "{pubkey}"
"#),
        )
        .unwrap();
        let key = machine_pubkey(dir.path(), "host-a").await.unwrap();
        assert_eq!(key, pubkey);
    }

    #[tokio::test]
    async fn machine_pubkey_unknown_machine_errors() {
        let dir = tempfile::TempDir::new().unwrap();
        std::fs::write(
            dir.path().join("lusid-secrets.toml"),
            format!(r#"[machines]
rpi = "{SSH_PUBKEY_NO_COMMENT}"
"#),
        )
        .unwrap();
        let err = machine_pubkey(dir.path(), "missing").await.unwrap_err();
        assert!(matches!(
            err,
            MachinePubkeyError::UnknownMachine { ref machine_id } if machine_id == "missing"
        ));
    }

    #[tokio::test]
    async fn machine_pubkey_missing_toml_errors() {
        let dir = tempfile::TempDir::new().unwrap();
        let err = machine_pubkey(dir.path(), "anything").await.unwrap_err();
        assert!(matches!(
            err,
            MachinePubkeyError::Recipients(RecipientsError::Missing { .. })
        ));
    }
}
