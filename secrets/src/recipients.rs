//! `<secrets_dir>/lusid-secrets.toml` — the project-level table mapping each
//! `*.age` file to the recipients that can decrypt it.
//!
//! Shape:
//!
//! ```toml
//! [operators]
//! mikey = "age1..."          # implicit recipient on every [files] entry
//!
//! [machines]
//! rpi4b-1 = "ssh-ed25519 AAAA..."
//!
//! [groups]
//! prod = ["rpi4b-1"]         # machine groups only
//!
//! [files]
//! "api_token"  = { recipients = ["@prod"] }     # ops + rpi4b-1
//! "local_only" = { recipients = [] }            # ops only
//! ```
//!
//! Operators are implicit recipients on every `[files]` entry. `@name`
//! references expand via `[groups]`; bare names look up in `[machines]`.
//! `[groups]` and `[files].recipients` may NOT reference operator aliases —
//! that's the whole point of the implicit rule. Expansion is shallow
//! (groups cannot reference groups).
//!
//! The operator / machine split is load-bearing for per-target re-encryption
//! done by `lusid remote apply`: the target machine's SSH host key (looked up
//! in `[machines]` by `machine_id`) is the sole recipient before ciphertext
//! is shipped to the guest.

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
    #[serde(default)]
    pub recipients: Vec<String>,
}

/// Which table an alias was declared in. Used in
/// [`RecipientsError::DuplicateKey`] to disambiguate the diagnostic
/// (op-vs-op vs op-vs-machine vs machine-vs-machine).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AliasKind {
    Operator,
    Machine,
}

impl std::fmt::Display for AliasKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AliasKind::Operator => f.write_str("operator"),
            AliasKind::Machine => f.write_str("machine"),
        }
    }
}

impl Recipients {
    /// Load `lusid-secrets.toml` from `<secrets_dir>/lusid-secrets.toml`.
    ///
    /// Performs all structural validation at load time:
    ///
    /// - alias collision between `[operators]` and `[machines]`;
    /// - duplicate `Key` values across `[operators]` ∪ `[machines]`;
    /// - operator alias appearing in `[groups]`;
    /// - operator alias appearing in `[files].recipients`;
    /// - empty `[groups]` member list;
    /// - file whose effective recipient set (operators + listed) is empty;
    /// - group members that reference unknown machine aliases;
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

        // Alias-name collision across operators/machines.
        for alias in operators.keys() {
            if machines.contains_key(alias) {
                return Err(RecipientsError::AliasCollision {
                    alias: alias.clone(),
                });
            }
        }

        // Duplicate key *value* across operators + machines. Compare by the
        // canonical Display form: a stable string identity that's uniform
        // across X25519 and SSH variants without exposing inner pubkey
        // bytes. O(N²) — fine for the expected handful of recipients.
        let labelled: Vec<(&String, AliasKind, String)> = operators
            .iter()
            .map(|(a, k)| (a, AliasKind::Operator, k.to_string()))
            .chain(
                machines
                    .iter()
                    .map(|(a, k)| (a, AliasKind::Machine, k.to_string())),
            )
            .collect();
        for i in 0..labelled.len() {
            for j in (i + 1)..labelled.len() {
                if labelled[i].2 == labelled[j].2 {
                    return Err(RecipientsError::DuplicateKey {
                        first: labelled[i].0.clone(),
                        first_kind: labelled[i].1,
                        second: labelled[j].0.clone(),
                        second_kind: labelled[j].1,
                    });
                }
            }
        }

        // Groups: non-empty; members must be machine aliases (no operators,
        // no nested @groups).
        for (group, members) in &groups {
            if members.is_empty() {
                return Err(RecipientsError::EmptyGroup {
                    group: group.clone(),
                });
            }
            for member in members {
                if let Some(nested) = member.strip_prefix('@') {
                    return Err(RecipientsError::NestedGroup {
                        group: group.clone(),
                        nested: nested.to_owned(),
                    });
                }
                if operators.contains_key(member) {
                    return Err(RecipientsError::OperatorInGroup {
                        group: group.clone(),
                        operator: member.clone(),
                    });
                }
                if !machines.contains_key(member) {
                    return Err(RecipientsError::UnknownAliasInGroup {
                        group: group.clone(),
                        alias: member.clone(),
                    });
                }
            }
        }

        // Files: every ref resolves to a known machine or group; operators
        // are implicit and forbidden in the explicit list. Empty recipients
        // is allowed iff there are operators to fill the slot.
        for (stem, entry) in &files {
            if entry.recipients.is_empty() && operators.is_empty() {
                return Err(RecipientsError::EmptyEffectiveRecipients { file: stem.clone() });
            }
            for name in &entry.recipients {
                if let Some(group) = name.strip_prefix('@') {
                    if !groups.contains_key(group) {
                        return Err(RecipientsError::UnknownGroup {
                            file: stem.clone(),
                            group: group.to_owned(),
                        });
                    }
                } else if operators.contains_key(name) {
                    return Err(RecipientsError::OperatorInFileRecipients {
                        file: stem.clone(),
                        operator: name.clone(),
                    });
                } else if !machines.contains_key(name) {
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
    /// Order: operators in `[operators]` declaration order, then unique
    /// machines in first-mention order through the file's recipients
    /// (with `@group` refs expanded). Stable order keeps ciphertext
    /// header layout deterministic for `compare_stanzas` drift detection.
    ///
    /// Returns an error only when `stem` is not in `[files]`.
    pub(crate) fn resolve(&self, stem: &str) -> Result<Vec<ResolvedRecipient>, ResolveError> {
        let entry = self
            .files
            .get(stem)
            .ok_or_else(|| ResolveError::UnknownFile {
                stem: stem.to_owned(),
            })?;

        let mut seen: BTreeSet<String> = BTreeSet::new();
        let mut out = Vec::new();

        // Operators first, always.
        for alias in self.operators.keys() {
            if seen.insert(alias.clone()) {
                out.push(self.lookup(alias));
            }
        }

        // Then explicitly-listed machines (via direct ref or @group).
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

    /// File stems this alias can decrypt, in `[files]` declaration order.
    ///
    /// - For an operator alias: every file in `[files]`, since operators are
    ///   implicit recipients on every entry (including `recipients = []`).
    /// - For a machine alias: files whose `[files].recipients` mentions the
    ///   machine directly, or names a group containing it.
    /// - For an unknown alias: empty.
    pub fn files_for_alias(&self, alias: &str) -> Vec<&str> {
        if self.operators.contains_key(alias) {
            return self.files.keys().map(String::as_str).collect();
        }

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

    /// Same pubkey declared as {first_kind} {first:?} and {second_kind} {second:?}; pubkeys must be unique across [operators] and [machines]
    DuplicateKey {
        first: String,
        first_kind: AliasKind,
        second: String,
        second_kind: AliasKind,
    },

    /// Group {group:?} references unknown machine alias {alias:?}
    UnknownAliasInGroup { group: String, alias: String },

    /// Group {group:?} references operator {operator:?}; operators are implicit recipients and cannot appear in [groups]
    OperatorInGroup { group: String, operator: String },

    /// Group {group:?} references nested group @{nested}; groups cannot reference other groups
    NestedGroup { group: String, nested: String },

    /// Group {group:?} has an empty member list
    EmptyGroup { group: String },

    /// File {file:?} resolves to an empty recipient set: empty [files].recipients and no operators declared
    EmptyEffectiveRecipients { file: String },

    /// File {file:?} references unknown alias {alias:?}
    UnknownAlias { file: String, alias: String },

    /// File {file:?} references operator {operator:?}; operators are implicit recipients and cannot appear in [files] recipients
    OperatorInFileRecipients { file: String, operator: String },

    /// File {file:?} references unknown group @{group}
    UnknownGroup { file: String, group: String },
}

#[derive(Debug, Error, Display)]
pub enum ResolveError {
    /// No [files] entry for {stem:?}
    UnknownFile { stem: String },
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Stable x25519 + SSH pubkey strings for fixture toml. Real keys
    /// (bech32 / base64 valid) so `Key::from_str` accepts them.
    const X25519_MIKEY: &str = "age1t7rxyev2z3rw82stdlrrepyc39nvn86l5078zqkf5uasdy86jp6svpy7pa";
    const SSH_RPI: &str = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHsKLqeplhpW+uObz5dvMgjz1OxfM/XXUB+VHtZ6isGN alice@rust";

    const SAMPLE: &str = r#"
[operators]
mikey = "age1t7rxyev2z3rw82stdlrrepyc39nvn86l5078zqkf5uasdy86jp6svpy7pa"

[machines]
rpi = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHsKLqeplhpW+uObz5dvMgjz1OxfM/XXUB+VHtZ6isGN alice@rust"

[groups]
prod = ["rpi"]

[files]
"api_token" = { recipients = ["@prod"] }
"db_pw" = { recipients = ["rpi"] }
"local_only" = { recipients = [] }
"#;

    fn parse_toml(s: &str) -> Result<Recipients, RecipientsError> {
        let raw: RecipientsToml = toml::from_str(s).unwrap();
        Recipients::from_toml(raw)
    }

    fn parse() -> Recipients {
        parse_toml(SAMPLE).unwrap()
    }

    /// Two real, distinct x25519 public keys. Generated per-call so they
    /// vary between tests; keys equal across two calls would defeat the
    /// duplicate-key tests.
    fn two_pubkeys() -> (String, String) {
        let a = age::x25519::Identity::generate().to_public().to_string();
        let b = age::x25519::Identity::generate().to_public().to_string();
        (a, b)
    }

    // -- shape & happy-path resolve ------------------------------------

    #[test]
    fn parses_sample() {
        let r = parse();
        assert_eq!(r.operators.len(), 1);
        assert_eq!(r.machines.len(), 1);
        assert_eq!(r.groups["prod"], vec!["rpi"]);
        assert_eq!(r.files.len(), 3);
    }

    fn resolved_aliases(r: &Recipients, stem: &str) -> Vec<String> {
        r.resolve(stem)
            .unwrap()
            .into_iter()
            .map(|x| x.alias)
            .collect()
    }

    #[test]
    fn resolve_prepends_operators() {
        let r = parse();
        assert_eq!(resolved_aliases(&r, "api_token"), vec!["mikey", "rpi"]);
    }

    #[test]
    fn resolve_operator_only_file() {
        // `local_only` has empty recipients — only operators should remain.
        let r = parse();
        assert_eq!(resolved_aliases(&r, "local_only"), vec!["mikey"]);
    }

    #[test]
    fn resolve_dedups_machine_via_direct_and_group() {
        // `rpi` is mentioned both directly and via `@prod` — should appear
        // once, in first-mention position (direct).
        let r = parse_toml(&format!(
            r#"
[operators]
mikey = "{X25519_MIKEY}"

[machines]
rpi = "{SSH_RPI}"

[groups]
prod = ["rpi"]

[files]
"f" = {{ recipients = ["rpi", "@prod", "rpi"] }}
"#,
        ))
        .unwrap();
        assert_eq!(resolved_aliases(&r, "f"), vec!["mikey", "rpi"]);
    }

    #[test]
    fn resolve_unknown_file_errors() {
        assert!(matches!(
            parse().resolve("nope").unwrap_err(),
            ResolveError::UnknownFile { .. }
        ));
    }

    // -- files_for_alias -----------------------------------------------

    #[test]
    fn files_for_alias_operator_returns_every_file() {
        // Operators are implicit on every file, including ones with empty
        // recipients lists.
        let r = parse();
        assert_eq!(
            r.files_for_alias("mikey"),
            vec!["api_token", "db_pw", "local_only"]
        );
    }

    #[test]
    fn files_for_alias_machine_filters() {
        // Both machines parameterised as x25519 — `Key` allows either
        // variant in `[machines]`, and using two fresh x25519 keys
        // sidesteps needing a second SSH keypair just for this fixture.
        let (op, m1) = two_pubkeys();
        let m2 = age::x25519::Identity::generate().to_public().to_string();
        let r = parse_toml(&format!(
            r#"
[operators]
op = "{op}"

[machines]
m1 = "{m1}"
m2 = "{m2}"

[files]
"only_m1" = {{ recipients = ["m1"] }}
"only_m2" = {{ recipients = ["m2"] }}
"#,
        ))
        .unwrap();
        assert_eq!(r.files_for_alias("m1"), vec!["only_m1"]);
        assert_eq!(r.files_for_alias("m2"), vec!["only_m2"]);
    }

    #[test]
    fn files_for_alias_unknown_returns_empty() {
        let r = parse();
        assert!(r.files_for_alias("nobody").is_empty());
    }

    /// Regression: `files_for_alias` for a machine alias must follow
    /// `@group` references in `[files].recipients`. With the implicit-
    /// operators schema there's no longer an "operator-group" path, but
    /// machine groups still need to expand.
    #[test]
    fn files_for_alias_machine_via_group() {
        let (op, m1) = two_pubkeys();
        let m2 = age::x25519::Identity::generate().to_public().to_string();
        let r = parse_toml(&format!(
            r#"
[operators]
op = "{op}"

[machines]
m1 = "{m1}"
m2 = "{m2}"

[groups]
prod = ["m1", "m2"]

[files]
"via_group"  = {{ recipients = ["@prod"] }}
"via_direct" = {{ recipients = ["m1"] }}
"#,
        ))
        .unwrap();
        // m1 is a recipient on both files; m2 only on `via_group`.
        assert_eq!(r.files_for_alias("m1"), vec!["via_group", "via_direct"]);
        assert_eq!(r.files_for_alias("m2"), vec!["via_group"]);
    }

    // -- validation: structural ----------------------------------------

    #[test]
    fn alias_collision_errors() {
        let err = parse_toml(&format!(
            r#"
[operators]
dup = "{X25519_MIKEY}"

[machines]
dup = "{SSH_RPI}"
"#,
        ))
        .unwrap_err();
        assert!(matches!(err, RecipientsError::AliasCollision { .. }));
    }

    #[test]
    fn duplicate_key_op_op_errors() {
        let (a, _) = two_pubkeys();
        let err = parse_toml(&format!(
            r#"
[operators]
alpha = "{a}"
beta  = "{a}"
"#,
        ))
        .unwrap_err();
        match err {
            RecipientsError::DuplicateKey {
                first_kind,
                second_kind,
                ..
            } => {
                assert_eq!(first_kind, AliasKind::Operator);
                assert_eq!(second_kind, AliasKind::Operator);
            }
            other => panic!("wrong variant: {other:?}"),
        }
    }

    #[test]
    fn duplicate_key_op_machine_errors() {
        // Same SSH pubkey under both an operator and a machine alias.
        let err = parse_toml(&format!(
            r#"
[operators]
op = "{SSH_RPI}"

[machines]
m  = "{SSH_RPI}"
"#,
        ))
        .unwrap_err();
        match err {
            RecipientsError::DuplicateKey {
                first_kind,
                second_kind,
                ..
            } => {
                assert_eq!(first_kind, AliasKind::Operator);
                assert_eq!(second_kind, AliasKind::Machine);
            }
            other => panic!("wrong variant: {other:?}"),
        }
    }

    // -- validation: groups --------------------------------------------

    #[test]
    fn nested_group_errors() {
        let err = parse_toml(&format!(
            r#"
[operators]
op = "{X25519_MIKEY}"

[machines]
m = "{SSH_RPI}"

[groups]
g1 = ["m"]
g2 = ["@g1"]
"#,
        ))
        .unwrap_err();
        assert!(matches!(err, RecipientsError::NestedGroup { .. }));
    }

    #[test]
    fn empty_group_errors() {
        let err = parse_toml(&format!(
            r#"
[operators]
op = "{X25519_MIKEY}"

[groups]
g = []
"#,
        ))
        .unwrap_err();
        assert!(matches!(err, RecipientsError::EmptyGroup { .. }));
    }

    #[test]
    fn operator_in_group_errors() {
        let err = parse_toml(&format!(
            r#"
[operators]
op = "{X25519_MIKEY}"

[machines]
m = "{SSH_RPI}"

[groups]
g = ["op", "m"]
"#,
        ))
        .unwrap_err();
        match err {
            RecipientsError::OperatorInGroup { group, operator } => {
                assert_eq!(group, "g");
                assert_eq!(operator, "op");
            }
            other => panic!("wrong variant: {other:?}"),
        }
    }

    #[test]
    fn unknown_alias_in_group_errors() {
        let err = parse_toml(&format!(
            r#"
[operators]
op = "{X25519_MIKEY}"

[groups]
g = ["mystery"]
"#,
        ))
        .unwrap_err();
        assert!(matches!(err, RecipientsError::UnknownAliasInGroup { .. }));
    }

    // -- validation: files ---------------------------------------------

    #[test]
    fn unknown_alias_in_files_errors() {
        let err = parse_toml(&format!(
            r#"
[operators]
op = "{X25519_MIKEY}"

[files]
"f" = {{ recipients = ["bogus"] }}
"#,
        ))
        .unwrap_err();
        assert!(matches!(err, RecipientsError::UnknownAlias { .. }));
    }

    #[test]
    fn unknown_group_in_files_errors() {
        let err = parse_toml(&format!(
            r#"
[operators]
op = "{X25519_MIKEY}"

[files]
"f" = {{ recipients = ["@bogus"] }}
"#,
        ))
        .unwrap_err();
        assert!(matches!(err, RecipientsError::UnknownGroup { .. }));
    }

    #[test]
    fn operator_in_file_recipients_errors() {
        let err = parse_toml(&format!(
            r#"
[operators]
op = "{X25519_MIKEY}"

[machines]
m = "{SSH_RPI}"

[files]
"f" = {{ recipients = ["op", "m"] }}
"#,
        ))
        .unwrap_err();
        match err {
            RecipientsError::OperatorInFileRecipients { file, operator } => {
                assert_eq!(file, "f");
                assert_eq!(operator, "op");
            }
            other => panic!("wrong variant: {other:?}"),
        }
    }

    #[test]
    fn empty_recipients_with_operators_is_valid() {
        // The whole point of the implicit-operators rule.
        let r = parse_toml(&format!(
            r#"
[operators]
op = "{X25519_MIKEY}"

[files]
"only_op" = {{ recipients = [] }}
"#,
        ))
        .unwrap();
        assert_eq!(resolved_aliases(&r, "only_op"), vec!["op"]);
    }

    #[test]
    fn empty_recipients_without_operators_errors() {
        let err = parse_toml(
            r#"
[files]
"orphan" = { recipients = [] }
"#,
        )
        .unwrap_err();
        match err {
            RecipientsError::EmptyEffectiveRecipients { file } => assert_eq!(file, "orphan"),
            other => panic!("wrong variant: {other:?}"),
        }
    }

    #[test]
    fn no_operators_with_machine_recipients_is_valid() {
        // `[operators]` may be empty as long as every file names at least
        // one machine.
        let r = parse_toml(&format!(
            r#"
[machines]
m = "{SSH_RPI}"

[files]
"f" = {{ recipients = ["m"] }}
"#,
        ))
        .unwrap();
        assert_eq!(r.resolve("f").unwrap().len(), 1);
    }
}
