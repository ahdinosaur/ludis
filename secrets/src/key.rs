//! Recipient-side keys.
//!
//! Parsed from the strings declared in `lusid-secrets.toml`'s `[operators]`
//! and `[machines]` tables. Each value is an SSH public key (`ssh-ed25519 ...`
//! / `ssh-rsa ...`) - the same key the operator uses for SSH auth.

use std::collections::HashSet;
use std::str::FromStr;

use age_core::format::{FileKey, Stanza};
use displaydoc::Display;
use thiserror::Error;

/// A parsed SSH recipient. Parsed eagerly so malformed keys surface at load
/// time rather than on first use.
#[derive(Debug, Clone)]
pub struct Key {
    inner: age::ssh::Recipient,
}

impl Key {
    pub(crate) fn recipient(&self) -> &age::ssh::Recipient {
        &self.inner
    }
}

impl age::Recipient for Key {
    fn wrap_file_key(
        &self,
        file_key: &FileKey,
    ) -> Result<(Vec<Stanza>, HashSet<String>), age::EncryptError> {
        self.inner.wrap_file_key(file_key)
    }
}

impl FromStr for Key {
    type Err = KeyParseError;

    /// Parse an SSH recipient (`ssh-ed25519 ...` or `ssh-rsa ...`). Trailing
    /// SSH comments (`... user@host`) are tolerated and stripped.
    ///
    /// Rejects `age1...` x25519 recipients with a specific error so operators
    /// migrating from the old schema see a clear hint rather than the generic
    /// "unknown prefix".
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let trimmed = s.trim();
        if trimmed.starts_with("age1") {
            return Err(KeyParseError::X25519NoLongerSupported);
        }
        if !trimmed.starts_with("ssh-") {
            return Err(KeyParseError::UnknownPrefix);
        }
        let mut parts = trimmed.split_whitespace();
        let kind = parts.next().ok_or(KeyParseError::Empty)?;
        let body = parts.next().ok_or(KeyParseError::Empty)?;
        let canonical = format!("{kind} {body}");
        let inner = age::ssh::Recipient::from_str(&canonical).map_err(KeyParseError::Ssh)?;
        Ok(Key { inner })
    }
}

impl std::fmt::Display for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

#[derive(Debug, Error, Display)]
pub enum KeyParseError {
    /// Empty recipient
    Empty,

    /// Unknown recipient prefix (expected ssh-ed25519 or ssh-rsa)
    UnknownPrefix,

    /// x25519 recipients (`age1...`) are no longer supported; declare an SSH public key instead (e.g. the contents of `~/.ssh/id_ed25519.pub`)
    X25519NoLongerSupported,

    /// Invalid SSH recipient: {0:?}
    Ssh(age::ssh::ParseRecipientKeyError),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_fixtures::TEST_SSH_ED25519_A_PUB;

    #[test]
    fn parses_ssh_ed25519_recipient() {
        let _: Key = TEST_SSH_ED25519_A_PUB.parse().unwrap();
    }

    #[test]
    fn parses_ssh_with_comment() {
        // The fixture already carries a trailing comment; verify a different
        // comment also parses to keep regression coverage on the strip path.
        let with_comment = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHsKLqeplhpW+uObz5dvMgjz1OxfM/XXUB+VHtZ6isGN alice@host";
        let _: Key = with_comment.parse().unwrap();
    }

    #[test]
    fn rejects_x25519_with_specific_error() {
        let err = "age1t7rxyev2z3rw82stdlrrepyc39nvn86l5078zqkf5uasdy86jp6svpy7pa"
            .parse::<Key>()
            .unwrap_err();
        assert!(matches!(err, KeyParseError::X25519NoLongerSupported));
    }

    #[test]
    fn rejects_unknown_prefix() {
        assert!(matches!(
            "not-a-key".parse::<Key>().unwrap_err(),
            KeyParseError::UnknownPrefix
        ));
    }
}
