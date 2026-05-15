//! Substring-replace secret plaintexts out of arbitrary strings.

use secrecy::ExposeSecret;

use crate::secrets::Secret;

/// Minimum plaintext length eligible for redaction. Shorter secrets are
/// skipped to avoid pathological false positives when substring-matching
/// against arbitrary process output.
pub(crate) const REDACT_MIN_LEN: usize = 8;

/// Placeholder string substituted in place of matched secret plaintext.
pub(crate) const REDACTED: &str = "<redacted>";

/// Substring-replaces secret plaintexts with [`REDACTED`] in arbitrary
/// strings. Intended for scrubbing `lusid-apply`'s per-operation stdout
/// and stderr lines before they are streamed to the TUI.
///
/// Limitations (read before trusting this for anything load-bearing):
///
/// - **Substring-only.** A secret that appears base64-encoded, escaped,
///   JSON-serialised, or chunked across multiple read boundaries will not
///   be caught. This is a best-effort scrub, not a guarantee.
/// - **Short secrets are skipped.** See [`REDACT_MIN_LEN`].
/// - **Emits plaintext briefly** via [`ExposeSecret`] during each call;
///   the plaintext is not copied but is borrowed for the length of one
///   `String::replace`.
/// - **Overlapping/adjacent secrets are not reliably caught.** Longest-first
///   ordering handles the nested case (secret B is a substring of secret A)
///   but not the interleaved case: if A = "foobar" and B = "barfoo" both
///   appear in "foobarfoo", only one of them will redact, leaving the
///   other's plaintext visible. In practice this would need two secrets
///   that share a suffix/prefix by coincidence; flagging anyway.
#[derive(Clone)]
pub struct Redactor {
    secrets: Vec<Secret>,
}

impl std::fmt::Debug for Redactor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Redactor")
            .field("len", &self.secrets.len())
            .finish()
    }
}

impl Redactor {
    /// No-op redactor (no secrets).
    pub fn empty() -> Self {
        Self {
            secrets: Vec::new(),
        }
    }

    pub(crate) fn new(secrets: Vec<Secret>) -> Self {
        Self { secrets }
    }

    /// Replace every occurrence of every registered secret plaintext in
    /// `input` with [`REDACTED`]. Returns `input` unchanged when no
    /// secrets match (including the trivial empty-redactor case).
    pub fn redact(&self, input: &str) -> String {
        if self.secrets.is_empty() || input.is_empty() {
            return input.to_string();
        }
        let mut out = input.to_string();
        for secret in &self.secrets {
            let plaintext = secret.expose_secret();
            if out.contains(plaintext.as_str()) {
                out = out.replace(plaintext.as_str(), REDACTED);
            }
        }
        out
    }

    pub fn is_empty(&self) -> bool {
        self.secrets.is_empty()
    }

    pub fn len(&self) -> usize {
        self.secrets.len()
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use secrecy::SecretBox;

    use super::*;
    use crate::secrets::Secrets;

    fn secret_of(s: &str) -> Secret {
        Arc::new(SecretBox::new(Box::new(s.to_string())))
    }

    fn secrets_from(pairs: &[(&str, &str)]) -> Secrets {
        let values = pairs
            .iter()
            .map(|(k, v)| ((*k).to_owned(), secret_of(v)))
            .collect();
        Secrets::from_values(values)
    }

    #[test]
    fn empty_is_noop() {
        let redactor = Redactor::empty();
        assert_eq!(redactor.redact("hello world"), "hello world");
        assert!(redactor.is_empty());
    }

    #[test]
    fn replaces_occurrences() {
        let secrets = secrets_from(&[("api_key", "supersecretvalue")]);
        let redactor = secrets.redactor();
        assert_eq!(
            redactor.redact("auth: supersecretvalue; retrying supersecretvalue"),
            "auth: <redacted>; retrying <redacted>"
        );
    }

    #[test]
    fn skips_short_secrets() {
        // Below REDACT_MIN_LEN (8) - skipped entirely to avoid false
        // positives on common short substrings.
        let secrets = secrets_from(&[("pin", "12345")]);
        let redactor = secrets.redactor();
        assert!(redactor.is_empty());
        assert_eq!(redactor.redact("pin is 12345"), "pin is 12345");
    }

    #[test]
    fn prefers_longer_patterns() {
        // Two secrets where one plaintext is a substring of the other:
        // longer-first ordering ensures the outer pattern is redacted as
        // a whole rather than leaving a fragment after the inner match.
        let secrets = secrets_from(&[("outer", "aaaaaaaabbbbbbbb"), ("inner", "aaaaaaaabb")]);
        let redactor = secrets.redactor();
        assert_eq!(
            redactor.redact("value=aaaaaaaabbbbbbbb done"),
            "value=<redacted> done"
        );
    }

    #[test]
    fn handles_empty_input() {
        let secrets = secrets_from(&[("k", "eightchars")]);
        let redactor = secrets.redactor();
        assert_eq!(redactor.redact(""), "");
    }

    #[test]
    fn no_match_returns_input_unchanged() {
        let secrets = secrets_from(&[("k", "eightchars")]);
        let redactor = secrets.redactor();
        assert_eq!(redactor.redact("nothing to see"), "nothing to see");
    }
}
