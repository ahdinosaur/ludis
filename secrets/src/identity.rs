//! Decryption identity: either an x25519 age key or an SSH private key.
//!
//! An identity file contains a single key. Comments (`#`) and blank lines at
//! the top are skipped; the first non-comment line determines the format:
//!
//! - `AGE-SECRET-KEY-1...` (one line)     - x25519 age identity.
//! - `-----BEGIN OPENSSH PRIVATE KEY-----` - multi-line OpenSSH ed25519 or RSA
//!   private key. The whole BEGIN..END block (plus any trailing newline) is
//!   handed to [`age::ssh::Identity::from_buffer`].
//!
//! Passphrase-protected SSH keys are rejected up-front
//! ([`IdentityError::EncryptedSsh`]) because decrypting them would require
//! prompting during `lusid-apply`.
//!
//! TODO(cc): support passphrase-protected SSH identities. Needs an
//! interactive prompt path (or ssh-agent integration) wired through the
//! secrets pipeline so the decryption credential can arrive after the
//! apply process has started.

use std::io::Cursor;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use age_core::format::{FileKey, Stanza};
use displaydoc::Display;
use secrecy::{ExposeSecret, SecretBox};
use thiserror::Error;
use tokio::fs;

const X25519_PREFIX: &str = "AGE-SECRET-KEY-";
const OPENSSH_BEGIN: &str = "-----BEGIN OPENSSH PRIVATE KEY-----";

/// A decryption identity loaded from a file or string.
///
/// Parse via [`Identity::from_file`] / [`FromStr`]. Pass the result to any
/// API taking a `&dyn age::Identity` via [`Identity::as_age`].
pub struct Identity {
    inner: IdentityInner,
}

enum IdentityInner {
    X25519(age::x25519::Identity),
    Ssh(age::ssh::Identity),
}

impl std::fmt::Debug for Identity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Never leak key material via Debug - just report the kind.
        let kind = match &self.inner {
            IdentityInner::X25519(_) => "x25519",
            IdentityInner::Ssh(_) => "ssh",
        };
        f.debug_struct("Identity").field("kind", &kind).finish()
    }
}

impl Identity {
    /// Read an identity file from disk. See module docs for the accepted formats.
    ///
    /// The file contents are held in a [`SecretBox`] for the duration of
    /// parsing, so the raw key material buffer is zeroised as soon as this
    /// function returns. The parsed [`Identity`] keeps its key inside the
    /// `age` crate's own `SecretString` envelope.
    pub async fn from_file(path: &Path) -> Result<Self, IdentityError> {
        let text: SecretBox<String> =
            SecretBox::new(Box::new(fs::read_to_string(path).await.map_err(
                |source| IdentityError::Read {
                    path: path.to_path_buf(),
                    source,
                },
            )?));
        parse(text.expose_secret(), Some(path))
    }

    /// Borrow this identity as the age crate's trait object, for use with
    /// [`age::Decryptor::decrypt`].
    pub fn as_age(&self) -> &dyn age::Identity {
        match &self.inner {
            IdentityInner::X25519(id) => id,
            IdentityInner::Ssh(id) => id,
        }
    }
}

impl age::Identity for Identity {
    fn unwrap_stanza(&self, stanza: &Stanza) -> Option<Result<FileKey, age::DecryptError>> {
        self.as_age().unwrap_stanza(stanza)
    }
}

impl FromStr for Identity {
    type Err = IdentityError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse(s, None)
    }
}

fn parse(text: &str, path: Option<&Path>) -> Result<Identity, IdentityError> {
    let first = first_content_line(text).ok_or_else(|| IdentityError::Empty {
        path: path.map(Path::to_path_buf),
    })?;

    if first.starts_with(X25519_PREFIX) {
        let inner = age::x25519::Identity::from_str(first).map_err(IdentityError::ParseX25519)?;
        Ok(Identity {
            inner: IdentityInner::X25519(inner),
        })
    } else if first.starts_with(OPENSSH_BEGIN) {
        // Pass from the BEGIN line onward - comments have been skipped but
        // blank/content lines inside the block are preserved.
        let begin = text
            .find(OPENSSH_BEGIN)
            .expect("first line started with it");
        let body = &text[begin..];
        let filename = path.map(|p| p.display().to_string());
        let ssh =
            age::ssh::Identity::from_buffer(Cursor::new(body), filename).map_err(|source| {
                IdentityError::ParseSsh {
                    path: path.map(Path::to_path_buf),
                    source,
                }
            })?;
        match ssh {
            age::ssh::Identity::Unencrypted(_) => Ok(Identity {
                inner: IdentityInner::Ssh(ssh),
            }),
            age::ssh::Identity::Encrypted(_) => Err(IdentityError::EncryptedSsh {
                path: path.map(Path::to_path_buf),
            }),
            age::ssh::Identity::Unsupported(_) => Err(IdentityError::UnsupportedSsh {
                path: path.map(Path::to_path_buf),
            }),
        }
    } else {
        Err(IdentityError::UnknownFormat {
            path: path.map(Path::to_path_buf),
        })
    }
}

/// First non-blank, non-comment line in `text`, trimmed.
fn first_content_line(text: &str) -> Option<&str> {
    text.lines()
        .map(str::trim)
        .find(|l| !l.is_empty() && !l.starts_with('#'))
}

#[derive(Debug, Error, Display)]
pub enum IdentityError {
    /// Failed to read identity file {path}: {source}
    Read {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// Identity {path:?} has no key line
    Empty { path: Option<PathBuf> },

    /// Identity {path:?} is not in a recognised format (expected AGE-SECRET-KEY-... or -----BEGIN OPENSSH PRIVATE KEY-----)
    UnknownFormat { path: Option<PathBuf> },

    /// Failed to parse x25519 identity: {0}
    ParseX25519(&'static str),

    /// Failed to parse SSH identity {path:?}: {source}
    ParseSsh {
        path: Option<PathBuf>,
        #[source]
        source: std::io::Error,
    },

    /// SSH identity {path:?} is passphrase-protected, which is not supported
    EncryptedSsh { path: Option<PathBuf> },

    /// SSH identity {path:?} uses an unsupported key type (supported: ed25519, rsa)
    UnsupportedSsh { path: Option<PathBuf> },
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_X25519: &str =
        "AGE-SECRET-KEY-1GQ9778VQXMMJVE8SK7J6VT8UJ4HDQAJUVSFCWCM02D8GEWQ72PVQ2Y5J33";

    // Unencrypted ed25519 SSH private key from the age crate's test vectors.
    const TEST_SSH_ED25519: &str = "-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZW
QyNTUxOQAAACB7Ci6nqZYaVvrjm8+XbzII89TsXzP111AflR7WeorBjQAAAJCfEwtqnxML
agAAAAtzc2gtZWQyNTUxOQAAACB7Ci6nqZYaVvrjm8+XbzII89TsXzP111AflR7WeorBjQ
AAAEADBJvjZT8X6JRJI8xVq/1aU8nMVgOtVnmdwqWwrSlXG3sKLqeplhpW+uObz5dvMgjz
1OxfM/XXUB+VHtZ6isGNAAAADHN0cjRkQGNhcmJvbgE=
-----END OPENSSH PRIVATE KEY-----
";

    // Passphrase-protected ("passphrase") ed25519 key, same public key as TEST_SSH_ED25519.
    const TEST_SSH_ED25519_ENCRYPTED: &str = "-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAACmFlczI1Ni1jdHIAAAAGYmNyeXB0AAAAGAAAABBSs0SUhQ
958xWERf6ibyf2AAAAEAAAAAEAAAAzAAAAC3NzaC1lZDI1NTE5AAAAIHsKLqeplhpW+uOb
z5dvMgjz1OxfM/XXUB+VHtZ6isGNAAAAkLvH9UsJa+ulewsZT2YtEkme1y9UZKI/vUbTms
LVqWdLprBQIm3IClfGso6IPW7+imkwYRHPKYfBYGYuexzO8b+LRiZU5/lDQmsvZA3asNxp
KjW7kUOJnI8dAeaqJa18P7XkAuzcuZmVoCTurqEOSeb5Ww9Nq0csB0zkF22/PeWy3+BZW5
hDsL1OfQl4WbakZQ==
-----END OPENSSH PRIVATE KEY-----
";

    #[test]
    fn parses_x25519() {
        let id: Identity = TEST_X25519.parse().unwrap();
        assert!(matches!(id.inner, IdentityInner::X25519(_)));
    }

    #[test]
    fn parses_x25519_with_leading_comments() {
        let with_comments =
            format!("# created: 2024-01-01T00:00:00Z\n# public key: age1...\n{TEST_X25519}\n");
        let id: Identity = with_comments.parse().unwrap();
        assert!(matches!(id.inner, IdentityInner::X25519(_)));
    }

    #[test]
    fn parses_openssh_ed25519() {
        let id: Identity = TEST_SSH_ED25519.parse().unwrap();
        assert!(matches!(id.inner, IdentityInner::Ssh(_)));
    }

    #[test]
    fn rejects_passphrase_protected_ssh() {
        let err = TEST_SSH_ED25519_ENCRYPTED.parse::<Identity>().unwrap_err();
        assert!(matches!(err, IdentityError::EncryptedSsh { .. }));
    }

    #[test]
    fn rejects_empty() {
        assert!(matches!(
            "".parse::<Identity>().unwrap_err(),
            IdentityError::Empty { .. }
        ));
        assert!(matches!(
            "# only a comment\n\n".parse::<Identity>().unwrap_err(),
            IdentityError::Empty { .. }
        ));
    }

    #[test]
    fn rejects_unknown_format() {
        let err = "not a key".parse::<Identity>().unwrap_err();
        assert!(matches!(err, IdentityError::UnknownFormat { .. }));
    }
}
