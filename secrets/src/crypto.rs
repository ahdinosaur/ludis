//! Raw age encryption / decryption primitives, plus a small header scanner
//! used by `rekey` to decide whether a re-encrypt is a no-op.
//!
//! Everything in this module operates on in-memory byte slices - file I/O
//! lives in the caller.

use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use age::Recipient;
use age_core::format::Stanza;
use age_core::format::read::age_stanza;
use displaydoc::Display;
use secrecy::SecretBox;
use thiserror::Error;

use crate::Secret;
use crate::identity::Identity;

const AGE_V1_MAGIC: &[u8] = b"age-encryption.org/v1\n";
const HEADER_MAC_PREFIX: &[u8] = b"--- ";

/// Decrypt a single age-encrypted payload.
///
/// `path` is used only for labelling errors; the bytes themselves come from
/// `ciphertext`.
pub(crate) fn decrypt_bytes(
    identity: &Identity,
    path: &Path,
    ciphertext: &[u8],
) -> Result<Secret, DecryptError> {
    let decryptor =
        age::Decryptor::new(ciphertext).map_err(|source| map_decrypt_error(path, source))?;
    let mut reader = decryptor
        .decrypt(std::iter::once(identity.as_age()))
        .map_err(|source| map_decrypt_error(path, source))?;

    let mut plaintext = Vec::new();
    reader
        .read_to_end(&mut plaintext)
        .map_err(|source| DecryptError::DecryptIo {
            path: path.to_path_buf(),
            source,
        })?;

    let plaintext = String::from_utf8(plaintext).map_err(|_| DecryptError::NotUtf8 {
        path: path.to_path_buf(),
    })?;
    Ok(Arc::new(SecretBox::new(Box::new(plaintext))))
}

/// Encrypt `plaintext` to `recipients`, returning the age ciphertext as a
/// byte vector.
///
/// `path` is only used for error labelling. `recipients` must be non-empty -
/// age rejects an empty recipient set.
pub(crate) fn encrypt_bytes(
    recipients: &[Box<dyn Recipient + Send>],
    path: &Path,
    plaintext: &[u8],
) -> Result<Vec<u8>, EncryptError> {
    let encryptor =
        age::Encryptor::with_recipients(recipients.iter().map(|r| &**r as &dyn Recipient))
            .map_err(|source| EncryptError::Build {
                path: path.to_path_buf(),
                source: Box::new(source),
            })?;
    let mut out = Vec::new();
    let mut writer = encryptor
        .wrap_output(&mut out)
        .map_err(|source| EncryptError::WrapIo {
            path: path.to_path_buf(),
            source,
        })?;
    writer
        .write_all(plaintext)
        .map_err(|source| EncryptError::WrapIo {
            path: path.to_path_buf(),
            source,
        })?;
    writer.finish().map_err(|source| EncryptError::WrapIo {
        path: path.to_path_buf(),
        source,
    })?;
    Ok(out)
}

/// Read just the recipient stanzas from an age v1 ciphertext header.
///
/// We only need the stanzas' tags and first argument to compare against the
/// intended recipient list - body and MAC are ignored. Returns the stanzas
/// in file order. Does not authenticate the header.
pub(crate) fn read_header_stanzas(ciphertext: &[u8]) -> Result<Vec<Stanza>, HeaderError> {
    if !ciphertext.starts_with(AGE_V1_MAGIC) {
        return Err(HeaderError::BadMagic);
    }
    let mut remaining = &ciphertext[AGE_V1_MAGIC.len()..];
    let mut stanzas = Vec::new();
    while remaining.starts_with(b"-> ") {
        let (rest, stanza) = age_stanza(remaining).map_err(|_| HeaderError::Malformed)?;
        stanzas.push(Stanza::from(stanza));
        remaining = rest;
    }
    if !remaining.starts_with(HEADER_MAC_PREFIX) {
        return Err(HeaderError::Malformed);
    }
    Ok(stanzas)
}

/// Translate an `age::DecryptError` to our richer surface. `NoMatchingKeys`
/// gets its own variant because it's the symptom an operator sees when their
/// SSH identity doesn't match any stanza in the file - most commonly because
/// the ciphertext pre-dates the x25519 → SSH migration. Surfacing it as a
/// distinct variant lets the Display message carry the rekey hint.
fn map_decrypt_error(path: &Path, source: age::DecryptError) -> DecryptError {
    if matches!(source, age::DecryptError::NoMatchingKeys) {
        return DecryptError::NoMatchingKeys {
            path: path.to_path_buf(),
        };
    }
    DecryptError::Decrypt {
        path: path.to_path_buf(),
        source: Box::new(source),
    }
}

#[derive(Debug, Error, Display)]
pub enum DecryptError {
    /// Failed to decrypt {path}: {source}
    Decrypt {
        path: PathBuf,
        // Boxed: `age::DecryptError` is ~128 bytes, which pushes `Result`
        // past clippy's `result_large_err` threshold. Boxing keeps the
        // hot success path cheap.
        #[source]
        source: Box<age::DecryptError>,
    },

    /// No matching key for {path}; this ciphertext may pre-date the x25519 → SSH migration. Re-encrypt with `lusid secrets rekey` after updating `[operators]` in `lusid-secrets.toml` to your SSH public key.
    NoMatchingKeys { path: PathBuf },

    /// I/O error while decrypting {path}: {source}
    DecryptIo {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// Decrypted bytes for {path} are not valid UTF-8
    NotUtf8 { path: PathBuf },
}

#[derive(Debug, Error, Display)]
pub enum EncryptError {
    /// Failed to build age encryptor for {path}: {source}
    Build {
        path: PathBuf,
        // Boxed: see the matching comment on `DecryptError::Decrypt`.
        #[source]
        source: Box<age::EncryptError>,
    },

    /// I/O error while encrypting {path}: {source}
    WrapIo {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },
}

#[derive(Debug, Error, Display)]
pub enum HeaderError {
    /// Not an age v1 file (missing magic)
    BadMagic,

    /// Age header is malformed or truncated
    Malformed,
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use secrecy::ExposeSecret;

    use super::*;
    use crate::test_fixtures::{
        TEST_SSH_ED25519_A_PRIV, TEST_SSH_ED25519_A_PUB, TEST_SSH_ED25519_B_PRIV,
    };

    fn ssh_recipient(pubkey: &str) -> Box<dyn Recipient + Send> {
        // age::ssh::Recipient drops trailing comments via FromStr; we still
        // canonicalise here to mirror what Key::from_str does in production.
        let mut parts = pubkey.split_whitespace();
        let kind = parts.next().unwrap();
        let body = parts.next().unwrap();
        Box::new(age::ssh::Recipient::from_str(&format!("{kind} {body}")).unwrap())
    }

    #[test]
    fn round_trip_ssh() {
        let recipients = vec![ssh_recipient(TEST_SSH_ED25519_A_PUB)];
        let ct = encrypt_bytes(&recipients, Path::new("test"), b"hello").unwrap();

        // Header is readable; one of the stanzas is an SSH stanza.
        let stanzas = read_header_stanzas(&ct).unwrap();
        assert!(stanzas.iter().any(|s| s.tag == "ssh-ed25519"));

        // Round-trip through Identity.
        let identity: crate::identity::Identity = TEST_SSH_ED25519_A_PRIV.parse().unwrap();
        let pt = decrypt_bytes(&identity, Path::new("test"), &ct).unwrap();
        assert_eq!(pt.expose_secret().as_str(), "hello");
    }

    #[test]
    fn header_bad_magic() {
        assert!(matches!(
            read_header_stanzas(b"not an age file"),
            Err(HeaderError::BadMagic)
        ));
    }

    #[test]
    fn decrypts_invalid_ciphertext_fails() {
        let identity: crate::identity::Identity = TEST_SSH_ED25519_A_PRIV.parse().unwrap();
        let err = decrypt_bytes(&identity, Path::new("test"), b"garbage").unwrap_err();
        assert!(matches!(err, DecryptError::Decrypt { .. }));
    }

    /// A ciphertext encrypted to keypair A, decrypted with keypair B, must
    /// surface as `NoMatchingKeys` (not the generic `Decrypt`) so the
    /// migration hint in Display reaches the operator.
    #[test]
    fn unrelated_identity_returns_no_matching_keys() {
        let recipients = vec![ssh_recipient(TEST_SSH_ED25519_A_PUB)];
        let ct = encrypt_bytes(&recipients, Path::new("test"), b"hi").unwrap();
        let other: crate::identity::Identity = TEST_SSH_ED25519_B_PRIV.parse().unwrap();
        let err = decrypt_bytes(&other, Path::new("test"), &ct).unwrap_err();
        assert!(matches!(err, DecryptError::NoMatchingKeys { .. }));
    }
}
