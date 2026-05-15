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
    let decryptor = age::Decryptor::new(ciphertext).map_err(|source| DecryptError::Decrypt {
        path: path.to_path_buf(),
        source: Box::new(source),
    })?;
    let mut reader = decryptor
        .decrypt(std::iter::once(identity.as_age()))
        .map_err(|source| DecryptError::Decrypt {
            path: path.to_path_buf(),
            source: Box::new(source),
        })?;

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
    use super::*;
    use age::x25519;
    use secrecy::ExposeSecret;

    fn x25519_recipient(id: &x25519::Identity) -> Box<dyn Recipient + Send> {
        Box::new(id.to_public())
    }

    #[test]
    fn round_trip_x25519() {
        let id = x25519::Identity::generate();
        let recipients = vec![x25519_recipient(&id)];
        let ct = encrypt_bytes(&recipients, Path::new("test"), b"hello").unwrap();

        // Header is readable.
        let stanzas = read_header_stanzas(&ct).unwrap();
        assert!(stanzas.iter().any(|s| s.tag == "X25519"));

        // Round-trip through Identity.
        let identity: crate::identity::Identity = id.to_string().expose_secret().parse().unwrap();
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
        let id = x25519::Identity::generate();
        let identity: crate::identity::Identity = id.to_string().expose_secret().parse().unwrap();
        let err = decrypt_bytes(&identity, Path::new("test"), b"garbage").unwrap_err();
        assert!(matches!(err, DecryptError::Decrypt { .. }));
    }
}
