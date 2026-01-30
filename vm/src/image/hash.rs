use lusid_fs::{self as fs, FsError};
use sha2::Digest;
use std::path::Path;
use thiserror::Error;
use tokio::io::AsyncReadExt;

use crate::image::index::{VmImageHashRef, VmImageIndex};

#[derive(Error, Debug)]
pub enum VmImageHashError {
    #[error(transparent)]
    Fs(#[from] FsError),

    #[error("malformed file name from '{url}'")]
    MalformedFileName { url: String },

    #[error("hash sums missing entry for image '{name}'")]
    HashNotFound { name: String },

    #[error("malformed sha512sums line {line_index}: '{line}'")]
    MalformedLine { line_index: usize, line: String },

    #[error("sha512 mismatch for '{name}': expected {expected}, actual {actual}")]
    HashMismatch {
        name: String,
        expected: String,
        actual: String,
    },
}

#[derive(Debug, Clone)]
pub enum VmImageHash<'a> {
    Sha512Sums { path: &'a Path },
    Sha256Sums { path: &'a Path },
}

impl<'a> VmImageHash<'a> {
    pub fn new(hash_ref: &VmImageHashRef, path: &'a Path) -> Self {
        match hash_ref {
            VmImageHashRef::Sha512Sums { url: _ } => VmImageHash::Sha512Sums { path },
            VmImageHashRef::Sha256Sums { url: _ } => VmImageHash::Sha256Sums { path },
        }
    }
    pub async fn validate(
        &self,
        image_index: &VmImageIndex,
        image_path: &Path,
    ) -> Result<(), VmImageHashError> {
        match self {
            VmImageHash::Sha512Sums { path: hash_path } => {
                self.validate_hash::<Sha512>(image_index, image_path, hash_path)
                    .await
            }
            VmImageHash::Sha256Sums { path: hash_path } => {
                self.validate_hash::<Sha256>(image_index, image_path, hash_path)
                    .await
            }
        }
    }

    pub async fn validate_hash<H: HashType>(
        &self,
        image_index: &VmImageIndex,
        image_path: &Path,
        hash_path: &Path,
    ) -> Result<(), VmImageHashError> {
        let sums = fs::read_file_to_string(hash_path).await?;

        // Resolve the target name we need to look up in sums
        let image_url = image_index.image.to_url();
        let image_name = image_url.split('/').next_back().ok_or_else(|| {
            VmImageHashError::MalformedFileName {
                url: image_url.to_string(),
            }
        })?;

        let expected = lookup_sum(&sums, image_name, H::hex_length())?;

        let actual = H::file_to_hex(image_path).await?;

        // Compare (case-insensitive to be safe)
        if expected.eq_ignore_ascii_case(&actual) {
            Ok(())
        } else {
            Err(VmImageHashError::HashMismatch {
                name: image_name.to_string(),
                expected,
                actual,
            })
        }
    }
}

pub trait HashType {
    fn hex_length() -> usize;

    async fn file_to_hex<P: AsRef<Path>>(path: P) -> Result<String, FsError>;
}

pub trait Sha2HashType {
    type Digest: Digest;
}

impl<H: Sha2HashType> HashType for H {
    fn hex_length() -> usize {
        H::Digest::output_size() * 2
    }

    async fn file_to_hex<P: AsRef<Path>>(path: P) -> Result<String, FsError> {
        let p = path.as_ref();
        let mut file = fs::open_file(p).await?;
        let mut hasher = H::Digest::new();
        let mut buf = [0u8; 8192];

        loop {
            let n = file
                .read(&mut buf)
                .await
                .map_err(|source| FsError::ReadFile {
                    path: p.to_path_buf(),
                    source,
                })?;
            if n == 0 {
                break;
            }
            hasher.update(&buf[..n]);
        }

        let digest = hasher.finalize();
        let mut hex = String::with_capacity(digest.len() * 2);
        for b in digest {
            hex.push_str(&format!("{:02x}", b));
        }
        Ok(hex)
    }
}

pub struct Sha256;
impl Sha2HashType for Sha256 {
    type Digest = sha2::Sha256;
}

pub struct Sha512;
impl Sha2HashType for Sha512 {
    type Digest = sha2::Sha512;
}

/// Parse the contents of a SHASUMS file and return the hash that
/// corresponds to `image_name`.
///
/// Accepts lines like:
/// <hex> [space][space or more][optional '*']<filename>
/// Ignores empty lines and lines starting with '#'.
fn lookup_sum(sums: &str, image_name: &str, hex_length: usize) -> Result<String, VmImageHashError> {
    for (idx, raw_line) in sums.lines().enumerate() {
        let line = raw_line.trim_end_matches('\r').trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        // Split into two parts: hash and the rest (file name). Using splitn to
        // avoid splitting file names that might (rarely) contain spaces.
        let (hash, name_part) = if let Some((h, rest)) = split_once_whitespace(line) {
            (h, rest)
        } else {
            return Err(VmImageHashError::MalformedLine {
                line_index: idx + 1,
                line: raw_line.to_string(),
            });
        };

        // Normalize filename token: handle optional leading '*' (binary mode).
        let listed_name = name_part.trim_start_matches('*');

        // Some sums may include paths. Compare only the file name component.
        let listed_basename = std::path::Path::new(listed_name)
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or(listed_name);

        // Validate hash shape: 128 hex chars
        if hash.len() != hex_length || !hash.chars().all(|c| c.is_ascii_hexdigit()) {
            return Err(VmImageHashError::MalformedLine {
                line_index: idx + 1,
                line: raw_line.to_string(),
            });
        }

        if listed_basename == image_name {
            return Ok(hash.to_ascii_lowercase());
        }
    }

    Err(VmImageHashError::HashNotFound {
        name: image_name.to_string(),
    })
}

/// Split `s` into two parts at the first run of ASCII whitespace:
/// (left, right-without-leading-whitespace).
fn split_once_whitespace(s: &str) -> Option<(&str, &str)> {
    let bytes = s.as_bytes().iter().enumerate();
    for (i, b) in bytes {
        if b.is_ascii_whitespace() {
            // Skip all following whitespace to get start of right part
            let mut j = i;
            let sb = s.as_bytes();
            while j < sb.len() && sb[j].is_ascii_whitespace() {
                j += 1;
            }
            return Some((&s[..i], &s[j..]));
        }
    }
    None
}
