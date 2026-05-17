use std::fmt::{self, Display};
use std::path::Path;

use async_trait::async_trait;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_ctx::Context;
use lusid_fs::{self as fs, FsError};
use lusid_operation::{
    Operation,
    operations::file::{FileGroup, FileMode, FileOperation, FilePath, FileSource, FileUser},
};
use lusid_params::{ParseError, ParseParams, StructFields};
use rimu::{Span, Spanned, Value};
use secrecy::ExposeSecret;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use thiserror::Error;

use crate::{ChangeKind, ResourceChangeTrait, ResourceType};

/// Byte payload of a file as it appears in [`FileState`] / [`FileChange`].
///
/// Carrying the bytes lets downstream renderers (the TUI detail pane in
/// particular) show unified diffs of the change. For sources flagged as
/// secret, we ship `Redacted { len, sha256 }` instead so the plaintext never
/// hits operator scrollback. The hash still answers "did this secret
/// change?" without revealing the content.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Content {
    Bytes(#[serde(with = "base64_bytes")] Vec<u8>),
    Redacted { len: usize, sha256: String },
}

impl Content {
    /// Construct from raw bytes. When `redact` is set, the bytes are hashed
    /// and dropped; otherwise they're carried verbatim.
    pub fn from_bytes(bytes: Vec<u8>, redact: bool) -> Self {
        if redact {
            Content::redacted(&bytes)
        } else {
            Content::Bytes(bytes)
        }
    }

    /// Hash `bytes` into a [`Content::Redacted`] without ever storing them.
    pub fn redacted(bytes: &[u8]) -> Self {
        let mut hasher = Sha256::new();
        hasher.update(bytes);
        let digest: [u8; 32] = hasher.finalize().into();
        Content::Redacted {
            len: bytes.len(),
            sha256: hex_encode(&digest),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Content::Bytes(b) => b.len(),
            Content::Redacted { len, .. } => *len,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Display for Content {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Content::Bytes(b) => write!(f, "Bytes({} bytes)", b.len()),
            Content::Redacted { len, sha256 } => {
                let prefix = sha256.get(..16).unwrap_or(sha256.as_str());
                write!(f, "Redacted({len} bytes, sha256:{prefix})")
            }
        }
    }
}

fn hex_encode(bytes: &[u8]) -> String {
    use std::fmt::Write;
    let mut s = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        let _ = write!(&mut s, "{b:02x}");
    }
    s
}

/// True when `source` points into (or under) `secrets_dir`.
///
/// Comparison is lexical path-prefix-based; both arguments are expected to
/// be absolute and free of `.`/`..` components. The caller (`lusid-apply`)
/// canonicalises `secrets_dir` before passing it in. A non-absolute
/// `secrets_dir` short-circuits to `false` rather than producing a
/// confusing false match.
///
/// Note(cc): a `source` containing `..` segments that walks through
/// `secrets_dir` (e.g. `/proj/secrets/../leaked/api.txt`) will not match -
/// future hardening could canonicalise `source` too, but that requires
/// the file to exist (cleared by `validate_host_paths`) and an async
/// I/O hop at construction time.
pub fn is_secret_source(source: &FilePath, secrets_dir: &Path) -> bool {
    if !secrets_dir.is_absolute() {
        return false;
    }
    source.as_path().starts_with(secrets_dir)
}

mod base64_bytes {
    use base64::Engine;
    use base64::engine::general_purpose::STANDARD;
    use serde::Serializer;
    use serde::de::{self, Deserialize, Deserializer};

    pub fn serialize<S: Serializer>(bytes: &[u8], s: S) -> Result<S::Ok, S::Error> {
        s.serialize_str(&STANDARD.encode(bytes))
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<Vec<u8>, D::Error> {
        let s = String::deserialize(d)?;
        STANDARD.decode(s.as_bytes()).map_err(de::Error::custom)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FileParams {
    /// Byte-copy from `source` (a host-path) into `path` (a target-path),
    /// atomically. Edits to `source` only propagate on the next apply. Use
    /// this for files whose contents are an artifact of the plan and whose
    /// bytes must live on the target - including dev/remote apply, where the
    /// operator's filesystem isn't reachable.
    Sourced {
        source: FilePath,
        /// Span of the `source` value in the plan source. Carried so
        /// host-path validation errors can point at the offending line.
        /// Skipped on the wire: validation runs pre-emit, so the span
        /// is unused downstream.
        #[serde(skip, default)]
        source_span: Span,
        path: FilePath,
        mode: Option<FileMode>,
        user: Option<FileUser>,
        group: Option<FileGroup>,
    },

    /// Materialise `path` as a symlink to `source` (a host-path on the
    /// machine running apply). Edits to `source` propagate immediately -
    /// nothing to re-apply - which is the dotfiles ergonomic. Symlinks have
    /// no meaningful `mode`/`user`/`group` of their own on Linux (chmod
    /// follows the link, lchmod doesn't exist), and we don't want
    /// chmod/chown silently mutating the operator's source file via the
    /// link, so the parser refuses those fields here.
    ///
    /// Note(cc): `source` arrives here as an *absolute* host-path - the
    /// `host-path` param-type coercion resolves relative strings against
    /// the plan's source dir before this point. The created symlink target
    /// is therefore absolute, so moving the source repo breaks every link.
    /// GNU stow defaults to relative for that reason; if relative-target
    /// becomes a use-case, add an opt-in `relative: true` field here and
    /// thread it through `FileChange::CreateSymlink` to the operation.
    Linked {
        source: FilePath,
        /// Span of the `source` value in the plan source. See
        /// [`FileParams::Sourced::source_span`] for rationale.
        #[serde(skip, default)]
        source_span: Span,
        path: FilePath,
    },

    Present {
        path: FilePath,
        mode: Option<FileMode>,
        user: Option<FileUser>,
        group: Option<FileGroup>,
    },
    Absent {
        path: FilePath,
    },
}

impl ParseParams for FileParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let state =
            fields.take_discriminator("state", &["sourced", "linked", "present", "absent"])?;
        let out = match state {
            "sourced" => {
                // `source` is a `host-path`; the parser resolves a relative
                // string against the plan's source dir (or accepts a typed
                // `Value::HostPath` from a plan that uses `host_path("./...")`).
                // Either way we lower to a `FilePath` string for the operation
                // layer; the original span is kept for downstream diagnostics.
                let (source_path, source_span) =
                    fields.required_host_path_spanned("source")?.take();
                FileParams::Sourced {
                    source: FilePath::new(source_path.to_string_lossy().into_owned()),
                    source_span,
                    path: FilePath::new(fields.required_target_path("path")?),
                    mode: fields.optional_u32("mode")?.map(FileMode::new),
                    user: fields.optional_string("user")?.map(FileUser::new),
                    group: fields.optional_string("group")?.map(FileGroup::new),
                }
            }
            "linked" => {
                // No `mode`/`user`/`group` here - see the variant docs. Any
                // such field will be left in `fields` and rejected by
                // `fields.finish()` below as an unknown key.
                let (source_path, source_span) =
                    fields.required_host_path_spanned("source")?.take();
                FileParams::Linked {
                    source: FilePath::new(source_path.to_string_lossy().into_owned()),
                    source_span,
                    path: FilePath::new(fields.required_target_path("path")?),
                }
            }
            "present" => FileParams::Present {
                path: FilePath::new(fields.required_target_path("path")?),
                mode: fields.optional_u32("mode")?.map(FileMode::new),
                user: fields.optional_string("user")?.map(FileUser::new),
                group: fields.optional_string("group")?.map(FileGroup::new),
            },
            "absent" => FileParams::Absent {
                path: FilePath::new(fields.required_target_path("path")?),
            },
            _ => unreachable!(),
        };
        fields.finish()?;
        Ok(out)
    }
}

impl Display for FileParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileParams::Sourced { source, path, .. } => {
                write!(f, "File::Sourced(source = {source}, path = {path})")
            }
            FileParams::Linked { source, path, .. } => {
                write!(f, "File::Linked(source = {source}, path = {path})")
            }
            FileParams::Present { path, .. } => write!(f, "File::Present(path = {path})"),
            FileParams::Absent { path } => write!(f, "File::Absent(path = {path})"),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FileResource {
    Sourced {
        source: FilePath,
        path: FilePath,
        /// True when `source` lies under the project's secrets directory.
        /// Set by [`ResourceParams::resources`](crate::ResourceParams::resources)
        /// using its `secrets_dir` argument; downstream `state`/`change`
        /// use it to decide whether file content is shipped verbatim or as
        /// [`Content::Redacted`].
        is_secret: bool,
    },
    Linked {
        source: FilePath,
        path: FilePath,
    },
    /// Contents sourced from a decrypted secret by name; resolved against
    /// [`Context::secrets`] at state/apply time so plaintext never travels
    /// through the resource/change tree. See `@resource/secret`.
    Secret {
        name: String,
        path: FilePath,
    },
    Present {
        path: FilePath,
    },
    Absent {
        path: FilePath,
    },
    Mode {
        path: FilePath,
        mode: FileMode,
    },
    User {
        path: FilePath,
        user: FileUser,
    },
    Group {
        path: FilePath,
        group: FileGroup,
    },
}

impl Display for FileResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileResource::Sourced { source, path, .. } => {
                write!(f, "FileSourced({source} -> {path})")
            }
            FileResource::Linked { source, path } => {
                write!(f, "FileLinked({source} -> {path})")
            }
            FileResource::Secret { name, path } => {
                write!(f, "FileSecret(secret = {name} -> {path})")
            }
            FileResource::Present { path } => write!(f, "FilePresent({path})"),
            FileResource::Absent { path } => write!(f, "FileAbsent({path})"),
            FileResource::Mode { path, mode } => write!(f, "FileMode({path}, mode = {mode})"),
            FileResource::User { path, user } => write!(f, "FileUser({path}, user = {user})"),
            FileResource::Group { path, group } => write!(f, "FileGroup({path}, group = {group})"),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FileState {
    /// File at `path` matches its declared `source`/`secret` byte-for-byte.
    /// `content` is the matching bytes (or redacted equivalent for secrets).
    Sourced {
        content: Content,
    },

    /// File at `path` differs from (or is absent versus) its declared
    /// `source`/`secret`. `current` is `None` when the target doesn't
    /// exist yet; `desired` is the source bytes (or redacted equivalent).
    NotSourced {
        current: Option<Content>,
        desired: Content,
    },

    Linked,
    NotLinked,
    Present,
    Absent,
    ModeCorrect,
    ModeIncorrect,
    UserCorrect,
    UserIncorrect,
    GroupCorrect,
    GroupIncorrect,
}

impl Display for FileState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use FileState::*;
        match self {
            Sourced { content } => write!(f, "Sourced({content})"),
            NotSourced { current, desired } => match current {
                Some(current) => write!(f, "NotSourced(current = {current}, desired = {desired})"),
                None => write!(f, "NotSourced(absent, desired = {desired})"),
            },
            Linked => write!(f, "Linked"),
            NotLinked => write!(f, "NotLinked"),
            Present => write!(f, "Present"),
            Absent => write!(f, "Absent"),
            ModeCorrect => write!(f, "ModeCorrect"),
            ModeIncorrect => write!(f, "ModeIncorrect"),
            UserCorrect => write!(f, "UserCorrect"),
            UserIncorrect => write!(f, "UserIncorrect"),
            GroupCorrect => write!(f, "GroupCorrect"),
            GroupIncorrect => write!(f, "GroupIncorrect"),
        }
    }
}

#[derive(Error, Debug)]
pub enum FileStateError {
    #[error(transparent)]
    Fs(#[from] FsError),

    /// Fires at state probe time when diffing on-disk contents against a
    /// declared secret. Apply-side twin:
    /// [`FileApplyError::MissingSecret`](lusid_operation::operations::file::FileApplyError::MissingSecret).
    #[error(
        "secret {name:?} referenced by file resource was not found in decrypted secrets bundle"
    )]
    MissingSecret { name: String },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FileChange {
    /// Apply-time write of `source`'s bytes to `path`. `before` and `after`
    /// are present-time snapshots (current target bytes, desired source
    /// bytes) carried for diff display; the operation still resolves
    /// `source` at apply time and is the authoritative byte sink.
    Write {
        path: FilePath,
        source: FileSource,
        before: Option<Content>,
        after: Content,
    },
    CreateSymlink {
        source: FilePath,
        path: FilePath,
    },
    Remove {
        path: FilePath,
    },
    ChangeMode {
        path: FilePath,
        mode: FileMode,
    },
    ChangeOwner {
        path: FilePath,
        user: Option<FileUser>,
        group: Option<FileGroup>,
    },
}

impl Display for FileChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileChange::Write { path, source, .. } => match source {
                FileSource::Contents(contents) => write!(
                    f,
                    "File::Write(path = {}, source = Contents({} bytes))",
                    path,
                    contents.len()
                ),
                FileSource::Path(source_path) => write!(
                    f,
                    "File::Write(path = {}, source = Path({}))",
                    path, source_path
                ),
                FileSource::Secret(name) => {
                    write!(f, "File::Write(path = {}, source = Secret({}))", path, name)
                }
            },
            FileChange::CreateSymlink { source, path } => {
                write!(f, "File::CreateSymlink(source = {source}, path = {path})")
            }
            FileChange::Remove { path } => write!(f, "File::Remove(path = {path})"),
            FileChange::ChangeMode { path, mode } => {
                write!(f, "File::ChangeMode(path = {path}, mode = {mode})")
            }
            FileChange::ChangeOwner { path, user, group } => write!(
                f,
                "File::ChangeOwner(path = {path}, user = {user:?}, group = {group:?})"
            ),
        }
    }
}

impl ResourceChangeTrait for FileChange {
    fn kind(&self) -> ChangeKind {
        match self {
            // `before: None` means the target did not previously have these
            // bytes (no file or non-regular file), so the write introduces
            // new state. `Some` means we are overwriting existing bytes.
            FileChange::Write { before: None, .. } => ChangeKind::Added,
            FileChange::Write {
                before: Some(_), ..
            } => ChangeKind::Modified,
            FileChange::CreateSymlink { .. } => ChangeKind::Added,
            FileChange::Remove { .. } => ChangeKind::Removed,
            FileChange::ChangeMode { .. } | FileChange::ChangeOwner { .. } => ChangeKind::Modified,
        }
    }
}

#[derive(Debug, Clone)]
pub struct File;

#[async_trait]
impl ResourceType for File {
    const ID: &'static str = "file";

    type Params = FileParams;
    type Resource = FileResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        // Mode/User/Group sub-atoms are common to `Sourced` and `Present`
        // (Linked rejects them at parse time, so it never reaches here).
        fn permission_atoms(
            path: &FilePath,
            mode: Option<FileMode>,
            user: Option<FileUser>,
            group: Option<FileGroup>,
        ) -> Vec<CausalityTree<FileResource>> {
            let mut nodes = Vec::new();
            if let Some(mode) = mode {
                nodes.push(CausalityTree::leaf(
                    CausalityMeta::requires(vec!["file".into()]),
                    FileResource::Mode {
                        path: path.clone(),
                        mode,
                    },
                ));
            }
            if let Some(user) = user {
                nodes.push(CausalityTree::leaf(
                    CausalityMeta::requires(vec!["file".into()]),
                    FileResource::User {
                        path: path.clone(),
                        user,
                    },
                ));
            }
            if let Some(group) = group {
                nodes.push(CausalityTree::leaf(
                    CausalityMeta::requires(vec!["file".into()]),
                    FileResource::Group {
                        path: path.clone(),
                        group,
                    },
                ));
            }
            nodes
        }

        match params {
            FileParams::Sourced {
                source,
                source_span: _,
                path,
                mode,
                user,
                group,
            } => {
                let mut nodes = vec![CausalityTree::leaf(
                    CausalityMeta::id("file".into()),
                    FileResource::Sourced {
                        source,
                        path: path.clone(),
                        // Real value set by `ResourceParams::resources`,
                        // which knows the secrets dir; leaving false here
                        // keeps `File` itself secrets-agnostic.
                        is_secret: false,
                    },
                )];
                nodes.extend(permission_atoms(&path, mode, user, group));
                nodes
            }

            FileParams::Linked {
                source,
                source_span: _,
                path,
            } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                FileResource::Linked { source, path },
            )],

            FileParams::Present {
                path,
                mode,
                user,
                group,
            } => {
                let mut nodes = vec![CausalityTree::leaf(
                    CausalityMeta::id("file".into()),
                    FileResource::Present { path: path.clone() },
                )];
                nodes.extend(permission_atoms(&path, mode, user, group));
                nodes
            }

            FileParams::Absent { path } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                FileResource::Absent { path },
            )],
        }
    }

    type State = FileState;
    type StateError = FileStateError;

    async fn state(
        ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        let state = match resource {
            FileResource::Sourced {
                source,
                path,
                is_secret,
            } => {
                let source_bytes = fs::read_file_to_bytes(source.as_path()).await?;
                if !fs::path_exists(path.as_path()).await? {
                    FileState::NotSourced {
                        current: None,
                        desired: Content::from_bytes(source_bytes, *is_secret),
                    }
                } else {
                    let path_bytes = fs::read_file_to_bytes(path.as_path()).await?;
                    if path_bytes == source_bytes {
                        FileState::Sourced {
                            content: Content::from_bytes(path_bytes, *is_secret),
                        }
                    } else {
                        FileState::NotSourced {
                            current: Some(Content::from_bytes(path_bytes, *is_secret)),
                            desired: Content::from_bytes(source_bytes, *is_secret),
                        }
                    }
                }
            }

            FileResource::Linked { source, path } => probe_linked_state(source, path).await?,

            FileResource::Secret { name, path } => {
                // Compare the file's current contents against the
                // decrypted secret plaintext. A missing secret here
                // (e.g. typo in the plan's `name` field) surfaces as
                // `MissingSecret` rather than a silent NotSourced.
                let secret = ctx
                    .secrets()
                    .get(name)
                    .ok_or_else(|| FileStateError::MissingSecret { name: name.clone() })?;
                let secret_bytes = secret.expose_secret().as_bytes();
                let desired = Content::redacted(secret_bytes);
                if !fs::path_exists(path.as_path()).await? {
                    FileState::NotSourced {
                        current: None,
                        desired,
                    }
                } else {
                    let path_bytes = fs::read_file_to_bytes(path.as_path()).await?;
                    if path_bytes.as_slice() == secret_bytes {
                        FileState::Sourced {
                            content: Content::redacted(&path_bytes),
                        }
                    } else {
                        FileState::NotSourced {
                            current: Some(Content::redacted(&path_bytes)),
                            desired,
                        }
                    }
                }
            }

            FileResource::Present { path } | FileResource::Absent { path } => {
                if fs::path_exists(path.as_path()).await? {
                    FileState::Present
                } else {
                    FileState::Absent
                }
            }

            FileResource::Mode { path, mode } => {
                if !fs::path_exists(path.as_path()).await? {
                    FileState::ModeIncorrect
                } else {
                    let actual_mode = fs::get_mode(path.as_path()).await?;
                    let actual_mode = actual_mode & 0o7777;
                    if actual_mode == mode.as_u32() {
                        FileState::ModeCorrect
                    } else {
                        FileState::ModeIncorrect
                    }
                }
            }

            FileResource::User { path, user } => {
                if !fs::path_exists(path.as_path()).await? {
                    FileState::UserIncorrect
                } else {
                    let actual_user = fs::get_owner_user(path.as_path()).await?;
                    let actual_user = actual_user.map(|u| u.name.to_string());
                    if actual_user.as_deref() == Some(user.as_str()) {
                        FileState::UserCorrect
                    } else {
                        FileState::UserIncorrect
                    }
                }
            }

            FileResource::Group { path, group } => {
                if !fs::path_exists(path.as_path()).await? {
                    FileState::GroupIncorrect
                } else {
                    let actual_group = fs::get_owner_group(path.as_path()).await?;
                    let actual_group = actual_group.map(|g| g.name.to_string());
                    if actual_group.as_deref() == Some(group.as_str()) {
                        FileState::GroupCorrect
                    } else {
                        FileState::GroupIncorrect
                    }
                }
            }
        };

        Ok(state)
    }

    type Change = FileChange;

    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        match (resource, state) {
            (
                FileResource::Sourced { source, path, .. },
                FileState::NotSourced { current, desired },
            ) => Some(FileChange::Write {
                path: path.clone(),
                source: FileSource::Path(source.clone()),
                before: current.clone(),
                after: desired.clone(),
            }),

            (FileResource::Sourced { .. }, FileState::Sourced { .. }) => None,

            (FileResource::Linked { source, path }, FileState::NotLinked) => {
                Some(FileChange::CreateSymlink {
                    source: source.clone(),
                    path: path.clone(),
                })
            }

            (FileResource::Linked { .. }, FileState::Linked) => None,

            (FileResource::Secret { name, path }, FileState::NotSourced { current, desired }) => {
                Some(FileChange::Write {
                    path: path.clone(),
                    source: FileSource::Secret(name.clone()),
                    before: current.clone(),
                    after: desired.clone(),
                })
            }

            (FileResource::Secret { .. }, FileState::Sourced { .. }) => None,

            (FileResource::Present { path }, FileState::Absent) => Some(FileChange::Write {
                path: path.clone(),
                source: FileSource::Contents(Vec::new()),
                before: None,
                after: Content::Bytes(Vec::new()),
            }),

            (FileResource::Present { .. }, FileState::Present) => None,

            (FileResource::Absent { path }, FileState::Present) => {
                Some(FileChange::Remove { path: path.clone() })
            }

            (FileResource::Absent { .. }, FileState::Absent) => None,

            (FileResource::Mode { path, mode }, FileState::ModeIncorrect) => {
                Some(FileChange::ChangeMode {
                    path: path.clone(),
                    mode: *mode,
                })
            }

            (FileResource::Mode { .. }, FileState::ModeCorrect) => None,

            (FileResource::User { path, user }, FileState::UserIncorrect) => {
                Some(FileChange::ChangeOwner {
                    path: path.clone(),
                    user: Some(user.clone()),
                    group: None,
                })
            }

            (FileResource::User { .. }, FileState::UserCorrect) => None,

            (FileResource::Group { path, group }, FileState::GroupIncorrect) => {
                Some(FileChange::ChangeOwner {
                    path: path.clone(),
                    user: None,
                    group: Some(group.clone()),
                })
            }

            (FileResource::Group { .. }, FileState::GroupCorrect) => None,

            _ => {
                // TODO (mw): Return an error. Which means changing the trait's change method.
                // Or, alternatively, we have separate resources for each case, so there's no
                // possible mismatch.
                panic!("Unexpected case in change method for File resource.")
            }
        }
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        let op = match change {
            FileChange::Write {
                path,
                source,
                before: _,
                after: _,
            } => Operation::File(FileOperation::Write { path, source }),
            FileChange::CreateSymlink { source, path } => {
                Operation::File(FileOperation::CreateSymlink { source, path })
            }
            FileChange::Remove { path } => Operation::File(FileOperation::Remove { path }),
            FileChange::ChangeMode { path, mode } => {
                Operation::File(FileOperation::ChangeMode { path, mode })
            }
            FileChange::ChangeOwner { path, user, group } => {
                Operation::File(FileOperation::ChangeOwner { path, user, group })
            }
        };

        vec![CausalityTree::leaf(CausalityMeta::default(), op)]
    }
}

/// Probe `path` for whether it's a symlink with the desired `source` target.
///
/// Comparison is *lexical*: `target` is whatever `readlink(2)` returned,
/// compared as a `PathBuf` against the source path string. We deliberately
/// don't canonicalise - `source` arrives as the absolute resolved host-path
/// (see `params::ParamType::HostPath` coercion), and any pre-existing symlink
/// that `readlink`s to a different string - even one that resolves to the
/// same inode - should re-create. Otherwise the operator can never see drift
/// between a plan declaring `./foo` and an existing link with a different
/// declaration.
async fn probe_linked_state(
    source: &FilePath,
    path: &FilePath,
) -> Result<FileState, FileStateError> {
    match fs::probe_symlink(path.as_path()).await? {
        fs::SymlinkTarget::Symlink(target) if target == source.as_path() => Ok(FileState::Linked),
        // Wrong-target symlink, regular file, or missing path - all mean
        // "(re)create the symlink".
        _ => Ok(FileState::NotLinked),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    fn file_path(p: &std::path::Path) -> FilePath {
        FilePath::new(p.to_string_lossy().into_owned())
    }

    // --- Sourced state probe (byte-equality) ----------------------------

    #[tokio::test]
    async fn sourced_byte_equal_reports_sourced() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src.txt");
        let target = dir.path().join("dest.txt");
        tokio::fs::write(&source, b"hello").await.unwrap();
        tokio::fs::write(&target, b"hello").await.unwrap();

        let resource = FileResource::Sourced {
            source: file_path(&source),
            path: file_path(&target),
            is_secret: false,
        };
        let mut ctx = lusid_ctx::Context::create(dir.path()).unwrap();
        let state = File::state(&mut ctx, &resource).await.unwrap();
        match state {
            FileState::Sourced {
                content: Content::Bytes(bytes),
            } => assert_eq!(bytes, b"hello"),
            other => panic!("expected Sourced(Bytes), got {other:?}"),
        }
    }

    #[tokio::test]
    async fn sourced_byte_diff_reports_not_sourced() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src.txt");
        let target = dir.path().join("dest.txt");
        tokio::fs::write(&source, b"new").await.unwrap();
        tokio::fs::write(&target, b"old").await.unwrap();

        let resource = FileResource::Sourced {
            source: file_path(&source),
            path: file_path(&target),
            is_secret: false,
        };
        let mut ctx = lusid_ctx::Context::create(dir.path()).unwrap();
        let state = File::state(&mut ctx, &resource).await.unwrap();
        match state {
            FileState::NotSourced {
                current: Some(Content::Bytes(c)),
                desired: Content::Bytes(d),
            } => {
                assert_eq!(c, b"old");
                assert_eq!(d, b"new");
            }
            other => panic!("expected NotSourced(Bytes, Bytes), got {other:?}"),
        }
    }

    #[tokio::test]
    async fn sourced_missing_path_reports_not_sourced() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src.txt");
        tokio::fs::write(&source, b"x").await.unwrap();
        let target = dir.path().join("dest.txt");

        let resource = FileResource::Sourced {
            source: file_path(&source),
            path: file_path(&target),
            is_secret: false,
        };
        let mut ctx = lusid_ctx::Context::create(dir.path()).unwrap();
        let state = File::state(&mut ctx, &resource).await.unwrap();
        match state {
            FileState::NotSourced {
                current: None,
                desired: Content::Bytes(d),
            } => assert_eq!(d, b"x"),
            other => panic!("expected NotSourced(None, Bytes), got {other:?}"),
        }
    }

    #[tokio::test]
    async fn sourced_with_is_secret_redacts_content() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src.txt");
        let target = dir.path().join("dest.txt");
        tokio::fs::write(&source, b"super-secret").await.unwrap();

        let resource = FileResource::Sourced {
            source: file_path(&source),
            path: file_path(&target),
            is_secret: true,
        };
        let mut ctx = lusid_ctx::Context::create(dir.path()).unwrap();
        let state = File::state(&mut ctx, &resource).await.unwrap();
        match state {
            FileState::NotSourced {
                current: None,
                desired: Content::Redacted { len, sha256 },
            } => {
                assert_eq!(len, "super-secret".len());
                assert_eq!(sha256.len(), 64);
            }
            other => panic!("expected NotSourced with Redacted desired, got {other:?}"),
        }
    }

    // --- Linked state probe (lexical-symlink-target) --------------------

    #[tokio::test]
    async fn linked_correct_symlink_reports_linked() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src.txt");
        tokio::fs::write(&source, b"x").await.unwrap();
        let target = dir.path().join("link.txt");
        tokio::fs::symlink(&source, &target).await.unwrap();

        let state = probe_linked_state(&file_path(&source), &file_path(&target))
            .await
            .unwrap();
        assert!(matches!(state, FileState::Linked));
    }

    #[tokio::test]
    async fn linked_regular_file_reports_not_linked() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src.txt");
        tokio::fs::write(&source, b"shared").await.unwrap();
        let target = dir.path().join("regular.txt");
        tokio::fs::write(&target, b"shared").await.unwrap();

        let state = probe_linked_state(&file_path(&source), &file_path(&target))
            .await
            .unwrap();
        assert!(matches!(state, FileState::NotLinked));
    }

    #[tokio::test]
    async fn linked_wrong_symlink_target_reports_not_linked() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src.txt");
        let other = dir.path().join("other.txt");
        tokio::fs::write(&source, b"x").await.unwrap();
        tokio::fs::write(&other, b"y").await.unwrap();
        let target = dir.path().join("link.txt");
        tokio::fs::symlink(&other, &target).await.unwrap();

        let state = probe_linked_state(&file_path(&source), &file_path(&target))
            .await
            .unwrap();
        assert!(matches!(state, FileState::NotLinked));
    }

    #[tokio::test]
    async fn linked_missing_path_reports_not_linked() {
        let dir = tempdir().unwrap();
        let source = dir.path().join("src.txt");
        tokio::fs::write(&source, b"x").await.unwrap();
        let target = dir.path().join("link.txt");

        let state = probe_linked_state(&file_path(&source), &file_path(&target))
            .await
            .unwrap();
        assert!(matches!(state, FileState::NotLinked));
    }

    // --- Change-emission table -----------------------------------------

    #[test]
    fn change_for_sourced_not_sourced_writes_path_source() {
        let resource = FileResource::Sourced {
            source: FilePath::new("/host/src.txt"),
            path: FilePath::new("/target/dest.txt"),
            is_secret: false,
        };
        let change = File::change(
            &resource,
            &FileState::NotSourced {
                current: None,
                desired: Content::Bytes(b"hello".to_vec()),
            },
        )
        .expect("some change");
        match change {
            FileChange::Write {
                path,
                source: FileSource::Path(s),
                before,
                after,
            } => {
                assert_eq!(path.as_path(), std::path::Path::new("/target/dest.txt"));
                assert_eq!(s.as_path(), std::path::Path::new("/host/src.txt"));
                assert!(before.is_none());
                assert!(matches!(after, Content::Bytes(b) if b == b"hello"));
            }
            other => panic!("expected Write{{Path}}, got {other:?}"),
        }
    }

    #[test]
    fn change_for_linked_not_linked_emits_create_symlink() {
        let resource = FileResource::Linked {
            source: FilePath::new("/host/src.txt"),
            path: FilePath::new("/target/dest.txt"),
        };
        let change = File::change(&resource, &FileState::NotLinked).expect("some change");
        match change {
            FileChange::CreateSymlink { source, path } => {
                assert_eq!(source.as_path(), std::path::Path::new("/host/src.txt"));
                assert_eq!(path.as_path(), std::path::Path::new("/target/dest.txt"));
            }
            other => panic!("expected CreateSymlink, got {other:?}"),
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
        // `source_span` is `#[serde(skip, default)]`; it does not survive
        // the wire and the round-tripped value contains a default Span.
        round_trip(&FileParams::Sourced {
            source: FilePath::new("/host/src.txt"),
            source_span: Span::default(),
            path: FilePath::new("/target/dest.txt"),
            mode: Some(FileMode::new(0o644)),
            user: Some(FileUser::new("root")),
            group: Some(FileGroup::new("wheel")),
        });
        round_trip(&FileParams::Linked {
            source: FilePath::new("/host/src.txt"),
            source_span: Span::default(),
            path: FilePath::new("/target/dest.txt"),
        });
        round_trip(&FileParams::Present {
            path: FilePath::new("/target/dest.txt"),
            mode: None,
            user: None,
            group: None,
        });
        round_trip(&FileParams::Absent {
            path: FilePath::new("/target/dest.txt"),
        });
    }

    #[test]
    fn resource_round_trip_covers_every_variant() {
        let path = FilePath::new("/target/dest.txt");
        round_trip(&FileResource::Sourced {
            source: FilePath::new("/host/src.txt"),
            path: path.clone(),
            is_secret: false,
        });
        round_trip(&FileResource::Sourced {
            source: FilePath::new("/root/secrets/api.txt"),
            path: path.clone(),
            is_secret: true,
        });
        round_trip(&FileResource::Linked {
            source: FilePath::new("/host/src.txt"),
            path: path.clone(),
        });
        round_trip(&FileResource::Secret {
            name: "api-key".into(),
            path: path.clone(),
        });
        round_trip(&FileResource::Present { path: path.clone() });
        round_trip(&FileResource::Absent { path: path.clone() });
        round_trip(&FileResource::Mode {
            path: path.clone(),
            mode: FileMode::new(0o600),
        });
        round_trip(&FileResource::User {
            path: path.clone(),
            user: FileUser::new("root"),
        });
        round_trip(&FileResource::Group {
            path,
            group: FileGroup::new("wheel"),
        });
    }

    #[test]
    fn state_round_trip_covers_every_variant() {
        for state in [
            FileState::Sourced {
                content: Content::Bytes(b"hi".to_vec()),
            },
            FileState::Sourced {
                content: Content::redacted(b"secret"),
            },
            FileState::NotSourced {
                current: None,
                desired: Content::Bytes(b"new".to_vec()),
            },
            FileState::NotSourced {
                current: Some(Content::Bytes(b"old".to_vec())),
                desired: Content::Bytes(b"new".to_vec()),
            },
            FileState::NotSourced {
                current: Some(Content::redacted(b"old")),
                desired: Content::redacted(b"new"),
            },
            FileState::Linked,
            FileState::NotLinked,
            FileState::Present,
            FileState::Absent,
            FileState::ModeCorrect,
            FileState::ModeIncorrect,
            FileState::UserCorrect,
            FileState::UserIncorrect,
            FileState::GroupCorrect,
            FileState::GroupIncorrect,
        ] {
            round_trip(&state);
        }
    }

    #[test]
    fn change_round_trip_covers_every_variant() {
        let path = FilePath::new("/target/dest.txt");
        round_trip(&FileChange::Write {
            path: path.clone(),
            source: FileSource::Contents(b"hello\n".to_vec()),
            before: None,
            after: Content::Bytes(b"hello\n".to_vec()),
        });
        round_trip(&FileChange::Write {
            path: path.clone(),
            source: FileSource::Path(FilePath::new("/host/src.txt")),
            before: Some(Content::Bytes(b"old".to_vec())),
            after: Content::Bytes(b"new".to_vec()),
        });
        round_trip(&FileChange::Write {
            path: path.clone(),
            source: FileSource::Secret("api-key".into()),
            before: Some(Content::redacted(b"old-secret")),
            after: Content::redacted(b"new-secret"),
        });
        round_trip(&FileChange::CreateSymlink {
            source: FilePath::new("/host/src.txt"),
            path: path.clone(),
        });
        round_trip(&FileChange::Remove { path: path.clone() });
        round_trip(&FileChange::ChangeMode {
            path: path.clone(),
            mode: FileMode::new(0o600),
        });
        round_trip(&FileChange::ChangeOwner {
            path,
            user: Some(FileUser::new("root")),
            group: Some(FileGroup::new("wheel")),
        });
    }

    #[test]
    fn content_redacts_with_stable_hash() {
        let bytes = b"the password is hunter2";
        let redacted = Content::redacted(bytes);
        match redacted {
            Content::Redacted { len, sha256 } => {
                assert_eq!(len, bytes.len());
                assert_eq!(sha256.len(), 64);
                // Identical bytes hash identically.
                let again = Content::redacted(bytes);
                match again {
                    Content::Redacted { sha256: again, .. } => assert_eq!(again, sha256),
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn content_bytes_round_trip_through_base64() {
        let original = Content::Bytes(vec![0, 1, 2, 0xff, 0xfe]);
        let json = serde_json::to_string(&original).unwrap();
        // Encoded as base64 string, not a JSON array.
        assert!(json.contains('"'), "expected base64 string, got {json}");
        let back: Content = serde_json::from_str(&json).unwrap();
        assert_eq!(original, back);
    }

    #[test]
    fn wire_payload_redacts_plaintext_for_secret_state() {
        // FileState carrying a redacted Content must not include the
        // plaintext anywhere in its JSON representation - the whole point
        // of redaction is that operator terminal scrollback never sees it.
        let plaintext = "the-password-is-hunter2";
        let not_sourced = FileState::NotSourced {
            current: Some(Content::redacted(plaintext.as_bytes())),
            desired: Content::redacted(plaintext.as_bytes()),
        };
        let json = serde_json::to_string(&not_sourced).unwrap();
        assert!(
            !json.contains(plaintext),
            "plaintext leaked into NotSourced wire payload: {json}"
        );

        let sourced = FileState::Sourced {
            content: Content::redacted(plaintext.as_bytes()),
        };
        let json = serde_json::to_string(&sourced).unwrap();
        assert!(
            !json.contains(plaintext),
            "plaintext leaked into Sourced wire payload: {json}"
        );
    }

    #[test]
    fn wire_payload_redacts_plaintext_for_secret_change() {
        let plaintext = "topsecret-api-key-value";
        let change = FileChange::Write {
            path: FilePath::new("/etc/secret"),
            source: FileSource::Secret("api-key".into()),
            before: Some(Content::redacted(plaintext.as_bytes())),
            after: Content::redacted(plaintext.as_bytes()),
        };
        let json = serde_json::to_string(&change).unwrap();
        assert!(
            !json.contains(plaintext),
            "plaintext leaked into wire payload: {json}"
        );
    }

    #[test]
    fn is_secret_source_matches_prefix() {
        let secrets_dir = Path::new("/proj/secrets");
        assert!(is_secret_source(
            &FilePath::new("/proj/secrets/api.txt"),
            secrets_dir
        ));
        assert!(is_secret_source(
            &FilePath::new("/proj/secrets/nested/api.txt"),
            secrets_dir
        ));
        assert!(!is_secret_source(
            &FilePath::new("/proj/other/api.txt"),
            secrets_dir
        ));
        // Non-absolute secrets_dir short-circuits to false.
        assert!(!is_secret_source(
            &FilePath::new("/proj/secrets/api.txt"),
            Path::new("relative/secrets"),
        ));
    }
}
