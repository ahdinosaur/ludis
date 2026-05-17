use std::{fmt::Display, path::PathBuf};

use async_trait::async_trait;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_fs::{self as fs, FsError};
use lusid_operation::{
    Operation,
    operations::{file::FilePath, git::GitOperation},
};
use lusid_params::{ParseError, ParseParams, StructFields};
use rimu::{Spanned, Value};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::{ChangeKind, ResourceChangeTrait, ResourceType};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GitParams {
    pub repo: String,
    pub path: FilePath,
    pub version: Option<String>,
    pub update: Option<bool>,
    pub force: Option<bool>,
}

impl ParseParams for GitParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let repo = fields.required_string("repo")?;
        let path = FilePath::new(fields.required_target_path("path")?);
        let version = fields.optional_string("version")?;
        let update = fields.optional_bool("update")?;
        let force = fields.optional_bool("force")?;
        fields.finish()?;
        Ok(GitParams {
            repo,
            path,
            version,
            update,
            force,
        })
    }
}

impl Display for GitParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Git(repo = {}, path = {}, version = {:?}, update = {:?}, force = {:?})",
            self.repo, self.path, self.version, self.update, self.force
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GitResource {
    pub repo: String,
    pub path: FilePath,
    pub version: Option<String>,
    pub update: bool,
    pub force: bool,
}

impl Display for GitResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Git(repo = {}, path = {}, version = {:?}, update = {}, force = {})",
            self.repo, self.path, self.version, self.update, self.force
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GitState {
    Absent,
    Present {
        head: Option<String>,
        branch: Option<String>,
        is_dirty: bool,
    },
}

impl Display for GitState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GitState::Absent => write!(f, "Git::Absent"),
            GitState::Present {
                head,
                branch,
                is_dirty,
            } => {
                write!(
                    f,
                    "Git::Present(head = {:?}, branch = {:?}, is_dirty = {is_dirty})",
                    head, branch
                )
            }
        }
    }
}

#[derive(Error, Debug)]
pub enum GitStateError {
    #[error(transparent)]
    Command(#[from] CommandError),

    #[error(transparent)]
    Fs(#[from] FsError),

    #[error("path is not a git repo: {path}")]
    NotRepo { path: FilePath },

    #[error("git dir mismatch: expected {expected}, got {actual}")]
    GitDirMismatch { expected: String, actual: String },

    #[error("remote origin mismatch: expected {expected}, got {actual:?}")]
    RemoteMismatch {
        expected: String,
        actual: Option<String>,
    },

    #[error("working tree has uncommitted changes")]
    Dirty,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GitChange {
    Clone {
        repo: String,
        path: FilePath,
    },
    Checkout {
        path: FilePath,
        version: String,
        force: bool,
        fetch: bool,
    },
    Pull {
        path: FilePath,
    },
}

impl Display for GitChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GitChange::Clone { repo, path } => {
                write!(f, "Git::Clone(repo = {}, path = {})", repo, path)
            }
            GitChange::Checkout {
                path,
                version,
                force,
                fetch,
            } => write!(
                f,
                "Git::Checkout(path = {}, version = {}, force = {}, fetch = {})",
                path, version, force, fetch
            ),
            GitChange::Pull { path } => write!(f, "Git::Pull(path = {})", path),
        }
    }
}

impl ResourceChangeTrait for GitChange {
    fn kind(&self) -> ChangeKind {
        match self {
            GitChange::Clone { .. } => ChangeKind::Added,
            GitChange::Checkout { .. } | GitChange::Pull { .. } => ChangeKind::Modified,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Git;

#[async_trait]
impl ResourceType for Git {
    const ID: &'static str = "git";

    type Params = GitParams;
    type Resource = GitResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        vec![CausalityTree::leaf(
            CausalityMeta::default(),
            GitResource {
                repo: params.repo,
                path: params.path,
                version: params.version,
                update: params.update.unwrap_or(true),
                force: params.force.unwrap_or(false),
            },
        )]
    }

    type State = GitState;
    type StateError = GitStateError;

    async fn state(
        _ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        if !fs::path_exists(resource.path.as_path()).await? {
            return Ok(GitState::Absent);
        }

        // Check path is a git directory
        let git_dir = git_run(resource, ["rev-parse", "--git-dir"]).await?;
        let git_dir = String::from_utf8_lossy(&git_dir).trim().to_string();
        let expected_git_dir = resource.path.as_path().join(".git");
        let actual_git_dir = resolve_git_dir(resource.path.as_path(), &git_dir);
        if expected_git_dir != actual_git_dir {
            return Err(GitStateError::GitDirMismatch {
                expected: expected_git_dir.display().to_string(),
                actual: actual_git_dir.display().to_string(),
            });
        }

        // Check git remote of path matches the expected repo url
        let remote = match git_run(resource, ["config", "--get", "remote.origin.url"]).await {
            Ok(output) => Some(String::from_utf8_lossy(&output).trim().to_string()),
            Err(_) => None,
        };
        if remote.as_deref() != Some(resource.repo.as_str()) {
            return Err(GitStateError::RemoteMismatch {
                expected: resource.repo.clone(),
                actual: remote,
            });
        }

        // Get whether git repo at path is dirty.
        // Note(cc): a dirty working tree without `force` surfaces as a state error (not a
        // no-op): the intent is to refuse to touch user changes. `force: true` carries the
        // dirty flag forward into `change_for_present` where it gates pull/checkout.
        let status = git_run(resource, ["status", "--porcelain"]).await?;
        let is_dirty = !status.is_empty();
        if is_dirty && !resource.force {
            return Err(GitStateError::Dirty);
        }

        // Get head of git repo at path
        let head = git_run(resource, ["rev-parse", "HEAD"])
            .await
            .ok()
            .map(|s| String::from_utf8_lossy(&s).trim().to_string());
        // Get branch of git repo at path
        let branch = git_run(resource, ["symbolic-ref", "--quiet", "--short", "HEAD"])
            .await
            .ok()
            .map(|s| String::from_utf8_lossy(&s).trim().to_string());

        Ok(GitState::Present {
            head,
            branch,
            is_dirty,
        })
    }

    type Change = GitChange;

    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        match state {
            GitState::Absent => Some(GitChange::Clone {
                repo: resource.repo.clone(),
                path: resource.path.clone(),
            }),
            GitState::Present {
                head,
                branch,
                is_dirty,
            } => change_for_present(resource, head.as_deref(), branch.as_deref(), *is_dirty),
        }
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        match change {
            GitChange::Clone { repo, path } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                Operation::Git(GitOperation::Clone { repo, path }),
            )],
            GitChange::Checkout {
                path,
                version,
                force,
                fetch,
            } => {
                if fetch {
                    vec![
                        CausalityTree::leaf(
                            CausalityMeta::id("fetch".into()),
                            Operation::Git(GitOperation::Fetch { path: path.clone() }),
                        ),
                        CausalityTree::leaf(
                            CausalityMeta::requires(vec!["fetch".into()]),
                            Operation::Git(GitOperation::Checkout {
                                path,
                                version,
                                force,
                            }),
                        ),
                    ]
                } else {
                    vec![CausalityTree::leaf(
                        CausalityMeta::default(),
                        Operation::Git(GitOperation::Checkout {
                            path,
                            version,
                            force,
                        }),
                    )]
                }
            }
            GitChange::Pull { path } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                Operation::Git(GitOperation::Pull { path }),
            )],
        }
    }
}

fn change_for_present(
    resource: &GitResource,
    head: Option<&str>,
    branch: Option<&str>,
    is_dirty: bool,
) -> Option<GitChange> {
    if let Some(version) = resource.version.as_deref() {
        let matches = branch == Some(version) || head == Some(version);
        if matches {
            if !is_dirty && resource.update && branch == Some(version) {
                return Some(GitChange::Pull {
                    path: resource.path.clone(),
                });
            }
            return None;
        }

        return Some(GitChange::Checkout {
            path: resource.path.clone(),
            version: version.to_string(),
            force: resource.force,
            fetch: resource.update,
        });
    }

    if !is_dirty && resource.update && branch.is_some() {
        return Some(GitChange::Pull {
            path: resource.path.clone(),
        });
    }

    None
}

fn resolve_git_dir(base: &std::path::Path, git_dir: &str) -> PathBuf {
    let git_dir_path = PathBuf::from(git_dir);
    if git_dir_path.is_absolute() {
        git_dir_path
    } else {
        base.join(git_dir_path)
    }
}

async fn git_run(
    resource: &GitResource,
    args: impl IntoIterator<Item = &'static str>,
) -> Result<Vec<u8>, CommandError> {
    let mut cmd = Command::new("git");
    cmd.arg("-C").arg(resource.path.as_path()).args(args);
    cmd.run().await
}
