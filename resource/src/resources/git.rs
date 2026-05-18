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
use lusid_view::impl_display_render;
use rimu::{Spanned, Value};
use thiserror::Error;

use crate::ResourceType;

#[derive(Debug, Clone)]
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

impl_display_render!(GitParams);

#[derive(Debug, Clone)]
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

impl_display_render!(GitResource);

#[derive(Debug, Clone)]
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

impl_display_render!(GitState);

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

#[derive(Debug, Clone)]
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

impl_display_render!(GitChange);

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

        let status = git_run(resource, ["status", "--porcelain"]).await?;
        let is_dirty = !status.is_empty();

        let head = git_run(resource, ["rev-parse", "HEAD"])
            .await
            .ok()
            .map(|s| String::from_utf8_lossy(&s).trim().to_string());
        let branch = git_run(resource, ["symbolic-ref", "--quiet", "--short", "HEAD"])
            .await
            .ok()
            .map(|s| String::from_utf8_lossy(&s).trim().to_string());

        // Refuse a dirty working tree only when the resource would otherwise
        // mutate it (pull/checkout). A clone whose tree is dirtied by some
        // downstream resource but which lusid no longer intends to touch
        // (e.g. `update: false`, no `version`) stays a no-op so re-applies
        // remain idempotent. `force: true` defers the decision to the
        // underlying git op: `pull --ff-only` still refuses non-fast-forward
        // merges, but `checkout -f` will discard local changes - that is
        // what `force` opts into.
        if is_dirty && !resource.force {
            let intended = change_for_present(resource, head.as_deref(), branch.as_deref());
            if matches!(
                intended,
                Some(GitChange::Pull { .. } | GitChange::Checkout { .. })
            ) {
                return Err(GitStateError::Dirty);
            }
        }

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
            GitState::Present { head, branch, .. } => {
                change_for_present(resource, head.as_deref(), branch.as_deref())
            }
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
) -> Option<GitChange> {
    if let Some(version) = resource.version.as_deref() {
        let matches = branch == Some(version) || head == Some(version);
        if matches {
            if resource.update && branch == Some(version) {
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

    if resource.update && branch.is_some() {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;
    use tempfile::tempdir;

    fn resource(
        version: Option<&str>,
        update: bool,
        force: bool,
        path: &Path,
        repo: &str,
    ) -> GitResource {
        GitResource {
            repo: repo.to_string(),
            path: FilePath::new(path.to_string_lossy().into_owned()),
            version: version.map(str::to_string),
            update,
            force,
        }
    }

    fn dummy(version: Option<&str>, update: bool, force: bool) -> GitResource {
        // The `change_for_present` tests never touch the filesystem, so any
        // path-shaped placeholder works.
        resource(version, update, force, Path::new("/nonexistent"), "repo")
    }

    // --- change_for_present: no `version` -------------------------------

    #[test]
    fn no_version_update_false_is_noop() {
        let r = dummy(None, false, false);
        assert!(change_for_present(&r, Some("HEAD"), Some("main")).is_none());
    }

    #[test]
    fn no_version_update_true_on_branch_pulls() {
        let r = dummy(None, true, false);
        let change = change_for_present(&r, Some("HEAD"), Some("main"));
        assert!(matches!(change, Some(GitChange::Pull { .. })));
    }

    #[test]
    fn no_version_update_true_detached_head_is_noop() {
        // No branch to pull onto - nothing meaningful to do.
        let r = dummy(None, true, false);
        assert!(change_for_present(&r, Some("HEAD"), None).is_none());
    }

    // --- change_for_present: `version` matches --------------------------

    #[test]
    fn version_matches_branch_update_true_pulls() {
        let r = dummy(Some("main"), true, false);
        let change = change_for_present(&r, Some("HEAD"), Some("main"));
        assert!(matches!(change, Some(GitChange::Pull { .. })));
    }

    #[test]
    fn version_matches_branch_update_false_is_noop() {
        let r = dummy(Some("main"), false, false);
        assert!(change_for_present(&r, Some("HEAD"), Some("main")).is_none());
    }

    #[test]
    fn version_matches_head_only_is_noop() {
        // Pinned to a sha (or tag) that happens to be HEAD: no branch to pull,
        // no checkout needed.
        let r = dummy(Some("deadbeef"), true, false);
        assert!(change_for_present(&r, Some("deadbeef"), Some("main")).is_none());
    }

    // --- change_for_present: `version` mismatches -----------------------

    #[test]
    fn version_mismatch_checks_out_with_resource_flags() {
        let r = dummy(Some("v1"), true, true);
        let change = change_for_present(&r, Some("HEAD"), Some("main")).expect("checkout");
        match change {
            GitChange::Checkout {
                version,
                force,
                fetch,
                ..
            } => {
                assert_eq!(version, "v1");
                assert!(force, "force flag should follow resource.force");
                assert!(fetch, "fetch flag should follow resource.update");
            }
            other => panic!("expected Checkout, got {other:?}"),
        }
    }

    #[test]
    fn version_mismatch_no_update_no_force_checks_out_without_fetch() {
        let r = dummy(Some("v1"), false, false);
        let change = change_for_present(&r, Some("HEAD"), Some("main")).expect("checkout");
        match change {
            GitChange::Checkout { force, fetch, .. } => {
                assert!(!force);
                assert!(!fetch);
            }
            other => panic!("expected Checkout, got {other:?}"),
        }
    }

    // --- state(): dirty refusal is conditional on intent ----------------
    //
    // These tests drive a real `git` against a tempdir. The `run_git`
    // helper isolates the spawned process from the contributor's global
    // git config (e.g. `commit.gpgsign`, `core.hooksPath`) by pointing
    // `GIT_CONFIG_GLOBAL` and `GIT_CONFIG_SYSTEM` at `/dev/null`.

    async fn run_git(dir: &Path, args: &[&str]) {
        let status = tokio::process::Command::new("git")
            .env("GIT_CONFIG_GLOBAL", "/dev/null")
            .env("GIT_CONFIG_SYSTEM", "/dev/null")
            .arg("-C")
            .arg(dir)
            .args(args)
            .status()
            .await
            .expect("spawn git");
        assert!(status.success(), "git {args:?} failed in {dir:?}");
    }

    /// Initialise a git repo at `path` with an initial commit on `main` and
    /// `remote.origin.url` set to `repo_url` so the remote-mismatch check
    /// in `state()` passes. No network access.
    async fn init_repo(path: &Path, repo_url: &str) {
        run_git(path, &["init", "--quiet", "--initial-branch=main", "."]).await;
        run_git(path, &["config", "user.email", "test@example.invalid"]).await;
        run_git(path, &["config", "user.name", "test"]).await;
        run_git(path, &["commit", "--quiet", "--allow-empty", "-m", "init"]).await;
        run_git(path, &["remote", "add", "origin", repo_url]).await;
    }

    const TEST_REPO_URL: &str = "https://example.invalid/repo.git";

    #[tokio::test]
    async fn dirty_tree_with_noop_intent_does_not_error() {
        // Regression: `update: false`, no `version`. A dirty working tree
        // left by some downstream resource must not block re-applies when
        // lusid has no intent to mutate the tree.
        let dir = tempdir().unwrap();
        init_repo(dir.path(), TEST_REPO_URL).await;
        tokio::fs::write(dir.path().join("artifact.bin"), b"x")
            .await
            .unwrap();

        let r = resource(None, false, false, dir.path(), TEST_REPO_URL);
        let mut ctx = lusid_ctx::Context::create(dir.path()).expect("ctx");
        let state = Git::state(&mut ctx, &r).await.expect("state probe");
        assert!(matches!(state, GitState::Present { is_dirty: true, .. }));
    }

    #[tokio::test]
    async fn dirty_tree_with_pull_intent_errors() {
        // `update: true`, on the named branch, dirty, no `force`: refuse to
        // clobber user changes with a pull.
        let dir = tempdir().unwrap();
        init_repo(dir.path(), TEST_REPO_URL).await;
        tokio::fs::write(dir.path().join("artifact.bin"), b"x")
            .await
            .unwrap();

        let r = resource(None, true, false, dir.path(), TEST_REPO_URL);
        let mut ctx = lusid_ctx::Context::create(dir.path()).expect("ctx");
        let err = Git::state(&mut ctx, &r)
            .await
            .expect_err("dirty tree with pull intent should error");
        assert!(matches!(err, GitStateError::Dirty));
    }

    #[tokio::test]
    async fn dirty_tree_with_checkout_intent_errors() {
        // `version` mismatches the current branch, dirty, no `force`:
        // refuse to clobber user changes with a checkout.
        let dir = tempdir().unwrap();
        init_repo(dir.path(), TEST_REPO_URL).await;
        tokio::fs::write(dir.path().join("artifact.bin"), b"x")
            .await
            .unwrap();

        let r = resource(Some("v1"), false, false, dir.path(), TEST_REPO_URL);
        let mut ctx = lusid_ctx::Context::create(dir.path()).expect("ctx");
        let err = Git::state(&mut ctx, &r)
            .await
            .expect_err("dirty tree with checkout intent should error");
        assert!(matches!(err, GitStateError::Dirty));
    }

    #[tokio::test]
    async fn dirty_tree_with_force_passes_through() {
        // `force: true`: the operator opts back into mutating a dirty tree;
        // state() must not refuse, leaving the call to the downstream op.
        let dir = tempdir().unwrap();
        init_repo(dir.path(), TEST_REPO_URL).await;
        tokio::fs::write(dir.path().join("artifact.bin"), b"x")
            .await
            .unwrap();

        let r = resource(None, true, true, dir.path(), TEST_REPO_URL);
        let mut ctx = lusid_ctx::Context::create(dir.path()).expect("ctx");
        let state = Git::state(&mut ctx, &r).await.expect("state probe");
        assert!(matches!(state, GitState::Present { is_dirty: true, .. }));

        // And `change()` produces a Pull - the dirty gate no longer
        // silently swallows the work; the downstream `pull --ff-only`
        // either succeeds or fails loudly.
        let change = Git::change(&r, &state);
        assert!(matches!(change, Some(GitChange::Pull { .. })));
    }
}
