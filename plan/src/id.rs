//! Identifiers for plans and their internal nodes.
//!
//! - [`PlanId`] - where to find a plan (local path or, eventually, a git URL).
//! - [`PlanNodeId`] - how to name a specific node inside a planned tree for causality
//!   references (`requires` / `required_by`).

use lusid_store::StoreItemId;
use rimu::SourceId;
use serde::{Deserialize, Serialize};
use std::{
    fmt::Display,
    path::{Path, PathBuf},
};
use url::Url;

/// Location of a plan source. `PlanId::Git` is declared but not yet wired through the
/// store (see the `From<PlanId> for StoreItemId` impl below).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum PlanId {
    Path(PathBuf),
    Git(Url, PathBuf),
}

impl PlanId {
    /// Resolve a child plan reference against this plan's directory.
    ///
    /// `self` is treated as a file path; the child is joined against the file's parent
    /// directory. So joining `"foo/bar.lusid"` with `"baz.lusid"` yields `"foo/baz.lusid"`.
    pub fn join<P: AsRef<Path>>(&self, path: P) -> PlanId {
        match self {
            PlanId::Path(current_path) => PlanId::Path(relative(current_path, path)),
            PlanId::Git(url, current_path) => {
                PlanId::Git(url.clone(), relative(current_path, path))
            }
        }
    }

    /// The local filesystem path, if this plan is a local file. `None` for `Git`.
    pub fn as_path(self) -> Option<PathBuf> {
        match self {
            PlanId::Path(path) => Some(path),
            PlanId::Git(_, _) => None,
        }
    }
}

fn relative<P: AsRef<Path>>(current_path: &Path, next_path: P) -> PathBuf {
    current_path
        .parent()
        .unwrap_or(&PathBuf::default())
        .join(next_path)
}

impl Display for PlanId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PlanId::Path(path) => write!(f, "Path({})", path.display()),
            PlanId::Git(url, path) => write!(f, "Git({}, {})", url, path.display()),
        }
    }
}

impl From<PlanId> for StoreItemId {
    fn from(value: PlanId) -> Self {
        match value {
            PlanId::Path(path) => StoreItemId::LocalFile(path),
            // TODO(cc): wire `PlanId::Git` through the store. The rest of the pipeline
            // already accepts it (SourceId, diagnostics, etc.) - the missing piece is
            // a Git-aware `StoreItemId` variant.
            PlanId::Git(_url, _path) => todo!(),
        }
    }
}

impl From<PlanId> for SourceId {
    fn from(value: PlanId) -> Self {
        match value {
            PlanId::Path(path) => SourceId::from(path.to_string_lossy().to_string()),
            PlanId::Git(mut url, path) => {
                url.query_pairs_mut()
                    .append_pair("path", &path.to_string_lossy());
                SourceId::from(url.to_string())
            }
        }
    }
}

/// Identifier for any node in a planned tree.
///
/// - `Plan` - the root of a plan.
/// - `PlanItem` - a plan item declared with a user-authored `id`. Scoped by
///   `(scope_path, plan_id, item_id)`: `scope_path` is the chain of outer
///   invocation ids when this item lives inside a nested plan, empty at the
///   top level. Two invocations of the same subplan get different
///   `scope_path` values so their inner item ids don't collide.
/// - `SubItem` - an id minted *inside* a resource's expansion (e.g. the `"file"` id used
///   by `file` to order mode/user/group atoms). Scoped by a fresh `cuid2` so the
///   inner ids can never collide across resources.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum PlanNodeId {
    Plan(PlanId),
    PlanItem {
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        scope_path: Vec<String>,
        plan_id: PlanId,
        item_id: String,
    },
    SubItem {
        scope_id: String,
        item_id: String,
    },
}

impl Display for PlanNodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PlanNodeId::Plan(id) => write!(f, "Plan({id})"),
            PlanNodeId::PlanItem {
                scope_path,
                plan_id,
                item_id,
            } => {
                if scope_path.is_empty() {
                    write!(f, "PlanItem(plan = {plan_id}, item = {item_id})")
                } else {
                    write!(
                        f,
                        "PlanItem(scope = {scope}, plan = {plan_id}, item = {item_id})",
                        scope = scope_path.join("/"),
                    )
                }
            }
            PlanNodeId::SubItem { scope_id, item_id } => {
                write!(f, "SubItem(scope = {scope_id}, item = {item_id})")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    /// Items in two invocations of the same subplan are distinct ids when
    /// the outer invocations declared different `id`s. This is what stops
    /// `compute_epochs` from rejecting the dependency graph as a duplicate.
    #[test]
    fn scope_path_differentiates_subplan_invocations() {
        let plan_id = PlanId::Path(PathBuf::from("nginx.lusid"));
        let invocation_a = PlanNodeId::PlanItem {
            scope_path: vec!["nginx-grafana".to_string()],
            plan_id: plan_id.clone(),
            item_id: "package".to_string(),
        };
        let invocation_b = PlanNodeId::PlanItem {
            scope_path: vec!["nginx-prometheus".to_string()],
            plan_id,
            item_id: "package".to_string(),
        };
        assert_ne!(invocation_a, invocation_b);
    }

    /// Display omits the scope clause at the top level to keep the common
    /// case unchanged; scoped ids print the path joined by `/`.
    #[test]
    fn display_includes_scope_path_only_when_nested() {
        let plan_id = PlanId::Path(PathBuf::from("nginx.lusid"));
        let top_level = PlanNodeId::PlanItem {
            scope_path: Vec::new(),
            plan_id: plan_id.clone(),
            item_id: "package".to_string(),
        };
        let nested = PlanNodeId::PlanItem {
            scope_path: vec!["outer".to_string(), "inner".to_string()],
            plan_id,
            item_id: "package".to_string(),
        };
        assert_eq!(
            top_level.to_string(),
            "PlanItem(plan = Path(nginx.lusid), item = package)"
        );
        assert_eq!(
            nested.to_string(),
            "PlanItem(scope = outer/inner, plan = Path(nginx.lusid), item = package)"
        );
    }
}
