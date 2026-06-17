//! Local plan discovery for `remote apply` and `dev apply`.
//!
//! Runs the planner on the operator with a synthesised [`System`] to learn
//! exactly which files the plan touches - plan sources (recorded by the
//! [`Store`] read tracker) and operator-side host-path sources read off the
//! resulting [`PlanTree`]. The caller mirrors those paths onto the target
//! instead of shipping the whole plan directory.
//!
//! The synthesised [`System`] is "real enough" for planning but won't always
//! match what [`System::get`] would produce on the target (user-private-group
//! distros are the assumed default; override per-machine in `lusid.toml` via
//! the `[machines.<id>.user]` block when that's wrong). Plans that branch on
//! `system.user.primary_group` or other fields that differ between the
//! synthetic and real systems will produce a different upload set than the
//! target's re-plan and may fail at apply with missing-file errors.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use displaydoc::Display;
use lusid_machine::Machine;
use lusid_plan::PlanTree;
use lusid_resource::ResourceParams;
use lusid_resource::directory::DirectoryParams;
use lusid_resource::file::FileParams;
use lusid_store::StoreItemId;
use lusid_system::{System, User};
use lusid_tree::Tree;
use path_clean::PathClean;
use rimu::Span;
use thiserror::Error;

#[derive(Debug, Error, Display)]
pub enum UploadSetError {
    /// host-path source `{path}` is outside the project root `{root}` and cannot be mirrored. Move the source under the project root or restructure the plan to reference it from there.
    PathOutsideRoot {
        path: PathBuf,
        root: PathBuf,
        span: Option<Span>,
    },

    /// path `{path}` could not be made absolute against project root `{root}`; planner produced an unexpected shape
    PathNotAbsolute {
        path: PathBuf,
        root: PathBuf,
        span: Option<Span>,
    },
}

/// Resolve `path` to a project-relative form: absolutise against `root` if
/// needed, lexically clean (`..` and `.`), and `strip_prefix(root)`. Errors if
/// the result escapes `root` (e.g. via leading `..`s) or somehow remains
/// non-absolute. Shared between [`build_manifest`] and callers that need the
/// same relativisation for plan paths handed back to the apply command line.
pub fn relativize(root: &Path, path: &Path) -> Result<PathBuf, UploadSetError> {
    let root = make_absolute(root).clean();
    rebase(&root, path, None)
}

/// Build the synthetic [`System`] the operator uses to plan offline for a
/// target. Convention defaults match Debian/Arch user-private-group setups
/// (`home = /home/<name>`, `primary_group = <name>`, `/root` for root). Per-
/// machine `[machines.<id>.user]` overrides take precedence field-by-field.
pub fn synthesize_system(machine: &Machine, default_user: &str) -> System {
    let override_user = machine.user.as_ref();
    let name = override_user
        .and_then(|u| u.name.clone())
        .unwrap_or_else(|| default_user.to_string());
    let home = override_user
        .and_then(|u| u.home.clone())
        .unwrap_or_else(|| {
            if name == "root" {
                PathBuf::from("/root")
            } else {
                PathBuf::from(format!("/home/{name}"))
            }
        });
    let primary_group = override_user
        .and_then(|u| u.primary_group.clone())
        .unwrap_or_else(|| name.clone());

    System {
        hostname: machine.hostname.clone(),
        arch: machine.arch,
        os: machine.os.clone(),
        user: User {
            name,
            home,
            primary_group,
        },
    }
}

/// Walk a planned tree and collect every operator-side host-path source with
/// its span (for diagnostics).
///
/// Today only `@resource/file` and `@resource/directory` carry an operator-
/// side `source`. Note(cc): if a new `ResourceParams` variant grows an
/// operator-side path field, extend the match in `host_source` so the file
/// ships with remote/dev apply. `PlanMeta::handlers` holds operations
/// (`@operation/command` etc.) which do not currently reference operator-side
/// bytes - intentionally not walked.
pub fn collect_host_paths(tree: &PlanTree<ResourceParams>) -> Vec<(PathBuf, Span)> {
    let mut out = Vec::new();
    walk(tree, &mut out);
    out
}

fn walk(tree: &PlanTree<ResourceParams>, out: &mut Vec<(PathBuf, Span)>) {
    match tree {
        Tree::Branch { children, .. } => {
            for child in children {
                walk(child, out);
            }
        }
        Tree::Leaf { node, .. } => {
            if let Some(entry) = host_source(node) {
                out.push(entry);
            }
        }
    }
}

fn host_source(params: &ResourceParams) -> Option<(PathBuf, Span)> {
    match params {
        ResourceParams::File(FileParams::Sourced {
            source,
            source_span,
            ..
        })
        | ResourceParams::File(FileParams::Linked {
            source,
            source_span,
            ..
        })
        | ResourceParams::Directory(DirectoryParams::Sourced {
            source,
            source_span,
            ..
        })
        | ResourceParams::Directory(DirectoryParams::Linked {
            source,
            source_span,
            ..
        }) => Some((source.as_path().to_path_buf(), source_span.clone())),
        _ => None,
    }
}

/// Build the upload manifest: a deduped, sorted set of paths relative to
/// `root`. Both plan sources (from [`lusid_store::Store::reads`]) and host-
/// path sources are absolutised against `root`, lexically cleaned (resolving
/// `..` segments so the same file referenced via different traversals
/// dedupes), and rebased to relative.
///
/// The returned [`BTreeSet`]'s lexicographic order is load-bearing for
/// callers: parent directories sort before their children, so iterating in
/// natural order uploads `DirPath` mirrors before any sibling `FilePath`
/// upload could land inside the same subtree.
pub fn build_manifest(
    root: &Path,
    plan_reads: &[StoreItemId],
    host_paths: &[(PathBuf, Span)],
) -> Result<BTreeSet<PathBuf>, UploadSetError> {
    let root = make_absolute(root).clean();
    let mut out = BTreeSet::new();
    for id in plan_reads {
        let StoreItemId::LocalFile(path) = id;
        out.insert(rebase(&root, path, None)?);
    }
    for (path, span) in host_paths {
        out.insert(rebase(&root, path, Some(span.clone()))?);
    }
    Ok(out)
}

fn rebase(root: &Path, path: &Path, span: Option<Span>) -> Result<PathBuf, UploadSetError> {
    let absolutised = if path.is_absolute() {
        path.to_path_buf()
    } else {
        root.join(path)
    };
    let cleaned = absolutised.clean();
    if !cleaned.is_absolute() {
        return Err(UploadSetError::PathNotAbsolute {
            path: cleaned,
            root: root.to_path_buf(),
            span,
        });
    }
    cleaned
        .strip_prefix(root)
        .map(PathBuf::from)
        .map_err(|_| UploadSetError::PathOutsideRoot {
            path: cleaned,
            root: root.to_path_buf(),
            span,
        })
}

fn make_absolute(path: &Path) -> PathBuf {
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()
            .map(|cwd| cwd.join(path))
            .unwrap_or_else(|_| path.to_path_buf())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lusid_system::{Arch, Hostname, Linux, Os};
    use rimu::SourceId;

    fn debian_machine() -> Machine {
        Machine {
            hostname: Hostname::from("web-a".to_string()),
            arch: Arch::X86_64,
            os: Os::Linux(Linux::Debian { version: 13 }),
            vm: None,
            remote: None,
            user: None,
        }
    }

    fn empty_span() -> Span {
        Span::new(SourceId::empty(), 0, 0)
    }

    fn store(p: &str) -> StoreItemId {
        StoreItemId::LocalFile(PathBuf::from(p))
    }

    #[test]
    fn manifest_dedupes_paths_with_dotdot_segments() {
        let root = PathBuf::from("/tmp/proj");
        let plan_reads = vec![
            store("/tmp/proj/servers/rpi4b-1/../../plans/base.lusid"),
            store("/tmp/proj/plans/base.lusid"),
        ];
        let manifest = build_manifest(&root, &plan_reads, &[]).unwrap();
        assert_eq!(manifest.len(), 1);
        assert!(manifest.contains(Path::new("plans/base.lusid")));
    }

    #[test]
    fn manifest_collects_plan_and_host_paths() {
        let root = PathBuf::from("/tmp/proj");
        let plan_reads = vec![store("/tmp/proj/servers/web/web.lusid")];
        let host_paths = vec![(
            PathBuf::from("/tmp/proj/servers/web/files/zshrc"),
            empty_span(),
        )];
        let manifest = build_manifest(&root, &plan_reads, &host_paths).unwrap();
        assert!(manifest.contains(Path::new("servers/web/web.lusid")));
        assert!(manifest.contains(Path::new("servers/web/files/zshrc")));
    }

    #[test]
    fn manifest_sorts_parent_dirs_before_children() {
        let root = PathBuf::from("/tmp/proj");
        let host_paths = vec![
            (PathBuf::from("/tmp/proj/files/zshrc/inner"), empty_span()),
            (PathBuf::from("/tmp/proj/files"), empty_span()),
        ];
        let manifest = build_manifest(&root, &[], &host_paths).unwrap();
        let collected: Vec<_> = manifest.iter().collect();
        assert_eq!(collected[0], Path::new("files"));
        assert_eq!(collected[1], Path::new("files/zshrc/inner"));
    }

    #[test]
    fn manifest_errors_when_path_escapes_root() {
        let root = PathBuf::from("/tmp/proj");
        let host_paths = vec![(PathBuf::from("/etc/passwd"), empty_span())];
        let err = build_manifest(&root, &[], &host_paths).unwrap_err();
        assert!(matches!(err, UploadSetError::PathOutsideRoot { .. }));
    }

    #[test]
    fn manifest_errors_when_dotdot_climbs_above_root() {
        let root = PathBuf::from("/tmp/proj");
        let plan_reads = vec![store("/tmp/proj/../escaped.lusid")];
        let err = build_manifest(&root, &plan_reads, &[]).unwrap_err();
        assert!(matches!(err, UploadSetError::PathOutsideRoot { .. }));
    }

    #[test]
    fn manifest_normalizes_host_path_resolved_via_dotdot_in_span_source() {
        // Sub-plan loaded via `../../plans/base.lusid` resolves an inline
        // host path against the sub-plan's directory. The resulting absolute
        // path keeps the `..` segments verbatim; lexical normalisation
        // collapses them back to the canonical location under the project
        // root.
        let root = PathBuf::from("/tmp/proj");
        let host_paths = vec![(
            PathBuf::from("/tmp/proj/servers/web/../../plans/files/zshrc"),
            empty_span(),
        )];
        let manifest = build_manifest(&root, &[], &host_paths).unwrap();
        assert!(manifest.contains(Path::new("plans/files/zshrc")));
        assert_eq!(manifest.len(), 1);
    }

    #[test]
    fn relativize_agrees_with_manifest_normalization() {
        let root = PathBuf::from("/tmp/proj");
        let plan = PathBuf::from("/tmp/proj/./servers/web/web.lusid");
        let rel = relativize(&root, &plan).unwrap();
        assert_eq!(rel, Path::new("servers/web/web.lusid"));
    }

    #[test]
    fn synthesize_system_applies_debian_defaults_for_named_user() {
        let sys = synthesize_system(&debian_machine(), "mikey");
        assert_eq!(sys.user.name, "mikey");
        assert_eq!(sys.user.home, PathBuf::from("/home/mikey"));
        assert_eq!(sys.user.primary_group, "mikey");
    }

    #[test]
    fn synthesize_system_uses_root_home_for_root() {
        let sys = synthesize_system(&debian_machine(), "root");
        assert_eq!(sys.user.home, PathBuf::from("/root"));
        assert_eq!(sys.user.primary_group, "root");
    }

    #[test]
    fn synthesize_system_honours_per_machine_override() {
        let mut machine = debian_machine();
        machine.user = Some(lusid_machine::MachineUser {
            name: None,
            home: Some(PathBuf::from("/srv/mikey")),
            primary_group: Some("wheel".into()),
        });
        let sys = synthesize_system(&machine, "mikey");
        assert_eq!(sys.user.name, "mikey");
        assert_eq!(sys.user.home, PathBuf::from("/srv/mikey"));
        assert_eq!(sys.user.primary_group, "wheel");
    }
}
