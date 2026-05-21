//! Plain-log fallback for when ratatui is not appropriate (non-TTY stdout,
//! or `--no-tui` set on the CLI). Folds the same `AppUpdate` stream into the
//! same [`AppView`] as the ratatui mode, but emits a one-line digest per
//! event to the parent process's stderr instead of drawing.
//!
//! All output goes to stderr. The parent's stdout is reserved for future
//! pipe-through forwarding of the child's JSON.

use std::future::Future;
use std::pin::Pin;

use lusid_apply_stdio::{AppUpdate, AppView, LeafState, Phase, ResourcesNode};
use lusid_render::Render;
use lusid_tree::Tree;
use tokio::io::{AsyncBufReadExt, AsyncRead, AsyncWrite, BufReader};

use super::TuiError;

/// Drive plain-log mode. Same signature shape as [`super::tui`]: reads JSON
/// `AppUpdate`s from `stdout`, raw stderr lines from `stderr`, races the
/// `wait` future. Returns when the child exits and both streams drain.
///
/// `stdin` mirrors [`super::tui`]'s parameter; plain mode has no interactive
/// confirm so it never writes acks.
pub async fn plain<Stdin, Stdout, Stderr, Wait, WaitError>(
    _stdin: Stdin,
    stdout: Stdout,
    stderr: Stderr,
    wait: Pin<Box<Wait>>,
) -> Result<(), TuiError>
where
    Stdin: AsyncWrite + Unpin,
    Stdout: AsyncRead + Unpin,
    Stderr: AsyncRead + Unpin,
    Wait: Future<Output = Result<(), WaitError>>,
    WaitError: Into<TuiError>,
{
    let mut app = AppView::default();
    let mut stdout_lines = BufReader::new(stdout).lines();
    let mut stderr_lines = BufReader::new(stderr).lines();
    let mut stdout_done = false;
    let mut stderr_done = false;
    let mut outcome: Option<Result<(), TuiError>> = None;

    tokio::pin!(wait);

    loop {
        if outcome.is_some() && stdout_done && stderr_done {
            break;
        }

        tokio::select! {
            result = &mut wait, if outcome.is_none() => {
                outcome = Some(result.map_err(Into::into));
            }

            line = stdout_lines.next_line(), if !stdout_done => {
                match line {
                    Ok(Some(line)) => {
                        if !line.trim().is_empty() {
                            let update: AppUpdate = serde_json::from_str(&line)?;
                            // The digest is computed against pre-fold state.
                            // Every event whose digest references a leaf
                            // label only needs its `resource` field, which the
                            // fold never rewrites, so pre-fold is equivalent
                            // and avoids cloning the update.
                            if let Some(text) = digest(&update, &app) {
                                eprintln!("{text}");
                            }
                            app = app.update(update)?;
                        }
                    }
                    Ok(None) => stdout_done = true,
                    Err(err) => return Err(err.into()),
                }
            }

            line = stderr_lines.next_line(), if !stderr_done => {
                match line {
                    Ok(Some(line)) => {
                        if !line.trim().is_empty() {
                            eprintln!("{line}");
                        }
                    }
                    Ok(None) => stderr_done = true,
                    Err(err) => return Err(err.into()),
                }
            }
        }
    }

    match outcome {
        None => Ok(()),
        Some(result) => result,
    }
}

/// Render one [`AppUpdate`] into a one-line digest. `None` for events the
/// plain log intentionally suppresses (`ResourcesStart`/`ResourcesComplete`
/// only bracket the single tree event).
///
/// `app` is the *pre-fold* view, used to resolve the `resource` label for
/// a leaf-indexed event from the resources tree that arrived earlier.
pub(crate) fn digest(update: &AppUpdate, app: &AppView) -> Option<String> {
    match update {
        AppUpdate::ResourceParams { resource_params } => {
            let n = count_tree_leaves(resource_params);
            Some(format!("parsed plan: {n} items"))
        }
        AppUpdate::ResourcesStart | AppUpdate::ResourcesComplete => None,
        AppUpdate::ResourcesNode { tree, .. } => {
            let n = count_tree_leaves(tree);
            Some(format!("expanded to {n} atoms"))
        }
        AppUpdate::PipelineInfo {
            resource_epochs_total,
            ..
        } => Some(format!(
            "scheduled {resource_epochs_total} resource epoch(s)"
        )),
        AppUpdate::ResourceStatesNodeStart { index } => {
            let id = leaf_label(app, *index);
            Some(format!("[probe] #{index} {id}"))
        }
        AppUpdate::ResourceStatesNodeComplete { index, state } => {
            let id = leaf_label(app, *index);
            Some(format!(
                "[probed] #{index} {id}: {}",
                state.render().to_plain_string()
            ))
        }
        AppUpdate::ResourceChangesNode { index, change } => {
            let id = leaf_label(app, *index);
            match change {
                None => Some(format!("[ok] #{index} {id}: no change")),
                Some(change) => Some(format!(
                    "[change] #{index} {id}: {}",
                    change.render().to_plain_string()
                )),
            }
        }
        AppUpdate::OperationsNode { index, operations } => {
            let n = count_tree_leaves(operations);
            Some(format!("[ops] #{index}: {n} operation(s)"))
        }
        AppUpdate::OperationsApplyEpochAdded {
            epoch_index,
            resource_epoch,
            phase,
            operations,
        } => {
            let phase_tag = match phase {
                Phase::Change => "change",
                Phase::OnChange => "on-change",
            };
            Some(format!(
                "[epoch] #{epoch_index} (resource {resource_epoch} phase {phase_tag}): {} operation(s)",
                operations.len()
            ))
        }
        AppUpdate::OperationApplyStart { index: (e, o) } => Some(format!("[op] {e}.{o} start")),
        AppUpdate::OperationApplyStdout {
            index: (e, o),
            stdout,
        } => Some(format!("[op {e}.{o}] {}", scrub_for_log(stdout))),
        AppUpdate::OperationApplyStderr {
            index: (e, o),
            stderr,
        } => Some(format!("[op {e}.{o} err] {}", scrub_for_log(stderr))),
        AppUpdate::OperationApplyComplete {
            index: (e, o),
            error,
        } => match error {
            None => Some(format!("[ok] op {e}.{o}")),
            Some(msg) => Some(format!("[err] op {e}.{o}: {msg}")),
        },
        AppUpdate::ResourceApplyFailed { index, error } => {
            let id = leaf_label(app, *index);
            Some(format!("[failed] #{index} {id}: {error}"))
        }
        // Plain mode auto-acks via --yes upstream (the CLI refuses non-TTY
        // apply without --yes), so a producer-side `EpochReady` here is
        // strictly informational; surface it as a one-liner.
        AppUpdate::EpochReady {
            resource_epoch,
            summary,
        } => Some(format!(
            "[epoch {} ready] {} atom(s), {} changed, {} handler(s) pending",
            resource_epoch + 1,
            summary.atoms_total,
            summary.atoms_changed,
            summary.handlers_pending,
        )),
        AppUpdate::ApplyComplete { had_changes } => {
            let n = count_changed_leaves(app);
            // Producer is authoritative on the boolean; the count is a hint.
            // When the count is 0 but `had_changes` is true (e.g. an event
            // got dropped or `ApplyComplete` arrives first), surface the
            // boolean rather than misreporting zero.
            Some(if *had_changes && n > 0 {
                format!("apply complete: {n} change(s)")
            } else if *had_changes {
                "apply complete: changes applied".to_string()
            } else {
                "apply complete: no changes".to_string()
            })
        }
    }
}

/// Maximum length of an operation-output line in the plain-mode digest.
/// Long lines (e.g. progress meters that pack many control sequences into
/// one event) get truncated with an ellipsis so they don't flood the log.
const SCRUB_MAX_LEN: usize = 240;

/// Strip ANSI control sequences and bare `\r` characters from a byte slice,
/// then truncate to `SCRUB_MAX_LEN` chars for plain-log display. The
/// terminal-emulator path in the TUI interprets these bytes; the plain
/// renderer just writes lines to stderr, so a noisy `pacman -S` with cursor
/// moves and colour escapes would otherwise dump literal `\x1b[31m` runs.
/// Non-UTF-8 bytes pass through as U+FFFD via `from_utf8_lossy`.
fn scrub_for_log(bytes: &[u8]) -> String {
    let lossy = String::from_utf8_lossy(bytes);
    let mut out = String::with_capacity(lossy.len());
    let mut chars = lossy.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '\x1b' => {
                // CSI sequence: ESC '[' params... final-byte. The final
                // byte is 0x40..=0x7E. Two-char sequences (ESC + non-`[`)
                // are dropped by simply consuming the next char without
                // emitting it.
                if let Some('[') = chars.next() {
                    for inner in chars.by_ref() {
                        if matches!(inner, '\x40'..='\x7e') {
                            break;
                        }
                    }
                }
            }
            // CR/LF are chunk terminators kept on the wire for the TUI
            // pane; the plain log writes one line per event via `eprintln!`,
            // so re-emitting them here would produce blank lines or
            // mid-line breaks. Bell is just noise.
            '\r' | '\n' | '\x07' => {}
            _ => out.push(c),
        }
    }
    if out.chars().count() > SCRUB_MAX_LEN {
        let truncated: String = out.chars().take(SCRUB_MAX_LEN).collect();
        format!("{truncated}…")
    } else {
        out
    }
}

/// Best-effort label for the atom at `index`: the resource's `Display`
/// rendering, or a placeholder if the slot is missing/branch/before the
/// resources tree arrived.
fn leaf_label(app: &AppView, index: usize) -> String {
    let Some(tree) = app.resources.as_ref() else {
        return format!("(unknown #{index})");
    };
    match tree.nodes.get(index).and_then(Option::as_ref) {
        Some(ResourcesNode::Leaf { state }) => state.resource().render().to_plain_string(),
        _ => format!("(unknown #{index})"),
    }
}

/// Count leaves in any `lusid_tree::Tree`. Works on `PlanTree<ResourceParams>`,
/// `PlanTree<Resource>`, and `PlanTree<Operation>` since they all alias to
/// `Tree<N, PlanMeta>`.
fn count_tree_leaves<N, M>(tree: &Tree<N, M>) -> usize {
    match tree {
        Tree::Leaf { .. } => 1,
        Tree::Branch { children, .. } => children.iter().map(count_tree_leaves).sum(),
    }
}

/// Count leaves whose computed diff was non-empty: `LeafState::Changed`
/// and `LeafState::Failed` (the latter had a change the apply attempted
/// but couldn't complete). Read after folding `ApplyComplete` so the
/// boolean and the count cannot disagree on "something definitely changed".
fn count_changed_leaves(app: &AppView) -> usize {
    let Some(tree) = app.resources.as_ref() else {
        return 0;
    };
    tree.leaves()
        .filter(|state| matches!(state, LeafState::Changed { .. } | LeafState::Failed { .. }))
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;
    use lusid_apply_stdio::AppUpdate;
    use lusid_operation::operations::file::FilePath;
    use lusid_plan::{PlanMeta, PlanTree};
    use lusid_resource::{
        Resource, ResourceChange, ResourceParams, ResourceState,
        apt::{AptChange, AptParams},
        file::{FileResource, FileState},
    };

    fn resource_leaf(path: &str) -> PlanTree<Resource> {
        PlanTree::Leaf {
            meta: PlanMeta::default(),
            node: Resource::File(FileResource::Present {
                path: FilePath::new(path),
                sudo: false,
            }),
        }
    }

    fn params_tree(n: usize) -> PlanTree<ResourceParams> {
        let children = (0..n)
            .map(|i| PlanTree::Leaf {
                meta: PlanMeta::default(),
                node: ResourceParams::Apt(AptParams::Package {
                    package: format!("pkg-{i}"),
                }),
            })
            .collect();
        PlanTree::Branch {
            meta: PlanMeta::default(),
            children,
        }
    }

    fn folded(updates: Vec<AppUpdate>) -> AppView {
        let mut v = AppView::default();
        for u in updates {
            v = v.update(u).expect("update should succeed");
        }
        v
    }

    #[test]
    fn digest_parsed_plan_counts_items() {
        let update = AppUpdate::ResourceParams {
            resource_params: params_tree(3),
        };
        let line = digest(&update, &AppView::default()).unwrap();
        assert_eq!(line, "parsed plan: 3 items");
    }

    #[test]
    fn digest_resources_node_counts_atoms() {
        let tree = PlanTree::Branch {
            meta: PlanMeta::default(),
            children: vec![resource_leaf("/a"), resource_leaf("/b")],
        };
        let update = AppUpdate::ResourcesNode { index: 0, tree };
        let line = digest(&update, &AppView::default()).unwrap();
        assert_eq!(line, "expanded to 2 atoms");
    }

    #[test]
    fn digest_brackets_are_suppressed() {
        let app = AppView::default();
        assert!(digest(&AppUpdate::ResourcesStart, &app).is_none());
        assert!(digest(&AppUpdate::ResourcesComplete, &app).is_none());
    }

    #[test]
    fn digest_probe_lifecycle_emits_atom_label() {
        let tree = PlanTree::Branch {
            meta: PlanMeta::default(),
            children: vec![resource_leaf("/etc/foo"), resource_leaf("/etc/bar")],
        };
        // Apply the tree-arrival event first; subsequent digests read leaf
        // labels from this folded state.
        let app = folded(vec![
            AppUpdate::ResourcesStart,
            AppUpdate::ResourcesNode { index: 0, tree },
        ]);

        let start = AppUpdate::ResourceStatesNodeStart { index: 1 };
        let line = digest(&start, &app).unwrap();
        assert!(line.starts_with("[probe] #1 "));
        assert!(line.contains("/etc/foo"));

        let complete = AppUpdate::ResourceStatesNodeComplete {
            index: 1,
            state: ResourceState::File(FileState::Absent),
        };
        let line = digest(&complete, &app).unwrap();
        assert!(line.starts_with("[probed] #1 "));
        assert!(line.contains("/etc/foo"));
    }

    #[test]
    fn digest_no_change_and_change_paths_differ() {
        let tree = PlanTree::Branch {
            meta: PlanMeta::default(),
            children: vec![resource_leaf("/a"), resource_leaf("/b")],
        };
        let app = folded(vec![
            AppUpdate::ResourcesStart,
            AppUpdate::ResourcesNode { index: 0, tree },
            AppUpdate::ResourceStatesNodeStart { index: 1 },
            AppUpdate::ResourceStatesNodeComplete {
                index: 1,
                state: ResourceState::File(FileState::Absent),
            },
        ]);

        let no_change = AppUpdate::ResourceChangesNode {
            index: 1,
            change: None,
        };
        let line = digest(&no_change, &app).unwrap();
        assert!(line.contains("no change"));

        let change = AppUpdate::ResourceChangesNode {
            index: 1,
            change: Some(ResourceChange::Apt(AptChange::Install {
                package: "nginx".into(),
            })),
        };
        let line = digest(&change, &app).unwrap();
        assert!(line.contains("[change]"));
        assert!(line.contains("nginx"));
    }

    #[test]
    fn digest_apply_complete_branches_on_had_changes() {
        // No fold yet -> the count helper returns 0, so we fall back to
        // the boolean-only summary.
        let line = digest(
            &AppUpdate::ApplyComplete { had_changes: true },
            &AppView::default(),
        )
        .unwrap();
        assert_eq!(line, "apply complete: changes applied");

        let line = digest(
            &AppUpdate::ApplyComplete { had_changes: false },
            &AppView::default(),
        )
        .unwrap();
        assert_eq!(line, "apply complete: no changes");
    }

    #[test]
    fn digest_apply_complete_reports_count_when_changes_are_visible() {
        // Fold a Changed leaf so count_changed_leaves returns >0.
        let tree = PlanTree::Branch {
            meta: PlanMeta::default(),
            children: vec![resource_leaf("/a")],
        };
        let app = folded(vec![
            AppUpdate::ResourcesStart,
            AppUpdate::ResourcesNode { index: 0, tree },
            AppUpdate::ResourceStatesNodeStart { index: 1 },
            AppUpdate::ResourceStatesNodeComplete {
                index: 1,
                state: ResourceState::File(FileState::Absent),
            },
            AppUpdate::ResourceChangesNode {
                index: 1,
                change: Some(ResourceChange::Apt(AptChange::Install {
                    package: "nginx".into(),
                })),
            },
        ]);
        let line = digest(&AppUpdate::ApplyComplete { had_changes: true }, &app).unwrap();
        assert_eq!(line, "apply complete: 1 change(s)");
    }

    #[test]
    fn digest_counts_nested_plan_items() {
        let tree: PlanTree<ResourceParams> = PlanTree::Branch {
            meta: PlanMeta::default(),
            children: vec![
                PlanTree::Leaf {
                    meta: PlanMeta::default(),
                    node: ResourceParams::Apt(AptParams::Package {
                        package: "a".into(),
                    }),
                },
                PlanTree::Branch {
                    meta: PlanMeta::default(),
                    children: vec![
                        PlanTree::Leaf {
                            meta: PlanMeta::default(),
                            node: ResourceParams::Apt(AptParams::Package {
                                package: "b".into(),
                            }),
                        },
                        PlanTree::Leaf {
                            meta: PlanMeta::default(),
                            node: ResourceParams::Apt(AptParams::Package {
                                package: "c".into(),
                            }),
                        },
                    ],
                },
            ],
        };
        let update = AppUpdate::ResourceParams {
            resource_params: tree,
        };
        let line = digest(&update, &AppView::default()).unwrap();
        assert_eq!(line, "parsed plan: 3 items");
    }

    #[test]
    fn scrub_strips_csi_sequences_and_cr() {
        // Colour and cursor sequences disappear; the plain text survives.
        assert_eq!(
            scrub_for_log(b"\x1b[31mred\x1b[0m \x1b[Hhome\rover"),
            "red homeover",
        );
    }

    #[test]
    fn scrub_truncates_long_lines() {
        let huge: Vec<u8> = (0..(SCRUB_MAX_LEN + 50)).map(|_| b'x').collect();
        let scrubbed = scrub_for_log(&huge);
        assert_eq!(scrubbed.chars().count(), SCRUB_MAX_LEN + 1); // + ellipsis
        assert!(scrubbed.ends_with('…'));
    }

    #[test]
    fn scrub_passes_non_utf8_through_as_replacement_char() {
        // The lossy decode replaces `\xff` with U+FFFD; the scrub leaves
        // it in place because the replacement is a normal character.
        let scrubbed = scrub_for_log(b"a\xffb");
        assert!(scrubbed.starts_with('a'));
        assert!(scrubbed.ends_with('b'));
    }
}
