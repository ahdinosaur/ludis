//! Pipeline orchestrator. Public surface: [`apply`] + [`ApplyOptions`].
//! See the crate README for the phase-by-phase pipeline.
//!
//! Stdout is reserved for the newline-delimited [`AppUpdate`] protocol;
//! human-facing output goes to stderr via `tracing`.

use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::PathBuf;
use std::sync::LazyLock;

use lusid_apply_stdio::AppUpdate;
use lusid_causality::{EpochError, compute_epochs};
use lusid_ctx::{Context, ContextError};
use lusid_operation::{Operation, OperationApplyError};
use lusid_params::ParamsContext;
use lusid_plan::{
    self, PlanError, PlanFlatTree, PlanFlatTreeNode, PlanId, PlanMeta, PlanNodeId, PlanTree,
    map_plan_subitems, plan,
};
use lusid_resource::{HostPathValidationError, Resource, ResourceStateError};
use lusid_secrets::{LoadError, Redactor, Secrets};
use lusid_store::Store;
use lusid_system::{GetSystemError, System};
use lusid_tree::{FlatTree, FlatTreeNode, Tree};
use rimu::SourceId;
use rimu_interop::{ToRimuError, to_rimu};
use thiserror::Error;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::sync::Mutex;
use tracing::{debug, info};

/// Inputs for [`apply`]. `root_path` is the lusid working-dir root passed to
/// [`Context::create`]; `plan_id` selects a plan; `params_json` is an
/// optional JSON object (validated against the plan's params schema).
///
/// Secrets: if `identity_path` is `Some`, `lusid-apply` loads that identity,
/// reads `lusid-secrets.toml` from `secrets_dir` (defaulting to
/// `<root>/secrets`), matches the identity to an alias, and decrypts the
/// subset of `*.age` files declared for that alias. `None` skips secrets
/// entirely (plans that reference `@resource/secret` will fail at apply with a
/// missing-secret error).
///
/// `guest_mode` changes the secrets path for remote / dev-apply guests:
/// skip the `lusid-secrets.toml` lookup and just decrypt every `*.age`
/// under `secrets_dir` with the single identity we were given. The host
/// has already re-encrypted ciphertexts per-target, so whatever landed in
/// `secrets_dir` is exactly the subset this guest is supposed to see.
/// Requires `identity_path` to be set.
pub struct ApplyOptions {
    pub root_path: PathBuf,
    pub plan_id: PlanId,
    pub params_json: Option<String>,
    pub identity_path: Option<PathBuf>,
    pub secrets_dir: Option<PathBuf>,
    pub guest_mode: bool,
    /// Short-circuit after planning + validation: parse the plan, expand
    /// resource params, validate host-paths, build the atoms tree, and run
    /// `compute_epochs` to catch cyclic dependencies. No probes, no changes,
    /// no operations. Emits `ResourceParams`, `ResourcesStart`, `ResourcesNode`,
    /// `ResourcesComplete` and exits without running the per-epoch loop.
    pub parse_only: bool,
}

#[derive(Error, Debug)]
pub enum ApplyError {
    #[error(transparent)]
    Context(#[from] ContextError),

    #[error("failed to get system: {0}")]
    GetSystem(#[from] GetSystemError),

    #[error("failed to parse JSON parameters: {0}")]
    JsonParameters(#[source] serde_json::Error),

    #[error("failed to parse parameters into rimu value: {0}")]
    RimuParameters(#[from] ToRimuError),

    #[error("failed to output JSON: {0}")]
    JsonOutput(#[source] serde_json::Error),

    #[error("failed to read operation stdio: {0}")]
    ReadOperationStdio(#[source] tokio::io::Error),

    #[error("failed to write to stdout: {0}")]
    WriteStdout(#[source] tokio::io::Error),

    #[error("failed to flush stdout: {0}")]
    FlushStdout(#[source] tokio::io::Error),

    #[error(transparent)]
    Plan(#[from] PlanError),

    #[error(transparent)]
    Epoch(#[from] EpochError<PlanNodeId>),

    #[error(transparent)]
    ResourceState(#[from] ResourceStateError),

    #[error(transparent)]
    OperationApply(#[from] OperationApplyError),

    #[error(transparent)]
    Secrets(#[from] LoadError),

    #[error("host-path validation failed: {0}")]
    HostPathValidation(#[from] HostPathValidationError),
}

/// Run the full apply pipeline, streaming [`AppUpdate`]s to stdout as it
/// goes. Returns `Ok(())` on success (including the "no changes" case after
/// every epoch is processed) or the first fatal error. On operation failure,
/// an `OperationApplyComplete { error: Some(..) }` is emitted before the
/// error propagates so the TUI can show which operation failed.
pub async fn apply(options: ApplyOptions) -> Result<(), ApplyError> {
    info!("starting");
    let ApplyOptions {
        root_path,
        plan_id,
        params_json,
        identity_path,
        secrets_dir,
        guest_mode,
        parse_only,
    } = options;

    let mut ctx = Context::create(&root_path)?;
    let mut store = Store::new(ctx.paths().cache_dir());
    let system = System::get().await?;

    // Resolve secrets_dir to <root>/secrets by default. Only consulted when
    // an identity is supplied - without one, there's no key to decrypt with
    // so the directory's existence is irrelevant.
    let secrets_dir = secrets_dir.unwrap_or_else(|| root_path.join("secrets"));
    // Built alongside `Secrets` so it can be cloned into per-operation
    // stdout/stderr scrubbing below. Holds `Arc` clones of the plaintexts,
    // so constructing it here and then moving `secrets` into `ctx` is safe.
    let secrets = Secrets::load(&secrets_dir, identity_path.as_deref(), guest_mode).await?;
    let redactor: Redactor = secrets.redactor();
    ctx.set_secrets(secrets);

    info!(plan = %plan_id, "using plan");

    let param_values = match params_json {
        None => {
            info!("no parameters provided");
            None
        }
        Some(json) => {
            let value: serde_json::Value =
                serde_json::from_str(&json).map_err(ApplyError::JsonParameters)?;
            let value = to_rimu(value, SourceId::empty())?;
            Some(value)
        }
    };

    // Fallback root path for resolving relative `host-path` strings that
    // arrive without a real source span - i.e. CLI-supplied `--params`.
    // Anchoring on the project root means a `--params '{"src": "./foo"}'`
    // invocation resolves "./foo" relative to the directory the user thinks of
    // as their project root, not the CWD lusid-apply happens to run from.
    //
    // In guest mode we refuse that fallback: the operator's root path
    // doesn't exist on this target, so synthesising a path there would
    // surface as a confusing "host-path not found" downstream. See
    // `ParamsContext::forbid_cli_relative_host_paths` for the TODO on
    // future upload+rewrite support.
    let params_ctx = if guest_mode {
        ParamsContext::new(root_path.clone()).forbid_cli_relative_host_paths()
    } else {
        ParamsContext::new(root_path.clone())
    };

    // Parse + evaluate to a tree of resource params.
    let resource_params = plan(plan_id, param_values, &params_ctx, &mut store, &system).await?;
    debug!("Resource params: {resource_params:?}");
    emit(AppUpdate::ResourceParams {
        resource_params: resource_params.clone(),
    })
    .await?;
    let resource_params_flat = FlatTree::from(resource_params);

    // Validate `host-path` sources up front so a typo doesn't surface as a
    // confusing apply-time symlink/copy failure. Only `@resource/file` and
    // `@resource/directory` "sourced" / "linked" variants currently have a
    // host-path source to validate; everything else is a no-op. The probes
    // are independent `lstat`/`stat` calls, so we fan them out - on a
    // network filesystem a serial walk would multiply round-trips by the
    // leaf count.
    let validations = resource_params_flat
        .leaves()
        .map(|params| params.validate_host_paths());
    futures_util::future::try_join_all(validations).await?;

    // Expand each ResourceParams into a tree of Resource atoms.
    let resources = resource_params_flat
        .map_tree(
            |node, meta| PlanTree::branch(meta, map_plan_subitems(node, |n| n.resources())),
            |_index, _tree| async { Ok::<(), ApplyError>(()) },
        )
        .await?;
    let atoms_nested: PlanTree<Resource> = resources.into();
    debug!("Atoms tree: {atoms_nested:?}");

    emit(AppUpdate::ResourcesStart).await?;
    emit(AppUpdate::ResourcesNode {
        index: 0,
        tree: atoms_nested.clone(),
    })
    .await?;
    emit(AppUpdate::ResourcesComplete).await?;

    // Build the arena once. `FlatTree::from` walks in pre-order, matching
    // `enumerate_atoms` below and the indices the consumer's `ResourcesTree`
    // assigns when it folds the `ResourcesNode { index: 0, tree }` event
    // above.
    let atoms_flat: PlanFlatTree<Resource> = atoms_nested.clone().into();
    let parent_of: HashMap<usize, usize> = build_parent_of(&atoms_flat);

    // Tag each leaf with its arena index so the per-epoch loop can carry
    // `(arena_index, atom)` pairs through `compute_epochs`.
    let indexed_atoms: PlanTree<(usize, Resource)> = enumerate_atoms(atoms_nested);
    let atom_epochs = compute_epochs(indexed_atoms.map(Some).map_meta(PlanMeta::to_causality))?;
    let epochs_count = atom_epochs.len();
    info!(epochs = epochs_count, "scheduled resource epochs");

    // TODO(Task 10): emit `PipelineInfo { resource_epochs_total, atom_epoch }`
    // here so consumers see the total epoch count + per-atom epoch mapping
    // under both `--parse-only` and full apply.

    if parse_only {
        info!("parse-only: skipping per-epoch apply loop");
        return Ok(());
    }

    // For each handler-bearing plan-item branch, the latest resource epoch
    // any of its descendant atoms appears in. Phase B fires that branch's
    // handlers at the end of its latest epoch. BTreeMap so Phase B's
    // iteration order is stable across runs.
    let latest_epoch_by_branch: BTreeMap<usize, usize> =
        build_latest_epoch_by_branch(&atom_epochs, &parent_of, &atoms_flat);

    // Process each resource epoch in causality order.
    //
    // Within each epoch:
    //   - Phase A: probe state for atoms (after prior epochs' ops have
    //     already been applied, so probes see fresh-from-disk state),
    //     compute changes, and apply the change ops. Atoms that change mark
    //     their nearest handler-bearing ancestor branch in `changed_branches`.
    //   - Phase B: for every handler-bearing branch whose latest epoch is
    //     this one and which was marked changed, apply its on_change
    //     operations. Phase B runs after Phase A's ops complete and before
    //     the next epoch's Phase A begins, so handlers fire strictly after
    //     the resource atoms they watch and strictly before any dependent's
    //     atoms.
    let mut changed_branches: HashSet<usize> = HashSet::new();
    let mut had_changes = false;
    let mut op_epoch_counter: usize = 0;

    for (resource_epoch_idx, atoms) in atom_epochs.into_iter().enumerate() {
        info!(
            epoch = resource_epoch_idx,
            total = epochs_count,
            "processing resource epoch"
        );

        // Phase A: probe states in parallel, then walk results sequentially
        // to emit events, compute changes, collect op subtrees, and mark
        // each changed atom's nearest handler-bearing ancestor.
        let probes = atoms.into_iter().map(|(idx, resource)| {
            let mut ctx = ctx.clone();
            async move {
                emit(AppUpdate::ResourceStatesNodeStart { index: idx }).await?;
                let state = resource.state(&mut ctx).await?;
                Ok::<_, ApplyError>((idx, resource, state))
            }
        });
        let probed = futures_util::future::try_join_all(probes).await?;

        let mut atom_op_subtrees: Vec<PlanTree<Operation>> = Vec::new();
        for (idx, resource, state) in probed {
            emit(AppUpdate::ResourceStatesNodeComplete {
                index: idx,
                state: state.clone(),
            })
            .await?;

            let change = resource.change(&state);

            if change.is_some() {
                had_changes = true;
                if let Some(branch_idx) = nearest_handler_ancestor(idx, &parent_of, &atoms_flat) {
                    changed_branches.insert(branch_idx);
                }
            }

            emit(AppUpdate::ResourceChangesNode {
                index: idx,
                change: change.clone(),
            })
            .await?;

            if let Some(change) = change {
                let scoped: Vec<PlanTree<Operation>> =
                    map_plan_subitems(change, |c| c.operations()).collect();

                emit(AppUpdate::OperationsNode {
                    index: idx,
                    operations: PlanTree::Branch {
                        meta: PlanMeta::default(),
                        children: scoped.clone(),
                    },
                })
                .await?;

                atom_op_subtrees.extend(scoped);
            }
        }

        apply_op_phase(atom_op_subtrees, &mut op_epoch_counter, &mut ctx, &redactor).await?;

        // Phase B: collect handlers for branches whose latest epoch is this
        // one and which had at least one atom change during apply.
        let mut handler_leaves: Vec<PlanTree<Operation>> = Vec::new();
        for (branch_idx, latest) in &latest_epoch_by_branch {
            if *latest != resource_epoch_idx || !changed_branches.contains(branch_idx) {
                continue;
            }
            // Remove the branch as we fire so any unintended re-entry fails
            // loudly under the debug_assert below.
            let removed = changed_branches.remove(branch_idx);
            debug_assert!(removed, "Phase B fired the same branch twice");

            let handlers = match atoms_flat.get(*branch_idx) {
                Ok(PlanFlatTreeNode::Branch { meta, .. }) => &meta.handlers,
                _ => unreachable!("latest_epoch_by_branch only contains handler-bearing branches"),
            };
            handler_leaves.extend(handlers.iter().cloned().map(|op| PlanTree::Leaf {
                meta: PlanMeta::default(),
                node: op,
            }));
        }
        apply_op_phase(handler_leaves, &mut op_epoch_counter, &mut ctx, &redactor).await?;
    }

    if !had_changes {
        info!("No changes to apply");
    } else {
        info!("Apply completed");
    }
    emit(AppUpdate::ApplyComplete { had_changes }).await?;

    Ok(())
}

/// Apply a batch of operation subtrees: compute their internal operation
/// epochs, merge same-family ops within each, and execute sequentially.
/// `op_epoch_counter` advances per internal op-epoch so Phase A and Phase B
/// calls within the same resource epoch keep emitting strictly-increasing
/// `OperationsApplyEpochAdded.epoch_index` values.
///
/// Returns early on the first operation failure, after emitting
/// `OperationApplyComplete { error: Some(..) }` so the TUI can surface
/// which op failed.
async fn apply_op_phase(
    subtrees: Vec<PlanTree<Operation>>,
    op_epoch_counter: &mut usize,
    ctx: &mut Context,
    redactor: &Redactor,
) -> Result<(), ApplyError> {
    if subtrees.is_empty() {
        return Ok(());
    }

    let combined: PlanTree<Operation> = PlanTree::Branch {
        meta: PlanMeta::default(),
        children: subtrees,
    };
    let op_epochs = compute_epochs(combined.map(Some).map_meta(PlanMeta::to_causality))?;
    debug!("Phase produced {} internal op epoch(s)", op_epochs.len());

    for ops_in_epoch in op_epochs {
        let merged = Operation::merge(ops_in_epoch);

        emit(AppUpdate::OperationsApplyEpochAdded {
            epoch_index: *op_epoch_counter,
            operations: merged.clone(),
        })
        .await?;

        for (op_idx, operation) in merged.iter().enumerate() {
            let index = (*op_epoch_counter, op_idx);
            emit(AppUpdate::OperationApplyStart { index }).await?;

            let (output, stdout, stderr) = operation.apply(ctx).await?;

            let output_task = async {
                output.await?;
                Ok::<(), ApplyError>(())
            };

            let stdout_task = {
                let mut lines = BufReader::new(stdout).lines();
                let redactor = redactor.clone();
                async move {
                    while let Some(line) = lines
                        .next_line()
                        .await
                        .map_err(ApplyError::ReadOperationStdio)?
                    {
                        emit(AppUpdate::OperationApplyStdout {
                            index,
                            stdout: redactor.redact(&line),
                        })
                        .await?;
                    }
                    Ok::<(), ApplyError>(())
                }
            };

            let stderr_task = {
                let mut lines = BufReader::new(stderr).lines();
                let redactor = redactor.clone();
                async move {
                    while let Some(line) = lines
                        .next_line()
                        .await
                        .map_err(ApplyError::ReadOperationStdio)?
                    {
                        emit(AppUpdate::OperationApplyStderr {
                            index,
                            stderr: redactor.redact(&line),
                        })
                        .await?;
                    }
                    Ok::<(), ApplyError>(())
                }
            };

            if let Err(error) = tokio::try_join!(output_task, stdout_task, stderr_task) {
                emit(AppUpdate::OperationApplyComplete {
                    index,
                    error: Some(error.to_string()),
                })
                .await?;
                return Err(error);
            } else {
                emit(AppUpdate::OperationApplyComplete { index, error: None }).await?;
            }
        }

        *op_epoch_counter += 1;
    }

    Ok(())
}

/// Map every non-root arena index to its parent's arena index. The root has
/// no entry. Tombstoned slots are skipped.
fn build_parent_of<Node, Meta>(flat: &FlatTree<Node, Meta>) -> HashMap<usize, usize>
where
    Node: Clone,
    Meta: Clone,
{
    let mut parent_of = HashMap::new();
    for (idx, node) in flat.nodes_indexed() {
        if let FlatTreeNode::Branch { children, .. } = node {
            for &child in children {
                parent_of.insert(child, idx);
            }
        }
    }
    parent_of
}

/// Walk up `parent_of` from `leaf_idx` until we hit a branch whose
/// `meta.handlers` is non-empty. Returns that branch's arena index, or
/// `None` if no ancestor has handlers.
fn nearest_handler_ancestor(
    leaf_idx: usize,
    parent_of: &HashMap<usize, usize>,
    flat: &PlanFlatTree<Resource>,
) -> Option<usize> {
    let mut cur = parent_of.get(&leaf_idx).copied();
    while let Some(idx) = cur {
        if let Ok(PlanFlatTreeNode::Branch { meta, .. }) = flat.get(idx)
            && !meta.handlers.is_empty()
        {
            return Some(idx);
        }
        cur = parent_of.get(&idx).copied();
    }
    None
}

/// For every handler-bearing plan-item branch reachable from any leaf in
/// `epochs`, record the max resource-epoch any descendant atom appears in.
/// Used by Phase B to decide when to fire each branch's handlers.
fn build_latest_epoch_by_branch(
    epochs: &[Vec<(usize, Resource)>],
    parent_of: &HashMap<usize, usize>,
    flat: &PlanFlatTree<Resource>,
) -> BTreeMap<usize, usize> {
    let mut latest: BTreeMap<usize, usize> = BTreeMap::new();
    for (epoch_idx, atoms) in epochs.iter().enumerate() {
        for (atom_idx, _) in atoms {
            if let Some(branch_idx) = nearest_handler_ancestor(*atom_idx, parent_of, flat) {
                latest
                    .entry(branch_idx)
                    .and_modify(|e| *e = (*e).max(epoch_idx))
                    .or_insert(epoch_idx);
            }
        }
    }
    latest
}

/// Tag each leaf with the arena index it would have under
/// `lusid_tree::FlatTree::from` (pre-order, branches and leaves both
/// consuming a slot). Branch positions are still counted; only the leaf
/// indices are kept.
fn enumerate_atoms<T>(tree: PlanTree<T>) -> PlanTree<(usize, T)> {
    fn walk<T>(tree: PlanTree<T>, counter: &mut usize) -> PlanTree<(usize, T)> {
        match tree {
            Tree::Leaf { meta, node } => {
                let idx = *counter;
                *counter += 1;
                Tree::Leaf {
                    meta,
                    node: (idx, node),
                }
            }
            Tree::Branch { meta, children } => {
                *counter += 1;
                Tree::Branch {
                    meta,
                    children: children.into_iter().map(|c| walk(c, counter)).collect(),
                }
            }
        }
    }
    walk(tree, &mut 0)
}

/// Serializes access to stdout across the apply. Operation stdout/stderr are
/// drained concurrently via `tokio::try_join!`, so without a mutex two
/// `emit()` calls can interleave - one task's JSON can land between another's
/// JSON and its trailing newline, which the TUI reads as a single line with
/// trailing characters. Pipe writes are only atomic up to `PIPE_BUF` (4 KiB);
/// AppUpdates with large trees exceed that easily.
static EMIT_LOCK: LazyLock<Mutex<()>> = LazyLock::new(|| Mutex::new(()));

/// Serialize `update` to a single JSON line on stdout and flush.
///
/// The flush is load-bearing: the TUI reads line-by-line with
/// `AsyncBufRead::lines()`, so buffering would make progress updates
/// invisible to the reader even though the work completed long before.
async fn emit(update: AppUpdate) -> Result<(), ApplyError> {
    let mut line = serde_json::to_vec(&update).map_err(ApplyError::JsonOutput)?;
    line.push(b'\n');

    let _guard = EMIT_LOCK.lock().await;
    let mut stdout = tokio::io::stdout();

    stdout
        .write_all(&line)
        .await
        .map_err(ApplyError::WriteStdout)?;

    stdout.flush().await.map_err(ApplyError::FlushStdout)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use lusid_operation::operations::command::{CommandExecutor, CommandOperation};
    use lusid_operation::operations::file::FilePath;
    use lusid_resource::file::FileResource;

    fn handler_op() -> Operation {
        Operation::Command(CommandOperation {
            command: "true".to_string(),
            executor: CommandExecutor::Shell,
        })
    }

    fn resource_leaf(path: &str) -> PlanTree<Resource> {
        PlanTree::Leaf {
            meta: PlanMeta::default(),
            node: Resource::File(FileResource::Present {
                path: FilePath::new(path),
            }),
        }
    }

    fn handler_branch(children: Vec<PlanTree<Resource>>) -> PlanTree<Resource> {
        PlanTree::Branch {
            meta: PlanMeta {
                handlers: vec![handler_op()],
                ..PlanMeta::default()
            },
            children,
        }
    }

    fn plain_branch(children: Vec<PlanTree<Resource>>) -> PlanTree<Resource> {
        PlanTree::Branch {
            meta: PlanMeta::default(),
            children,
        }
    }

    /// Pre-order arena layout: root branch is 0; its handler-bearing child
    /// branch is 1; that branch's two leaves are 2 and 3.
    #[test]
    fn build_parent_of_records_each_node_pointing_at_its_branch() {
        let tree = plain_branch(vec![handler_branch(vec![
            resource_leaf("/a"),
            resource_leaf("/b"),
        ])]);
        let flat: PlanFlatTree<Resource> = tree.into();
        let parent_of = build_parent_of(&flat);

        assert_eq!(parent_of.get(&0), None, "root has no parent entry");
        assert_eq!(
            parent_of.get(&1),
            Some(&0),
            "handler branch's parent is root"
        );
        assert_eq!(
            parent_of.get(&2),
            Some(&1),
            "leaf /a's parent is handler branch"
        );
        assert_eq!(
            parent_of.get(&3),
            Some(&1),
            "leaf /b's parent is handler branch"
        );
    }

    #[test]
    fn nearest_handler_ancestor_skips_intermediate_no_handler_branches() {
        // root (no handlers) -> handler-bearing branch -> no-handler branch -> leaf.
        // The leaf's nearest handler ancestor is the handler-bearing branch
        // (two parent hops), not the immediate no-handler parent.
        let tree = plain_branch(vec![handler_branch(vec![plain_branch(vec![
            resource_leaf("/a"),
        ])])]);
        let flat: PlanFlatTree<Resource> = tree.into();
        let parent_of = build_parent_of(&flat);

        // Arena layout: 0=root, 1=handler-branch, 2=no-handler-branch, 3=leaf.
        let leaf_idx = 3;
        let handler_branch_idx = 1;
        assert_eq!(
            nearest_handler_ancestor(leaf_idx, &parent_of, &flat),
            Some(handler_branch_idx),
        );
    }

    #[test]
    fn nearest_handler_ancestor_returns_nearest_not_outermost() {
        // outer (handlers) -> inner (handlers) -> leaf. The innermost
        // handler-bearing branch wins regardless of how deep the nesting is.
        let tree = plain_branch(vec![handler_branch(vec![handler_branch(vec![
            resource_leaf("/a"),
        ])])]);
        let flat: PlanFlatTree<Resource> = tree.into();
        let parent_of = build_parent_of(&flat);

        // Arena layout: 0=root, 1=outer-handler, 2=inner-handler, 3=leaf.
        let leaf_idx = 3;
        let inner_handler_idx = 2;
        assert_eq!(
            nearest_handler_ancestor(leaf_idx, &parent_of, &flat),
            Some(inner_handler_idx),
        );
    }

    #[test]
    fn nearest_handler_ancestor_returns_none_when_no_ancestor_has_handlers() {
        let tree = plain_branch(vec![resource_leaf("/a")]);
        let flat: PlanFlatTree<Resource> = tree.into();
        let parent_of = build_parent_of(&flat);

        assert_eq!(nearest_handler_ancestor(1, &parent_of, &flat), None);
    }

    #[test]
    fn build_latest_epoch_records_max_epoch_per_handler_branch() {
        // Two leaves under one handler-bearing branch, placed in epochs 0 and 2.
        let tree = plain_branch(vec![handler_branch(vec![
            resource_leaf("/a"),
            resource_leaf("/b"),
        ])]);
        let flat: PlanFlatTree<Resource> = tree.into();
        let parent_of = build_parent_of(&flat);

        // Arena layout: 0=root, 1=handler-branch, 2=leaf /a, 3=leaf /b.
        let epochs = vec![
            vec![(
                2,
                Resource::File(FileResource::Present {
                    path: FilePath::new("/a"),
                }),
            )],
            vec![],
            vec![(
                3,
                Resource::File(FileResource::Present {
                    path: FilePath::new("/b"),
                }),
            )],
        ];

        let latest = build_latest_epoch_by_branch(&epochs, &parent_of, &flat);
        assert_eq!(latest.get(&1), Some(&2), "branch's latest epoch is 2");
        assert_eq!(
            latest.len(),
            1,
            "only the handler-bearing branch is tracked"
        );
    }

    #[test]
    fn build_latest_epoch_skips_branches_without_handlers() {
        let tree = plain_branch(vec![resource_leaf("/a")]);
        let flat: PlanFlatTree<Resource> = tree.into();
        let parent_of = build_parent_of(&flat);

        let epochs = vec![vec![(
            1,
            Resource::File(FileResource::Present {
                path: FilePath::new("/a"),
            }),
        )]];
        let latest = build_latest_epoch_by_branch(&epochs, &parent_of, &flat);
        assert!(latest.is_empty());
    }
}
