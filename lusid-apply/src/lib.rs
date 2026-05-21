//! Pipeline orchestrator. Public surface: [`apply`] + [`ApplyOptions`].
//! See the crate README for the phase-by-phase pipeline.
//!
//! Stdout is reserved for the newline-delimited [`AppUpdate`] protocol;
//! human-facing output goes to stderr via `tracing`.

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::mem::Discriminant;
use std::path::PathBuf;
use std::sync::LazyLock;

use lusid_apply_stdio::{AckAction, AppUpdate, ChangeLabel, EpochSummary, Phase};
use lusid_causality::{EpochError, compute_epochs};
use lusid_ctx::{Context, ContextError};
use lusid_operation::{Operation, OperationApplyError};
use lusid_params::ParamsContext;
use lusid_plan::{
    self, PlanError, PlanFlatTree, PlanFlatTreeNode, PlanId, PlanMeta, PlanNodeId, PlanTree,
    map_plan_subitems, plan,
};
use lusid_resource::{HostPathValidationError, Resource, ResourceChangeTrait, ResourceStateError};
use lusid_secrets::{LoadError, Redactor, Secrets};
use lusid_store::Store;
use lusid_system::{GetSystemError, System};
use lusid_tree::{FlatTree, FlatTreeNode, Tree};
use rimu::SourceId;
use rimu_interop::{ToRimuError, to_rimu};
use thiserror::Error;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::sync::Mutex;
use tracing::{debug, info, warn};

/// Inputs for [`apply`]. `root_path` is the lusid working-dir root passed to
/// [`Context::create`]; `plan_id` selects a plan; `params_json` is an
/// optional JSON object (validated against the plan's params schema).
///
/// Secrets: if `identity_path` is `Some`, `lusid-apply` loads that SSH
/// identity, reads `lusid-secrets.toml` from `secrets_dir` (defaulting to
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
    /// Skip the per-epoch confirm prompt: every epoch is treated as if the
    /// consumer had acked `Apply`. When `false`, the per-epoch loop emits
    /// [`AppUpdate::EpochReady`] before each non-empty epoch's ops and reads
    /// one line of [`AckAction`] JSON from stdin. Ignored when `parse_only`
    /// is set (no per-epoch loop runs).
    pub yes: bool,
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

    /// Operator rejected the per-epoch confirm prompt (sent `{"action": "abort"}`),
    /// or stdin closed / produced a malformed ack. The producer treats any of
    /// these as an abort and exits without running the in-progress epoch's ops.
    /// Earlier epochs that have already run on the target stay applied; the
    /// message advises a re-run to retry from this epoch.
    #[error(
        "{}",
        format_aborted_by_user(*resource_epoch, *total)
    )]
    AbortedByUser { resource_epoch: usize, total: usize },
}

/// Build the operator-facing message for [`ApplyError::AbortedByUser`]. The
/// "earlier epochs ran" suffix is dropped at epoch 0 so the message doesn't
/// claim work happened when none did; at epoch 1 only one prior epoch ran,
/// so we singularise to avoid the "epochs 1 through 1" awkwardness.
fn format_aborted_by_user(resource_epoch: usize, total: usize) -> String {
    let one_based = resource_epoch + 1;
    let prior_summary = match resource_epoch {
        0 => "no earlier epochs ran".to_string(),
        1 => "epoch 1 has already been applied to the target".to_string(),
        prior_count => {
            format!("epochs 1 through {prior_count} have already been applied to the target")
        }
    };
    format!(
        "aborted at resource epoch {one_based} of {total}; {prior_summary}. \
         Re-run to retry from this epoch."
    )
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
        yes,
    } = options;

    let mut ctx = Context::create(&root_path)?;
    let mut store = Store::new(ctx.paths().cache_dir());
    let system = System::get().await?;

    // Resolve secrets_dir to <root>/secrets by default. Only consulted when
    // an identity is supplied - without one, there's no key to decrypt with
    // so the directory's existence is irrelevant.
    let secrets_dir = secrets_dir.unwrap_or_else(|| root_path.join("secrets"));
    // The is_secret tag on file resources compares against this dir by
    // lexical prefix; the comparison only works if both sides are absolute
    // and free of `.`/`..`. Canonicalise here so a relative `--root` or
    // missing canonicalisation upstream doesn't silently void redaction.
    // Falls through unchanged when the dir doesn't exist yet - no file
    // source can lie under a missing directory anyway.
    let secrets_dir_for_mark = match tokio::fs::canonicalize(&secrets_dir).await {
        Ok(p) => p,
        Err(_) => secrets_dir.clone(),
    };
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

    // Expand each ResourceParams into a tree of Resource atoms. Hand the
    // resolved secrets dir down so file sources rooted under it are tagged
    // `is_secret` and downstream state/change ship redacted Content.
    let secrets_dir_ref = secrets_dir_for_mark.as_path();
    let resources = resource_params_flat
        .map_tree(
            |node, meta| {
                PlanTree::branch(
                    meta,
                    map_plan_subitems(node, |n| n.resources(secrets_dir_ref)),
                )
            },
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

    emit(AppUpdate::PipelineInfo {
        resource_epochs_total: epochs_count,
        atom_epoch: build_atom_epoch_map(&atom_epochs),
    })
    .await?;

    if parse_only {
        info!("parse-only: skipping per-epoch apply loop");
        return Ok(());
    }

    // For each handler-bearing plan-item branch, the latest resource epoch
    // any of its descendant atoms appears in. The on-change phase fires that
    // branch's handlers at the end of its latest epoch. BTreeMap so the
    // on-change phase's iteration order is stable across runs.
    let latest_epoch_by_branch: BTreeMap<usize, usize> =
        build_latest_epoch_by_branch(&atom_epochs, &parent_of, &atoms_flat);

    // Per-epoch confirm: a reader over stdin that produces one [`AckAction`]
    // per `EpochReady`. With `yes` we skip both the emit and the read.
    let mut ack_reader = AckReader::new(yes);

    // Process each resource epoch in causality order.
    //
    // Within each epoch:
    //   - Change phase: probe state for atoms (after prior epochs' ops have
    //     already been applied, so probes see fresh-from-disk state),
    //     compute changes, and apply the change ops. Atoms that change record
    //     their arena index under their nearest handler-bearing ancestor in
    //     `changed_atoms_by_branch`. Tracking *which* atoms changed (not just
    //     which branches) lets handler-op failures attribute back to the exact
    //     leaves whose changes triggered the handler.
    //   - On-change phase: for every handler-bearing branch whose latest
    //     epoch is this one and which has at least one changed atom recorded,
    //     apply its on_change operations. The on-change phase runs after the
    //     change phase's ops complete and before the next epoch's change phase
    //     begins, so handlers fire strictly after the resource atoms they
    //     watch and strictly before any dependent's atoms.
    let mut changed_atoms_by_branch: HashMap<usize, BTreeSet<usize>> = HashMap::new();
    let mut had_changes = false;
    let mut op_epoch_counter: usize = 0;

    for (resource_epoch_idx, atoms) in atom_epochs.into_iter().enumerate() {
        info!(
            epoch = resource_epoch_idx,
            total = epochs_count,
            "processing resource epoch"
        );

        let atoms_total = atoms.len();

        // Change phase: probe states in parallel, then walk results
        // sequentially to emit events, compute changes, collect op subtrees,
        // and mark each changed atom's nearest handler-bearing ancestor.
        let probes = atoms.into_iter().map(|(idx, resource)| {
            let mut ctx = ctx.clone();
            async move {
                emit(AppUpdate::ResourceStatesNodeStart { index: idx }).await?;
                let state = resource.state(&mut ctx).await?;
                Ok::<_, ApplyError>((idx, resource, state))
            }
        });
        let probed = futures_util::future::try_join_all(probes).await?;

        let mut atom_op_subtrees: Vec<(BTreeSet<usize>, PlanTree<Operation>)> = Vec::new();
        let mut change_labels: Vec<ChangeLabel> = Vec::new();
        for (idx, resource, state) in probed {
            emit(AppUpdate::ResourceStatesNodeComplete {
                index: idx,
                state: state.clone(),
            })
            .await?;

            let change = resource.change(&state);

            if let Some(change) = &change {
                had_changes = true;
                if let Some(branch_idx) = nearest_handler_ancestor(idx, &parent_of, &atoms_flat) {
                    changed_atoms_by_branch
                        .entry(branch_idx)
                        .or_default()
                        .insert(idx);
                }
                change_labels.push(ChangeLabel {
                    atom_id: resource.to_string(),
                    kind: change.kind(),
                    summary: change.to_string(),
                });
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

                let source_atoms: BTreeSet<usize> = BTreeSet::from([idx]);
                atom_op_subtrees.extend(scoped.into_iter().map(|s| (source_atoms.clone(), s)));
            }
        }

        // Count handler-bearing plan-item branches whose latest atom landed
        // in this epoch and which have at least one descendant change marked
        // (possibly from an earlier epoch). These are the on-change-phase
        // handlers queued for after the change phase.
        let handlers_pending = latest_epoch_by_branch
            .iter()
            .filter(|(branch_idx, latest)| {
                **latest == resource_epoch_idx && changed_atoms_by_branch.contains_key(branch_idx)
            })
            .count();

        let atoms_changed = change_labels.len();

        // Per-epoch confirm gate. Empty epochs (no atom changes AND no
        // handlers pending) skip the prompt to reduce fatigue: nothing in
        // them mutates the target, so the operator never has to ack a no-op.
        if atoms_changed > 0 || handlers_pending > 0 {
            let summary =
                build_epoch_summary(atoms_total, atoms_changed, handlers_pending, change_labels);
            emit(AppUpdate::EpochReady {
                resource_epoch: resource_epoch_idx,
                summary,
            })
            .await?;

            match ack_reader.next_ack().await {
                AckAction::Apply => {}
                AckAction::Abort => {
                    info!(epoch = resource_epoch_idx, "aborted by user");
                    emit(AppUpdate::ApplyComplete { had_changes }).await?;
                    return Err(ApplyError::AbortedByUser {
                        resource_epoch: resource_epoch_idx,
                        total: epochs_count,
                    });
                }
            }
        }

        apply_op_phase(
            atom_op_subtrees,
            resource_epoch_idx,
            Phase::Change,
            &mut op_epoch_counter,
            &mut ctx,
            &redactor,
        )
        .await?;

        // On-change phase: collect handlers for branches whose latest epoch
        // is this one and which had at least one atom change. Each handler
        // is attributed to its branch's triggering atoms so a failure marks
        // only those, not unchanged siblings under the same branch.
        let mut handler_leaves: Vec<(BTreeSet<usize>, PlanTree<Operation>)> = Vec::new();
        for (branch_idx, latest) in &latest_epoch_by_branch {
            if *latest != resource_epoch_idx || !changed_atoms_by_branch.contains_key(branch_idx) {
                continue;
            }
            // Take the entry as we fire so any unintended re-entry fails
            // loudly under the debug_assert below.
            let trigger_atoms = changed_atoms_by_branch
                .remove(branch_idx)
                .expect("checked contains_key above");
            debug_assert!(
                !trigger_atoms.is_empty(),
                "on-change phase fired without any triggering atoms",
            );

            let handlers = match atoms_flat.get(*branch_idx) {
                Ok(PlanFlatTreeNode::Branch { meta, .. }) => &meta.handlers,
                _ => unreachable!("latest_epoch_by_branch only contains handler-bearing branches"),
            };
            handler_leaves.extend(handlers.iter().cloned().map(|op| {
                (
                    trigger_atoms.clone(),
                    PlanTree::Leaf {
                        meta: PlanMeta::default(),
                        node: op,
                    },
                )
            }));
        }
        apply_op_phase(
            handler_leaves,
            resource_epoch_idx,
            Phase::OnChange,
            &mut op_epoch_counter,
            &mut ctx,
            &redactor,
        )
        .await?;
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
/// `op_epoch_counter` advances per internal op-epoch so the change and
/// on-change phases within the same resource epoch keep emitting
/// strictly-increasing `OperationsApplyEpochAdded.epoch_index` values.
/// `resource_epoch` and `phase` are stamped on every emitted
/// `OperationsApplyEpochAdded` so the consumer can group ops by outer epoch
/// and separate change-phase ops from on-change-phase handlers.
///
/// Each subtree carries the atom arena indices that produced it - one atom
/// for change-phase subtrees, the branch's triggering atoms for on-change
/// handlers. Merging unions atom sets within a family, so a merged op's
/// failure maps back to every contributing atom.
///
/// Returns early on the first operation failure, emitting
/// `OperationApplyComplete { error: Some(..) }` and one
/// `ResourceApplyFailed` per attributed atom before propagating.
async fn apply_op_phase(
    subtrees: Vec<(BTreeSet<usize>, PlanTree<Operation>)>,
    resource_epoch: usize,
    phase: Phase,
    op_epoch_counter: &mut usize,
    ctx: &mut Context,
    redactor: &Redactor,
) -> Result<(), ApplyError> {
    if subtrees.is_empty() {
        return Ok(());
    }

    // Tag each operation leaf with its source-atom set so attributions ride
    // alongside the op all the way through `compute_epochs` and merge.
    let tagged_children: Vec<PlanTree<(BTreeSet<usize>, Operation)>> = subtrees
        .into_iter()
        .map(|(atoms, subtree)| tag_subtree_with_atoms(&atoms, subtree))
        .collect();
    let combined: PlanTree<(BTreeSet<usize>, Operation)> = PlanTree::Branch {
        meta: PlanMeta::default(),
        children: tagged_children,
    };
    let op_epochs = compute_epochs(combined.map(Some).map_meta(PlanMeta::to_causality))?;
    debug!(
        ?phase,
        resource_epoch,
        op_epochs = op_epochs.len(),
        "phase produced internal op epochs"
    );

    for ops_in_epoch in op_epochs {
        let merged_with_atoms = merge_with_attributions(ops_in_epoch);
        let merged: Vec<Operation> = merged_with_atoms.iter().map(|(_, op)| op.clone()).collect();

        emit(AppUpdate::OperationsApplyEpochAdded {
            epoch_index: *op_epoch_counter,
            resource_epoch,
            phase,
            operations: merged.clone(),
        })
        .await?;

        for (op_idx, (op_atoms, operation)) in merged_with_atoms.iter().enumerate() {
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
                let error_message = error.to_string();
                emit(AppUpdate::OperationApplyComplete {
                    index,
                    error: Some(error_message.clone()),
                })
                .await?;
                for &atom_idx in op_atoms {
                    emit(AppUpdate::ResourceApplyFailed {
                        index: atom_idx,
                        error: error_message.clone(),
                    })
                    .await?;
                }
                return Err(error);
            } else {
                emit(AppUpdate::OperationApplyComplete { index, error: None }).await?;
            }
        }

        *op_epoch_counter += 1;
    }

    Ok(())
}

/// Walk a `PlanTree<Operation>` and tag every leaf with `atoms`, preserving
/// branch metadata. Used by `apply_op_phase` to thread per-subtree atom
/// attributions through `compute_epochs` without changing the causality
/// types.
fn tag_subtree_with_atoms(
    atoms: &BTreeSet<usize>,
    subtree: PlanTree<Operation>,
) -> PlanTree<(BTreeSet<usize>, Operation)> {
    match subtree {
        Tree::Leaf { meta, node } => Tree::Leaf {
            meta,
            node: (atoms.clone(), node),
        },
        Tree::Branch { meta, children } => Tree::Branch {
            meta,
            children: children
                .into_iter()
                .map(|c| tag_subtree_with_atoms(atoms, c))
                .collect(),
        },
    }
}

/// Merge an op-epoch's worth of attributed ops, preserving each merged op's
/// source-atom attribution.
///
/// `Operation::merge` partitions inputs by family and produces outputs of
/// the same family, so we record the per-family atom-set union pre-merge
/// and look it back up per merged op. Distinct ops within a family (e.g.
/// `apt install` vs `apt remove`) all inherit that family-wide union -
/// over-attribution that is acceptable since apply halts on the first
/// failure anyway.
fn merge_with_attributions(
    ops: Vec<(BTreeSet<usize>, Operation)>,
) -> Vec<(BTreeSet<usize>, Operation)> {
    let mut atoms_per_family: HashMap<Discriminant<Operation>, BTreeSet<usize>> = HashMap::new();
    let plain_ops: Vec<Operation> = ops
        .into_iter()
        .map(|(atoms, op)| {
            atoms_per_family
                .entry(std::mem::discriminant(&op))
                .or_default()
                .extend(atoms);
            op
        })
        .collect();

    Operation::merge(plain_ops)
        .into_iter()
        .map(|op| {
            let atoms = atoms_per_family
                .get(&std::mem::discriminant(&op))
                .cloned()
                .unwrap_or_default();
            (atoms, op)
        })
        .collect()
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

/// Flatten `compute_epochs` output to a `leaf_arena_index -> resource_epoch`
/// map for the wire's `PipelineInfo` payload. Only leaves are keys; branch
/// arena slots are absent. Every leaf in the original atoms tree appears
/// exactly once, since `compute_epochs` partitions the leaves across epochs.
fn build_atom_epoch_map(epochs: &[Vec<(usize, Resource)>]) -> HashMap<usize, usize> {
    epochs
        .iter()
        .enumerate()
        .flat_map(|(epoch_idx, atoms)| {
            atoms
                .iter()
                .map(move |(atom_idx, _)| (*atom_idx, epoch_idx))
        })
        .collect()
}

/// For every handler-bearing plan-item branch reachable from any leaf in
/// `epochs`, record the max resource-epoch any descendant atom appears in.
/// Used by the on-change phase to decide when to fire each branch's handlers.
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

/// Cap on the number of [`ChangeLabel`]s the producer includes in an
/// [`EpochSummary`]. Beyond this the count survives via
/// `truncated_count`, so the consumer can show "and N more". Sized for a
/// reasonable terminal screen of "what's about to apply".
const MAX_CHANGE_LABELS: usize = 16;

/// Build an [`EpochSummary`] for the confirm prompt, truncating
/// `change_labels` at [`MAX_CHANGE_LABELS`] so a wide-fanout epoch can't
/// produce an unbounded wire payload.
fn build_epoch_summary(
    atoms_total: usize,
    atoms_changed: usize,
    handlers_pending: usize,
    mut change_labels: Vec<ChangeLabel>,
) -> EpochSummary {
    let truncated_count = change_labels.len().saturating_sub(MAX_CHANGE_LABELS);
    if change_labels.len() > MAX_CHANGE_LABELS {
        change_labels.truncate(MAX_CHANGE_LABELS);
    }
    EpochSummary {
        atoms_total,
        atoms_changed,
        handlers_pending,
        change_labels,
        truncated_count,
    }
}

/// Source of [`AckAction`]s for the per-epoch confirm prompt. `yes` mode
/// short-circuits every read to `Apply`; otherwise the next line on the
/// wrapped reader is parsed. EOF, parse failure, or any IO error returns
/// `Abort` so the producer treats a broken / closed channel as a deliberate
/// refusal.
///
/// Generic over its line source so tests can drive it from in-memory bytes
/// instead of process-wide [`tokio::io::stdin`]; the production constructor
/// wraps stdin.
struct AckReader<R> {
    yes: bool,
    lines: tokio::io::Lines<R>,
}

impl AckReader<BufReader<tokio::io::Stdin>> {
    fn new(yes: bool) -> Self {
        AckReader::from_reader(yes, BufReader::new(tokio::io::stdin()))
    }
}

impl<R> AckReader<R>
where
    R: AsyncBufReadExt + Unpin,
{
    fn from_reader(yes: bool, reader: R) -> Self {
        Self {
            yes,
            lines: reader.lines(),
        }
    }

    async fn next_ack(&mut self) -> AckAction {
        if self.yes {
            return AckAction::Apply;
        }
        match self.lines.next_line().await {
            Ok(Some(line)) => parse_ack(&line),
            // EOF or read failure: parent closed stdin or the channel
            // collapsed. Treat as deliberate abort - never assume consent.
            Ok(None) | Err(_) => AckAction::Abort,
        }
    }
}

/// Parse a single ack line. Malformed JSON or an unknown action becomes
/// `Abort`; spec is "explicit consent required, anything else is no".
///
/// Surfaces the rejection at `warn!` so an operator wondering "why did my
/// apply abort?" can grep the stderr for the malformed line; without this
/// the producer would silently treat garbage as a deliberate no.
fn parse_ack(line: &str) -> AckAction {
    let trimmed = line.trim();
    match serde_json::from_str::<AckAction>(trimmed) {
        Ok(action) => action,
        Err(err) => {
            warn!(line = %trimmed, error = %err, "unrecognized ack on stdin, treating as Abort");
            AckAction::Abort
        }
    }
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
    use lusid_apply_stdio::ChangeKind;
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
                sudo: false,
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
                    sudo: false,
                }),
            )],
            vec![],
            vec![(
                3,
                Resource::File(FileResource::Present {
                    path: FilePath::new("/b"),
                    sudo: false,
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
                sudo: false,
            }),
        )]];
        let latest = build_latest_epoch_by_branch(&epochs, &parent_of, &flat);
        assert!(latest.is_empty());
    }

    fn file_resource(path: &str) -> Resource {
        Resource::File(FileResource::Present {
            path: FilePath::new(path),
            sudo: false,
        })
    }

    #[test]
    fn build_atom_epoch_map_records_each_leaf_with_its_epoch() {
        // Two leaves in epoch 0, one in epoch 2, empty epoch 1.
        let epochs: Vec<Vec<(usize, Resource)>> = vec![
            vec![(2, file_resource("/a")), (3, file_resource("/b"))],
            vec![],
            vec![(5, file_resource("/c"))],
        ];

        let map = build_atom_epoch_map(&epochs);

        assert_eq!(map.len(), 3, "every leaf is represented exactly once");
        assert_eq!(map.get(&2), Some(&0));
        assert_eq!(map.get(&3), Some(&0));
        assert_eq!(map.get(&5), Some(&2));
        assert!(!map.contains_key(&0), "branch indices are not keys");
    }

    #[test]
    fn build_atom_epoch_map_is_empty_for_no_epochs() {
        let epochs: Vec<Vec<(usize, Resource)>> = vec![];
        assert!(build_atom_epoch_map(&epochs).is_empty());
    }

    #[test]
    fn parse_ack_accepts_apply_and_abort() {
        assert_eq!(parse_ack(r#"{"action":"apply"}"#), AckAction::Apply);
        assert_eq!(parse_ack(r#"{"action":"abort"}"#), AckAction::Abort);
        // Trailing whitespace must not break parsing (newline-delimited
        // protocol leaves a `\n` only if the producer didn't strip it).
        assert_eq!(parse_ack(r#" {"action":"apply"} "#), AckAction::Apply);
    }

    #[test]
    fn parse_ack_treats_malformed_input_as_abort() {
        // Anything that isn't a recognized ack falls to Abort; the producer
        // never assumes consent from a garbled line.
        for bad in &[
            "",
            "{}",
            r#"{"action":"maybe"}"#,
            "apply",
            "{not json",
            r#"{"action":42}"#,
        ] {
            assert_eq!(parse_ack(bad), AckAction::Abort, "input: {bad:?}");
        }
    }

    #[tokio::test]
    async fn ack_reader_yes_always_returns_apply() {
        // With yes=true the reader returns Apply without ever inspecting
        // its line source - empty buffer here would otherwise EOF to Abort,
        // so this assertion checks the yes short-circuit is honored.
        let empty: &[u8] = b"";
        let mut reader = AckReader::from_reader(true, empty);
        assert_eq!(reader.next_ack().await, AckAction::Apply);
        assert_eq!(reader.next_ack().await, AckAction::Apply);
    }

    #[tokio::test]
    async fn ack_reader_reads_each_line_in_order() {
        let input: &[u8] = b"{\"action\":\"apply\"}\n{\"action\":\"abort\"}\n";
        let mut reader = AckReader::from_reader(false, input);
        assert_eq!(reader.next_ack().await, AckAction::Apply);
        assert_eq!(reader.next_ack().await, AckAction::Abort);
        // Past EOF -> Abort.
        assert_eq!(reader.next_ack().await, AckAction::Abort);
    }

    #[tokio::test]
    async fn ack_reader_eof_returns_abort() {
        // Parent closed stdin without sending any ack: treat as abort
        // rather than block forever or assume consent.
        let empty: &[u8] = b"";
        let mut reader = AckReader::from_reader(false, empty);
        assert_eq!(reader.next_ack().await, AckAction::Abort);
    }

    #[tokio::test]
    async fn ack_reader_malformed_line_returns_abort_then_continues() {
        // A garbage line aborts that prompt; the reader stays usable for
        // subsequent prompts (a follow-up apply ack still parses cleanly).
        let input: &[u8] = b"garbage\n{\"action\":\"apply\"}\n";
        let mut reader = AckReader::from_reader(false, input);
        assert_eq!(reader.next_ack().await, AckAction::Abort);
        assert_eq!(reader.next_ack().await, AckAction::Apply);
    }

    #[test]
    fn build_epoch_summary_truncates_long_change_lists() {
        let labels: Vec<ChangeLabel> = (0..MAX_CHANGE_LABELS + 5)
            .map(|i| ChangeLabel {
                atom_id: format!("/etc/foo-{i}"),
                kind: ChangeKind::Modified,
                summary: "modify".into(),
            })
            .collect();
        let total_labels = labels.len();
        let summary = build_epoch_summary(20, total_labels, 0, labels);
        assert_eq!(summary.change_labels.len(), MAX_CHANGE_LABELS);
        assert_eq!(summary.truncated_count, 5);
        // atoms_changed mirrors the producer's count, not the truncated
        // list length, so the consumer can report "5 changes (showing 16)".
        assert_eq!(summary.atoms_changed, total_labels);
    }

    #[test]
    fn build_epoch_summary_under_cap_keeps_everything() {
        let labels = vec![ChangeLabel {
            atom_id: "/etc/foo".into(),
            kind: ChangeKind::Modified,
            summary: "modify".into(),
        }];
        let summary = build_epoch_summary(1, 1, 0, labels);
        assert_eq!(summary.change_labels.len(), 1);
        assert_eq!(summary.truncated_count, 0);
    }

    #[test]
    fn format_aborted_by_user_handles_epoch_zero() {
        // Abort at the first epoch: don't claim earlier work happened.
        let msg = format_aborted_by_user(0, 3);
        assert!(msg.contains("resource epoch 1 of 3"));
        assert!(msg.contains("no earlier epochs ran"));
        assert!(!msg.contains("epochs 1"));
    }

    #[test]
    fn format_aborted_by_user_singularises_single_prior_epoch() {
        // Abort at epoch 1 (0-indexed) of 3: exactly one prior epoch ran,
        // so the message is singular rather than "epochs 1 through 1".
        let msg = format_aborted_by_user(1, 3);
        assert!(msg.contains("resource epoch 2 of 3"));
        assert!(msg.contains("epoch 1 has already been applied"));
        assert!(!msg.contains("through"));
    }

    #[test]
    fn format_aborted_by_user_lists_prior_epochs() {
        // Abort at epoch 2 (0-indexed) of 4: epochs 1 and 2 (one-based)
        // already ran. Use "through" for prose clarity rather than `..`.
        let msg = format_aborted_by_user(2, 4);
        assert!(msg.contains("resource epoch 3 of 4"));
        assert!(msg.contains("epochs 1 through 2"));
    }

    fn shell_op(cmd: &str) -> Operation {
        Operation::Command(CommandOperation {
            command: cmd.to_string(),
            executor: CommandExecutor::Shell,
        })
    }

    #[test]
    fn tag_subtree_attaches_atoms_to_every_leaf() {
        // Branch with two leaves: every descendant leaf inherits the same
        // atom set; branch metadata is preserved.
        let subtree: PlanTree<Operation> = PlanTree::Branch {
            meta: PlanMeta::default(),
            children: vec![
                PlanTree::Leaf {
                    meta: PlanMeta::default(),
                    node: shell_op("echo a"),
                },
                PlanTree::Leaf {
                    meta: PlanMeta::default(),
                    node: shell_op("echo b"),
                },
            ],
        };
        let atoms: BTreeSet<usize> = [3, 4].into_iter().collect();
        let tagged = tag_subtree_with_atoms(&atoms, subtree);
        let leaves = collect_leaves(&tagged);
        assert_eq!(leaves.len(), 2);
        for (leaf_atoms, _) in &leaves {
            assert_eq!(leaf_atoms, &atoms);
        }
    }

    fn collect_leaves<T: Clone>(tree: &PlanTree<T>) -> Vec<T> {
        match tree {
            Tree::Leaf { node, .. } => vec![node.clone()],
            Tree::Branch { children, .. } => children.iter().flat_map(collect_leaves).collect(),
        }
    }

    #[test]
    fn merge_with_attributions_unions_atoms_across_dedup() {
        // Two identical Command ops from different atoms collapse to one
        // merged op carrying the union of their atom sets.
        let op = shell_op("reload nginx");
        let merged = merge_with_attributions(vec![
            (BTreeSet::from([1]), op.clone()),
            (BTreeSet::from([7]), op),
        ]);
        assert_eq!(merged.len(), 1);
        let (atoms, _) = &merged[0];
        assert_eq!(atoms, &BTreeSet::from([1, 7]));
    }

    #[test]
    fn merge_with_attributions_keeps_distinct_ops_separate_but_unions_family() {
        // Two distinct Command ops won't dedup, but both inherit the family
        // union - this is the conservative over-attribution noted in the
        // function's doc-comment.
        let merged = merge_with_attributions(vec![
            (BTreeSet::from([1]), shell_op("echo a")),
            (BTreeSet::from([2]), shell_op("echo b")),
        ]);
        assert_eq!(merged.len(), 2);
        for (atoms, _) in &merged {
            assert_eq!(atoms, &BTreeSet::from([1, 2]));
        }
    }

    #[test]
    fn merge_with_attributions_empty_input_is_empty_output() {
        let merged = merge_with_attributions(Vec::new());
        assert!(merged.is_empty());
    }
}
