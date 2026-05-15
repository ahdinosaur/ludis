//! Pipeline orchestrator. Public surface: [`apply`] + [`ApplyOptions`].
//! See the crate README for the phase-by-phase pipeline.
//!
//! Stdout is reserved for the newline-delimited [`AppUpdate`] protocol;
//! human-facing output goes to stderr via `tracing`.

use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::LazyLock;

use lusid_apply_stdio::AppUpdate;
use lusid_causality::{EpochError, compute_epochs};
use lusid_ctx::{Context, ContextError};
use lusid_operation::{Operation, OperationApplyError};
use lusid_params::ParamsContext;
use lusid_plan::{
    self, AtomNode, PlanError, PlanId, PlanMeta, PlanNodeId, PlanTree, inject_handlers,
    map_plan_subitems, plan, render_plan_tree,
};
use lusid_resource::{HostPathValidationError, Resource, ResourceStateError};
use lusid_secrets::{LoadError, Redactor, Secrets};
use lusid_store::Store;
use lusid_system::{GetSystemError, System};
use lusid_tree::{FlatTree, Tree};
use lusid_view::Render;
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

    // Phase 1: parse + evaluate to a tree of resource params.
    let resource_params = plan(plan_id, param_values, &params_ctx, &mut store, &system).await?;
    debug!("Resource params: {resource_params:?}");
    emit(AppUpdate::ResourceParams {
        resource_params: render_plan_tree(resource_params.clone()),
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

    // Phase 2: expand each ResourceParams into a tree of Resource atoms.
    // Synchronous (CPU-only); no point streaming a per-leaf event for a
    // negligible-cost transform.
    let resources = resource_params_flat
        .map_tree(
            |node, meta| PlanTree::branch(meta, map_plan_subitems(node, |n| n.resources())),
            |_index, _tree| async { Ok::<(), ApplyError>(()) },
        )
        .await?;
    let resources_nested: PlanTree<Resource> = resources.into();

    // Phase 3: graft on_change handlers into the atom tree. Wraps each
    // plan-item branch carrying handlers in an anchor branch + handler
    // leaves, and stamps each Resource atom with the anchor ids it lives
    // under (used in the per-epoch loop to decide whether each handler fires).
    let atoms_nested: PlanTree<AtomNode> = inject_handlers(resources_nested);
    debug!("Atoms tree: {atoms_nested:?}");

    emit(AppUpdate::ResourcesStart).await?;
    emit(AppUpdate::ResourcesNode {
        index: 0,
        tree: render_plan_tree(atoms_nested.clone()),
    })
    .await?;
    emit(AppUpdate::ResourcesComplete).await?;

    // Phase 4: assign each tree node a stable index matching the FlatTree
    // arena layout. The TUI uses these indices in subsequent per-leaf
    // events to update its mirror tree.
    let indexed_atoms: PlanTree<(usize, AtomNode)> = enumerate_atoms(atoms_nested);

    // Phase 5: compute resource epochs over the augmented atom tree.
    let atom_epochs = compute_epochs(indexed_atoms.map(Some).map_meta(PlanMeta::to_causality))?;
    let epochs_count = atom_epochs.len();
    info!(epochs = epochs_count, "scheduled resource epochs");
    emit(AppUpdate::ResourceEpochsStart {
        count: epochs_count,
    })
    .await?;

    // Phase 6: process each resource epoch in causality order.
    //
    // For each epoch:
    //   - probe state for Resource atoms in parallel (after prior epochs'
    //     ops have already been applied, so probes see fresh-from-disk
    //     state),
    //   - compute change per atom; record any change in `anchors_changed`
    //     against every anchor branch the atom lives under,
    //   - decide each Handler atom's fate by checking whether its anchor
    //     fired in this run,
    //   - combine per-atom op subtrees + emitted handler ops into one
    //     per-epoch op tree, compute INTERNAL operation epochs, and apply
    //     each (with same-family merging) sequentially.
    let mut anchors_changed: HashSet<PlanNodeId> = HashSet::new();
    let mut had_changes = false;
    let mut op_epoch_counter: usize = 0;

    for (resource_epoch_idx, atoms) in atom_epochs.into_iter().enumerate() {
        info!(
            epoch = resource_epoch_idx,
            total = epochs_count,
            "processing resource epoch"
        );

        // Partition atoms in this epoch by variant.
        let mut resource_atoms: Vec<(usize, Resource, Vec<PlanNodeId>)> = Vec::new();
        let mut handler_atoms: Vec<(usize, Operation, PlanNodeId)> = Vec::new();
        for (idx, atom) in atoms {
            match atom {
                AtomNode::Resource {
                    resource,
                    anchor_ids,
                } => resource_atoms.push((idx, resource, anchor_ids)),
                AtomNode::Handler {
                    operation,
                    anchor_id,
                } => handler_atoms.push((idx, operation, anchor_id)),
            }
        }

        // 6a. Probe states for Resource atoms in parallel.
        let probes = resource_atoms.iter().map(|(idx, resource, _)| {
            let mut ctx = ctx.clone();
            let resource = resource.clone();
            let idx = *idx;
            async move {
                emit(AppUpdate::ResourceStatesNodeStart { index: idx }).await?;
                let state = resource.state(&mut ctx).await?;
                Ok::<_, ApplyError>((idx, state))
            }
        });
        let states = futures_util::future::try_join_all(probes).await?;

        // 6b. For each probed atom, emit state event, compute change,
        // emit change + ops events, and collect op subtrees.
        let mut atom_op_subtrees: Vec<PlanTree<Operation>> = Vec::new();
        for ((idx, state), (_idx, resource, anchor_ids)) in states.iter().zip(&resource_atoms) {
            emit(AppUpdate::ResourceStatesNodeComplete {
                index: *idx,
                node: state.render(),
            })
            .await?;

            let change = resource.change(state);

            if change.is_some() {
                had_changes = true;
                for anchor_id in anchor_ids {
                    anchors_changed.insert(anchor_id.clone());
                }
            }

            emit(AppUpdate::ResourceChangesNode {
                index: *idx,
                node: change.as_ref().map(Render::render),
            })
            .await?;

            if let Some(change) = change {
                let scoped: Vec<PlanTree<Operation>> =
                    map_plan_subitems(change, |c| c.operations()).collect();

                let display_subtree: PlanTree<Operation> = PlanTree::Branch {
                    meta: PlanMeta::default(),
                    children: scoped.clone(),
                };
                emit(AppUpdate::OperationsNode {
                    index: *idx,
                    operations: render_plan_tree(display_subtree),
                })
                .await?;

                atom_op_subtrees.extend(scoped);
            }
        }

        // 6c. Decide each Handler atom's fate. Anchor changes are already
        // recorded (handler atoms are in epochs strictly after their anchor's
        // resource atoms, by inject_handlers' construction).
        for (idx, operation, anchor_id) in handler_atoms {
            let fires = anchors_changed.contains(&anchor_id);

            // For the per-stage trees, mark the handler's progression. We
            // emit Start before Complete to match the lifecycle of real
            // probe events; the "state" string here is conceptually the
            // anchor's change-flag rather than a probed system state.
            emit(AppUpdate::ResourceStatesNodeStart { index: idx }).await?;
            emit(AppUpdate::ResourceStatesNodeComplete {
                index: idx,
                node: if fires {
                    "anchor changed".render()
                } else {
                    "anchor unchanged".render()
                },
            })
            .await?;

            emit(AppUpdate::ResourceChangesNode {
                index: idx,
                node: if fires {
                    Some(operation.render())
                } else {
                    None
                },
            })
            .await?;

            if fires {
                emit(AppUpdate::OperationsNode {
                    index: idx,
                    operations: lusid_view::ViewTree::Leaf {
                        view: operation.render(),
                    },
                })
                .await?;

                atom_op_subtrees.push(PlanTree::Leaf {
                    meta: PlanMeta::default(),
                    node: operation,
                });
            }
        }

        if atom_op_subtrees.is_empty() {
            continue;
        }

        // 6d. Combine and compute internal operation epochs.
        let combined: PlanTree<Operation> = PlanTree::Branch {
            meta: PlanMeta::default(),
            children: atom_op_subtrees,
        };
        let op_epochs = compute_epochs(combined.map(Some).map_meta(PlanMeta::to_causality))?;
        debug!(
            "Resource epoch {resource_epoch_idx} -> {} internal op epoch(s)",
            op_epochs.len()
        );

        // 6e. Apply each internal op epoch (merge same-family, sequential
        // execution within a merged batch). `Operation::merge` of a
        // non-empty input always yields at least one op, so we don't guard
        // for an empty `merged` - if `atom_op_subtrees` was empty we already
        // continued above.
        for ops_in_epoch in op_epochs {
            let merged = Operation::merge(ops_in_epoch);

            emit(AppUpdate::OperationsApplyEpochAdded {
                epoch_index: op_epoch_counter,
                operations: merged.iter().map(Render::render).collect(),
            })
            .await?;

            for (op_idx, operation) in merged.iter().enumerate() {
                let index = (op_epoch_counter, op_idx);
                emit(AppUpdate::OperationApplyStart { index }).await?;

                let (output, stdout, stderr) = operation.apply(&mut ctx).await?;

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

            op_epoch_counter += 1;
        }
    }

    if !had_changes {
        info!("No changes to apply");
    } else {
        info!("Apply completed");
    }
    emit(AppUpdate::ApplyComplete { had_changes }).await?;

    Ok(())
}

/// Walk `tree` in pre-order, assigning each node (branch or leaf) a fresh
/// arena-style index. The order matches `lusid_tree::FlatTree::from`'s
/// `append_tree_nodes` traversal, so the indices on leaves correspond
/// exactly to the indices the TUI's `FlatViewTree` will assign when
/// consuming the equivalent `ResourcesNode { index: 0, tree }` event.
///
/// We walk and label here (rather than threading an arena index out of
/// `compute_epochs`) so the per-epoch loop can carry `(arena_index, atom)`
/// pairs and emit per-leaf events with a stable index.
fn enumerate_atoms<T>(tree: PlanTree<T>) -> PlanTree<(usize, T)> {
    let mut counter: usize = 0;
    enumerate_atoms_inner(tree, &mut counter)
}

fn enumerate_atoms_inner<T>(tree: PlanTree<T>, counter: &mut usize) -> PlanTree<(usize, T)> {
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
            let new_children: Vec<_> = children
                .into_iter()
                .map(|c| enumerate_atoms_inner(c, counter))
                .collect();
            Tree::Branch {
                meta,
                children: new_children,
            }
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
