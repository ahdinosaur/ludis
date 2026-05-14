//! Pipeline orchestrator: loads a plan, validates params, builds the resource
//! → state → change → operation trees, schedules operations by epoch, and
//! applies them — all while streaming [`AppUpdate`]s as newline-delimited
//! JSON on stdout for the `lusid` TUI to render.
//!
//! The public surface is [`apply`] + [`ApplyOptions`]; `main.rs` is a thin
//! clap wrapper.
//!
//! ## Pipeline (one phase per [`AppUpdate`] group)
//!
//! 1. [`plan`](lusid_plan::plan) — evaluate the plan, validate params,
//!    produce a [`PlanTree<ResourceParams>`](lusid_plan::PlanTree).
//! 2. `ResourceParams → Resources` via `ResourceParams::resources` — each
//!    plan node can expand into multiple resources with intra-scope ordering
//!    (file mode/user/group, etc.), handled by
//!    [`map_plan_subitems`](lusid_plan::map_plan_subitems).
//! 3. `Resource → ResourceState` via async state probes. This is the only
//!    I/O-bound phase prior to apply; emits per-leaf `NodeStart`/`NodeComplete`
//!    so the TUI can show a spinner while each probe runs.
//! 4. `(Resource, State) → ResourceChange` — pure; `None` means "no-op, prune".
//! 5. `ResourceChange → Operations` tree — each change expands to one or
//!    more ordered operations. Short-circuits if step 4 produced no changes.
//! 6. [`compute_epochs`] — Kahn's topological layering over the causality
//!    metadata in the operations tree; operations within an epoch are
//!    independent, operations across epochs have a required-before edge.
//! 7. [`Operation::merge`] + [`Operation::apply`] — per-epoch, merge like
//!    operations (e.g. multiple `apt install`s into one), then apply
//!    sequentially. Stdout + stderr are streamed line-by-line back into
//!    `AppUpdate` events.
//!
//! Human-facing output belongs on stderr (via `tracing`); stdout is reserved
//! for the machine-readable protocol.

use std::path::PathBuf;
use std::sync::LazyLock;

use lusid_apply_stdio::AppUpdate;
use lusid_causality::{CausalityTree, EpochError, compute_epochs};
use lusid_ctx::{Context, ContextError};
use lusid_operation::{Operation, OperationApplyError};
use lusid_params::ParamsContext;
use lusid_plan::{
    self, PlanError, PlanId, PlanMeta, PlanNodeId, PlanTree, inject_handlers, map_plan_subitems,
    plan, render_plan_tree,
};
use lusid_resource::{HostPathValidationError, Resource, ResourceState, ResourceStateError};
use lusid_secrets::{LoadError, Redactor, Secrets};
use lusid_store::Store;
use lusid_system::{GetSystemError, System};
use lusid_tree::FlatTree;
use lusid_view::Render;
use rimu::SourceId;
use rimu_interop::{ToRimuError, to_rimu};
use thiserror::Error;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::sync::Mutex;
use tracing::{debug, error, info};

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
/// goes. Returns `Ok(())` on success (including the "no changes" early
/// return after phase 4) or the first fatal error. On operation failure,
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
    // an identity is supplied — without one, there's no key to decrypt with
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
    // arrive without a real source span — i.e. CLI-supplied `--params`.
    // Anchoring on the project root means a `--params '{"src": "./foo"}'`
    // invocation resolves "./foo" relative to the directory the user thinks of
    // as their project root, not the CWD lusid-apply happens to run from.
    let params_ctx = ParamsContext::new(root_path.clone());

    // Parse/evaluate to tree of resource params.
    let resource_params = plan(plan_id, param_values, &params_ctx, &mut store, &system).await?;
    debug!("Resource params: {resource_params:?}");
    emit(AppUpdate::ResourceParams {
        resource_params: render_plan_tree(resource_params.clone()),
    })
    .await?;
    let resource_params = FlatTree::from(resource_params);

    // Validate `host-path` sources up front so a typo doesn't surface as a
    // confusing apply-time symlink/copy failure. Only `@resource/file` and
    // `@resource/directory` "sourced" variants currently have a host-path source
    // to validate; everything else is a no-op. The probes are independent
    // `lstat`/`stat` calls, so we fan them out — on a network filesystem a
    // serial walk would multiply round-trips by the leaf count.
    let validations = resource_params
        .leaves()
        .map(|params| params.validate_host_paths());
    futures_util::future::try_join_all(validations).await?;

    // Get tree of atomic resources.
    emit(AppUpdate::ResourcesStart).await?;
    let resources = resource_params
        .map_tree(
            |node, meta| PlanTree::branch(meta, map_plan_subitems(node, |node| node.resources())),
            |index, tree| {
                emit(AppUpdate::ResourcesNode {
                    index,
                    tree: render_plan_tree(tree),
                })
            },
        )
        .await?;
    debug!(
        "Resources: {:?}",
        CausalityTree::from(resources.clone().map_meta(PlanMeta::to_causality))
    );
    emit(AppUpdate::ResourcesComplete).await?;

    // Get tree of (resource, resource state)
    emit(AppUpdate::ResourceStatesStart).await?;
    let resource_states = resources
        .map_result_async(
            |resource| {
                let mut ctx = ctx.clone();
                async move {
                    let state = resource.state(&mut ctx).await?;
                    Ok::<(Resource, ResourceState), ApplyError>((resource, state))
                }
            },
            |index| emit(AppUpdate::ResourceStatesNodeStart { index }),
            |index, (_resource, resource_state)| {
                emit(AppUpdate::ResourceStatesNodeComplete {
                    index,
                    node: resource_state.render(),
                })
            },
        )
        .await?;
    debug!(
        "Resource states: {:?}",
        CausalityTree::from(resource_states.clone().map_meta(PlanMeta::to_causality))
            .map(|(_resource, state)| state)
    );
    emit(AppUpdate::ResourceStatesComplete).await?;

    // Get tree of resource changes
    emit(AppUpdate::ResourceChangesStart).await?;
    let resource_changes = resource_states
        .map(
            |(resource, state)| resource.change(&state),
            |index, node| {
                emit(AppUpdate::ResourceChangesNode {
                    index,
                    node: node.map(|n| n.render()),
                })
            },
        )
        .await?;
    debug!(
        "Resource changes: {:?}",
        CausalityTree::from(resource_changes.clone().map_meta(PlanMeta::to_causality))
    );

    let has_changes = resource_changes.leaves().any(|node| node.is_some());

    emit(AppUpdate::ResourceChangesComplete { has_changes }).await?;

    if !has_changes {
        info!("No changes to apply!");
        return Ok(());
    };

    // Get CausalityTree<Operations>
    emit(AppUpdate::OperationsStart).await?;
    let operations = resource_changes
        .map_tree(
            |node, meta| match node {
                Some(node) => {
                    let children = map_plan_subitems(node, |node| node.operations())
                        .map(|tree| tree.map(Some));
                    PlanTree::branch(meta, children)
                }
                None => PlanTree::leaf(meta, None),
            },
            |index, tree| {
                emit(AppUpdate::OperationsNode {
                    index,
                    operations: render_plan_tree(tree),
                })
            },
        )
        .await?;
    debug!(
        "Operations tree: {:?}",
        CausalityTree::from(operations.clone().map_meta(PlanMeta::to_causality))
    );
    emit(AppUpdate::OperationsComplete).await?;

    // Branch-level post-pass: wherever a plan-item branch carries on_change
    // handlers AND has any descendant change, wrap its children in an anchor
    // sub-branch and append the handler leaves alongside.
    let injected = inject_handlers(PlanTree::from(operations));
    debug!("Operations with handlers injected: {injected:?}");

    let operation_epochs =
        compute_epochs(injected.map_meta(PlanMeta::to_causality))?;
    debug!("Operation epochs: {operation_epochs:?}");
    emit(AppUpdate::OperationsApplyStart {
        operations: operation_epochs
            .iter()
            .map(|epoch| epoch.iter().map(Render::render).collect())
            .collect(),
    })
    .await?;

    let epochs_count = operation_epochs.len();
    for (epoch_index, operations) in operation_epochs.into_iter().enumerate() {
        info!(
            epoch = epoch_index,
            count = epochs_count,
            "processing epoch"
        );
        debug!("Operations: {operations:?}");

        let operations = Operation::merge(operations);
        debug!("Merged operations: {operations:?}");

        for (operation_index, operation) in operations.iter().enumerate() {
            let index = (epoch_index, operation_index);

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
    }

    info!("Apply completed");
    Ok(())
}

/// Serializes access to stdout across the apply. Operation stdout/stderr are
/// drained concurrently via `tokio::try_join!`, so without a mutex two
/// `emit()` calls can interleave — one task's JSON can land between another's
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
