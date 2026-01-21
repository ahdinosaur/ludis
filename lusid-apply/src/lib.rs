use std::path::PathBuf;

use lusid_apply_stdio::AppUpdate;
use lusid_causality::{compute_epochs, CausalityTree, EpochError};
use lusid_ctx::{Context, ContextError};
use lusid_operation::{Operation, OperationApplyError};
use lusid_params::{ParamValues, ParamValuesFromTypeError};
use lusid_plan::{self, map_plan_subitems, plan, render_plan_tree, PlanError, PlanId, PlanNodeId};
use lusid_resource::{Resource, ResourceState, ResourceStateError};
use lusid_store::Store;
use lusid_tree::FlatTree;
use lusid_view::Render;
use rimu::SourceId;
use thiserror::Error;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tracing::{debug, error, info};

pub struct ApplyOptions {
    pub root_path: PathBuf,
    pub plan_id: PlanId,
    pub params_json: Option<String>,
}

#[derive(Error, Debug)]
pub enum ApplyError {
    #[error(transparent)]
    Context(#[from] ContextError),

    #[error("failed to parse JSON parameters: {0}")]
    JsonParameters(#[source] serde_json::Error),

    #[error("failed to output JSON: {0}")]
    JsonOutput(#[source] serde_json::Error),

    #[error("failed to read operation stdio: {0}")]
    ReadOperationStdio(#[source] tokio::io::Error),

    #[error("failed to write to stdout: {0}")]
    WriteStdout(#[source] tokio::io::Error),

    #[error("failed to flush stdout: {0}")]
    FlushStdout(#[source] tokio::io::Error),

    #[error("failed to convert parameters for Lusid: {0}")]
    ParamValuesFromType(#[from] ParamValuesFromTypeError),

    #[error(transparent)]
    Plan(#[from] PlanError),

    #[error(transparent)]
    Epoch(#[from] EpochError<PlanNodeId>),

    #[error(transparent)]
    ResourceState(#[from] ResourceStateError),

    #[error(transparent)]
    OperationApply(#[from] OperationApplyError),
}

pub async fn apply(options: ApplyOptions) -> Result<(), ApplyError> {
    info!("starting");
    let ApplyOptions {
        root_path,
        plan_id,
        params_json,
    } = options;

    let ctx = Context::create(&root_path)?;
    let mut store = Store::new(ctx.paths().cache_dir());

    info!(plan = %plan_id, "using plan");

    let param_values = match params_json {
        None => {
            info!("no parameters provided");
            None
        }
        Some(json) => {
            let value: serde_json::Value =
                serde_json::from_str(&json).map_err(ApplyError::JsonParameters)?;
            let source_id = SourceId::from("<cli:params>".to_string());
            let params = ParamValues::from_type(value, source_id)?;
            Some(params)
        }
    };

    // Parse/evaluate to tree of resource params.
    let resource_params = plan(plan_id, param_values, &mut store).await?;
    debug!("Resource params: {resource_params:?}");
    emit(AppUpdate::ResourceParams {
        resource_params: render_plan_tree(resource_params.clone()),
    })
    .await?;
    let resource_params = FlatTree::from(resource_params);

    // Get tree of atomic resources.
    emit(AppUpdate::ResourcesStart).await?;
    let resources = resource_params
        .map_tree(
            |node, meta| map_plan_subitems(node, meta, |node| node.resources()),
            |index, tree| {
                emit(AppUpdate::ResourcesNode {
                    index,
                    tree: render_plan_tree(tree),
                })
            },
        )
        .await?;
    debug!("Resources: {:?}", CausalityTree::from(resources.clone()));
    emit(AppUpdate::ResourcesComplete).await?;

    // Get tree of (resource, resource state)
    emit(AppUpdate::ResourceStatesStart).await?;
    let resource_states = resources
        .map_result_async(
            |resource| async move {
                let state = resource.state(&mut ctx).await?;
                Ok::<(Resource, ResourceState), ApplyError>((resource, state))
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
        CausalityTree::from(resource_states.clone()).map(|(_resource, state)| state)
    );
    emit(AppUpdate::ResourceStatesComplete).await?;

    // Get tree of resource changes
    emit(AppUpdate::ResourceChangesStart).await?;
    let resource_changes = resource_states
        .map_option(
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
        CausalityTree::from(resource_changes.clone())
    );
    emit(AppUpdate::ResourceChangesComplete {
        has_changes: !resource_changes.is_empty(),
    })
    .await?;

    if resource_changes.is_empty() {
        info!("No changes to apply!");
        return Ok(());
    };

    // Get CausalityTree<Operations>
    emit(AppUpdate::OperationsStart).await?;
    let operations = resource_changes
        .map_tree(
            |node, meta| map_plan_subitems(node, meta, |node| node.operations()),
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
        CausalityTree::from(operations.clone())
    );
    emit(AppUpdate::OperationsComplete).await?;

    let operation_epochs = compute_epochs(CausalityTree::from(operations))?;
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

            emit(AppUpdate::OperationApplyStart { index }).await?;

            let (output, stdout, stderr) = operation.apply(&mut xtx).await?;

            let output_task = async {
                output.await?;
                Ok::<(), ApplyError>(())
            };

            let stdout_task = {
                let mut lines = BufReader::new(stdout).lines();
                async move {
                    while let Some(line) = lines
                        .next_line()
                        .await
                        .map_err(ApplyError::ReadOperationStdio)?
                    {
                        emit(AppUpdate::OperationApplyStdout {
                            index,
                            stdout: line,
                        })
                        .await?;
                    }
                    Ok::<(), ApplyError>(())
                }
            };

            let stderr_task = {
                let mut lines = BufReader::new(stderr).lines();
                async move {
                    while let Some(line) = lines
                        .next_line()
                        .await
                        .map_err(ApplyError::ReadOperationStdio)?
                    {
                        emit(AppUpdate::OperationApplyStderr {
                            index,
                            stderr: line,
                        })
                        .await?;
                    }
                    Ok::<(), ApplyError>(())
                }
            };

            tokio::try_join!(output_task, stdout_task, stderr_task)?;

            emit(AppUpdate::OperationApplyComplete {
                index: (epoch_index, operation_index),
            })
            .await?;
        }
    }

    info!("Apply completed");
    Ok(())
}

async fn emit(update: AppUpdate) -> Result<(), ApplyError> {
    let mut stdout = tokio::io::stdout();

    stdout
        .write_all(&serde_json::to_vec(&update).map_err(ApplyError::JsonOutput)?)
        .await
        .map_err(ApplyError::WriteStdout)?;

    stdout
        .write_all(b"\n")
        .await
        .map_err(ApplyError::WriteStdout)?;

    stdout.flush().await.map_err(ApplyError::FlushStdout)?;

    Ok(())
}
