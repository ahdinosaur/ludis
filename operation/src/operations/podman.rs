use async_trait::async_trait;
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use serde::{Deserialize, Serialize};
use std::{fmt::Display, pin::Pin};
use thiserror::Error;
use tokio::process::{ChildStderr, ChildStdout};
use tracing::info;

use crate::OperationType;

/// Label key written on every container lusid creates. Its value is the
/// resource layer's `config_hash` of the declared spec, used by drift
/// detection on the next plan to tell "still matches" from "needs recreate".
/// Kept in this crate so the create command and the resource-side reader
/// can't disagree - change the key in one place.
pub const CONFIG_HASH_LABEL: &str = "lusid.config-hash";

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PodmanOperation {
    /// Create a container from `image` under `name`. `--pull=missing` is used
    /// so the image is fetched inline when it isn't already present locally -
    /// keeps the operation set small without exposing a separate Pull op.
    /// `config_hash` is written as the [`CONFIG_HASH_LABEL`] label so the
    /// next state observation can detect drift without re-deriving fields
    /// from podman's normalised inspect output.
    Create {
        name: String,
        image: String,
        command: Option<Vec<String>>,
        env: Vec<String>,
        ports: Vec<String>,
        volumes: Vec<String>,
        restart_policy: Option<String>,
        /// Passed through to `podman create --network <value>`. `None` omits
        /// the flag entirely. See the resource-side field doc for accepted
        /// values and the host-network interaction with `ports`.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        network: Option<String>,
        config_hash: String,
        /// When set, the `podman create` shell-out runs under `sudo -n` so
        /// the container lives in the root podman runtime (rootful podman).
        /// Required when the container needs to bind-mount root-owned host
        /// paths or bind privileged ports. See module docs on
        /// [`crate::OperationType`] for the shared rationale.
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    Start {
        name: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    Stop {
        name: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
    /// Remove a container. Uses `--force` so a running container is stopped
    /// first; this matches the declarative "make this not exist" intent.
    Remove {
        name: String,
        #[serde(default, skip_serializing_if = "std::ops::Not::not")]
        sudo: bool,
    },
}

impl Display for PodmanOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = |sudo: bool| if sudo { "[sudo] " } else { "" };
        match self {
            PodmanOperation::Create {
                name, image, sudo, ..
            } => write!(
                f,
                "{}Podman::Create(name = {name}, image = {image})",
                prefix(*sudo)
            ),
            PodmanOperation::Start { name, sudo } => {
                write!(f, "{}Podman::Start({name})", prefix(*sudo))
            }
            PodmanOperation::Stop { name, sudo } => {
                write!(f, "{}Podman::Stop({name})", prefix(*sudo))
            }
            PodmanOperation::Remove { name, sudo } => {
                write!(f, "{}Podman::Remove({name})", prefix(*sudo))
            }
        }
    }
}

#[derive(Error, Debug)]
pub enum PodmanApplyError {
    #[error(transparent)]
    Command(#[from] CommandError),
}

#[derive(Debug, Clone)]
pub struct Podman;

#[async_trait]
impl OperationType for Podman {
    type Operation = PodmanOperation;

    // Note(cc): merge is a no-op. Each op targets a single named container and
    // ordering matters (create before start, remove before recreate) - that
    // ordering is already expressed in the causality tree, so merging would
    // have to respect it. Not worth the complexity for the typical "handful of
    // containers per plan" case.
    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation> {
        operations
    }

    type ApplyOutput = Pin<Box<dyn Future<Output = Result<(), Self::ApplyError>> + Send + 'static>>;
    type ApplyError = PodmanApplyError;
    type ApplyStdout = ChildStdout;
    type ApplyStderr = ChildStderr;

    async fn apply(
        _ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError> {
        match operation {
            PodmanOperation::Create {
                name,
                image,
                command,
                env,
                ports,
                volumes,
                restart_policy,
                network,
                config_hash,
                sudo,
            } => {
                info!(sudo, "[podman] create: {} from {}", name, image);
                let mut cmd = Command::new("podman");
                cmd.arg("create")
                    .arg("--pull=missing")
                    .arg("--name")
                    .arg(name)
                    .arg("--label")
                    .arg(format!("{CONFIG_HASH_LABEL}={config_hash}"));
                if let Some(policy) = restart_policy {
                    cmd.arg("--restart").arg(policy);
                }
                if let Some(value) = network {
                    cmd.arg("--network").arg(value);
                }
                for value in env {
                    cmd.arg("-e").arg(value);
                }
                for mapping in ports {
                    cmd.arg("-p").arg(mapping);
                }
                for mapping in volumes {
                    cmd.arg("-v").arg(mapping);
                }
                cmd.arg("--").arg(image);
                if let Some(command) = command {
                    cmd.args(command);
                }
                let mut cmd = if *sudo { cmd.sudo() } else { cmd };
                let output = cmd.output().await?;
                Ok((
                    Box::pin(async move {
                        output.status.await?;
                        Ok(())
                    }),
                    output.stdout,
                    output.stderr,
                ))
            }
            PodmanOperation::Start { name, sudo } => {
                info!(sudo, "[podman] start: {}", name);
                let mut cmd = Command::new("podman");
                cmd.arg("start").arg("--").arg(name);
                let mut cmd = if *sudo { cmd.sudo() } else { cmd };
                let output = cmd.output().await?;
                Ok((
                    Box::pin(async move {
                        output.status.await?;
                        Ok(())
                    }),
                    output.stdout,
                    output.stderr,
                ))
            }
            PodmanOperation::Stop { name, sudo } => {
                info!(sudo, "[podman] stop: {}", name);
                let mut cmd = Command::new("podman");
                cmd.arg("stop").arg("--").arg(name);
                let mut cmd = if *sudo { cmd.sudo() } else { cmd };
                let output = cmd.output().await?;
                Ok((
                    Box::pin(async move {
                        output.status.await?;
                        Ok(())
                    }),
                    output.stdout,
                    output.stderr,
                ))
            }
            PodmanOperation::Remove { name, sudo } => {
                info!(sudo, "[podman] remove: {}", name);
                let mut cmd = Command::new("podman");
                cmd.arg("rm").arg("--force").arg("--").arg(name);
                let mut cmd = if *sudo { cmd.sudo() } else { cmd };
                let output = cmd.output().await?;
                Ok((
                    Box::pin(async move {
                        output.status.await?;
                        Ok(())
                    }),
                    output.stdout,
                    output.stderr,
                ))
            }
        }
    }
}
