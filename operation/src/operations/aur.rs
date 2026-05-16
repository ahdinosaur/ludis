use async_trait::async_trait;
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_view::impl_display_render;
use serde::{Deserialize, Serialize};
use std::{collections::BTreeSet, fmt::Display, pin::Pin};
use thiserror::Error;
use tokio::process::{ChildStderr, ChildStdout};
use tracing::info;

use crate::OperationType;

// TODO(cc): the AUR helper command is hardcoded to `paru`. `yay` and
// `pikaur` cover the same install surface, but their review-suppression
// flags differ - paru uses `--skipreview`, yay relies on bare
// `--noconfirm`, pikaur uses `--noedit` - so a future `helper` field on
// `AurParams` would route through a per-helper arg builder rather than
// re-using this exact command shape.
const AUR_HELPER: &str = "paru";

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AurOperation {
    Install { packages: Vec<String> },
}

impl Display for AurOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AurOperation::Install { packages } => {
                write!(f, "Aur::Install(packages = [{}])", packages.join(", "))
            }
        }
    }
}

impl_display_render!(AurOperation);

#[derive(Error, Debug)]
pub enum AurApplyError {
    #[error(transparent)]
    Command(#[from] CommandError),
}

#[derive(Debug, Clone)]
pub struct Aur;

#[async_trait]
impl OperationType for Aur {
    type Operation = AurOperation;

    fn merge(operations: Vec<Self::Operation>) -> Vec<Self::Operation> {
        let mut install: BTreeSet<String> = BTreeSet::new();

        for operation in operations {
            match operation {
                AurOperation::Install { packages } => {
                    for package in packages {
                        install.insert(package);
                    }
                }
            }
        }

        let mut operations = Vec::new();
        if !install.is_empty() {
            operations.push(AurOperation::Install {
                packages: install.into_iter().collect(),
            })
        }
        operations
    }

    type ApplyOutput = Pin<Box<dyn Future<Output = Result<(), Self::ApplyError>> + Send + 'static>>;
    type ApplyError = AurApplyError;
    type ApplyStdout = ChildStdout;
    type ApplyStderr = ChildStderr;

    async fn apply(
        _ctx: &mut Context,
        operation: &Self::Operation,
    ) -> Result<(Self::ApplyOutput, Self::ApplyStdout, Self::ApplyStderr), Self::ApplyError> {
        match operation {
            AurOperation::Install { packages } => {
                info!("[aur] install: {}", packages.join(", "));
                // Deliberately not wrapped in `.sudo()`: AUR helpers run
                // makepkg, which refuses to build packages as root (paru
                // itself enforces this and exits with "can't install AUR
                // package as root" when euid is 0). lusid-apply is
                // therefore expected to run as the operator user; paru
                // sudo's internally for the `pacman -U` install handoff
                // when the build artefact is ready.
                //
                // Preconditions for non-interactive operation:
                //   - the operator has a primed sudo timestamp (the rest
                //     of lusid relies on `sudo -n` already, so this is
                //     consistent), or NOPASSWD sudoers for pacman;
                //   - `--sudoloop` (below) keeps that timestamp warm for
                //     the duration of long AUR builds, otherwise paru's
                //     end-of-build sudo handoff would block on a closed
                //     stdin and stall.
                let mut cmd = Command::new(AUR_HELPER);
                cmd.arg("-S")
                    .arg("--noconfirm")
                    .arg("--needed")
                    .arg("--skipreview")
                    .arg("--sudoloop")
                    .arg("--color=never")
                    .arg("--")
                    .args(packages);
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

#[cfg(test)]
mod serde_tests {
    use super::*;

    fn round_trip(op: AurOperation) {
        let json = serde_json::to_string(&op).unwrap();
        let back: AurOperation = serde_json::from_str(&json).unwrap();
        assert_eq!(json, serde_json::to_string(&back).unwrap());
    }

    #[test]
    fn round_trip_install() {
        round_trip(AurOperation::Install {
            packages: vec!["paru".into(), "yay".into()],
        });
    }
}
