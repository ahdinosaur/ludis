use std::fmt::Display;

use async_trait::async_trait;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_operation::{Operation, operations::aur::AurOperation};
use lusid_params::{ParseError, ParseParams, StructFields};
use lusid_view::impl_display_render;
use rimu::{Spanned, Value};
use thiserror::Error;

use crate::ResourceType;

#[derive(Debug, Clone)]
pub enum AurParams {
    Package { package: String },
    Packages { packages: Vec<String> },
}

impl ParseParams for AurParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let out = if fields.has("packages") {
            AurParams::Packages {
                packages: fields.required_string_list("packages")?,
            }
        } else {
            AurParams::Package {
                package: fields.required_string("package")?,
            }
        };
        fields.finish()?;
        Ok(out)
    }
}

impl Display for AurParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AurParams::Package { package } => write!(f, "Aur(package = {package})"),
            AurParams::Packages { packages } => {
                write!(f, "Aur(packages = [{}])", packages.join(", "))
            }
        }
    }
}

impl_display_render!(AurParams);

#[derive(Debug, Clone)]
pub struct AurResource {
    pub package: String,
}

impl Display for AurResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { package } = self;
        write!(f, "Aur({package})")
    }
}

impl_display_render!(AurResource);

#[derive(Debug, Clone)]
pub enum AurState {
    NotInstalled,
    Installed,
}

impl Display for AurState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AurState::NotInstalled => write!(f, "Aur::NotInstalled"),
            AurState::Installed => write!(f, "Aur::Installed"),
        }
    }
}

impl_display_render!(AurState);

#[derive(Error, Debug)]
pub enum AurStateError {
    #[error(transparent)]
    Command(#[from] CommandError),

    #[error("failed to determine package status: {output}")]
    ParseStatus { output: String },
}

// TODO(cc): add an `Uninstall` variant — mirror image of the apt resource. A declared
// package cannot currently be retracted.
#[derive(Debug, Clone)]
pub enum AurChange {
    Install { package: String },
}

impl Display for AurChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AurChange::Install { package } => write!(f, "Aur::Install({package})"),
        }
    }
}

impl_display_render!(AurChange);

#[derive(Debug, Clone)]
pub struct Aur;

#[async_trait]
impl ResourceType for Aur {
    const ID: &'static str = "aur";

    type Params = AurParams;
    type Resource = AurResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        match params {
            AurParams::Package { package } => vec![CausalityTree::leaf(
                CausalityMeta::default(),
                AurResource { package },
            )],
            AurParams::Packages { packages } => packages
                .into_iter()
                .map(|package| {
                    CausalityTree::leaf(CausalityMeta::default(), AurResource { package })
                })
                .collect(),
        }
    }

    type State = AurState;
    type StateError = AurStateError;
    async fn state(
        _ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        // AUR-installed packages register in the local pacman database
        // (the AUR helper builds via makepkg, then hands the resulting
        // package off to pacman -U), so `pacman -Q` is the canonical
        // idempotency probe — no need to invoke the AUR helper just to
        // ask "is this package present?".
        Command::new("pacman")
            .args(["-Q", &resource.package])
            .handle(
                |stdout| {
                    let stdout = String::from_utf8_lossy(stdout);
                    if stdout.trim().is_empty() {
                        Err(AurStateError::ParseStatus {
                            output: stdout.to_string(),
                        })
                    } else {
                        Ok(AurState::Installed)
                    }
                },
                |stderr| {
                    let stderr = String::from_utf8_lossy(stderr);
                    if stderr.contains("was not found") {
                        Ok(Some(AurState::NotInstalled))
                    } else {
                        Ok(None)
                    }
                },
            )
            .await?
    }

    type Change = AurChange;
    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        match state {
            AurState::Installed => None,
            AurState::NotInstalled => Some(AurChange::Install {
                package: resource.package.clone(),
            }),
        }
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        // Note(cc): unlike `@core/pacman` we do not emit a system-upgrade
        // step before the install — paru's `-S` already runs `pacman -Sy`
        // internally to refresh the package db before any build, and a
        // sibling `@core/pacman` resource (if present in the same plan)
        // is responsible for the `-Syu` itself. Emitting one here would
        // just double that work.
        //
        // Note(cc): in plans that mix `@core/aur` with `@core/pacman`,
        // the AUR install's epoch is independent of pacman's `-Syu`.
        // Operations within an epoch run serially (see
        // `lusid-apply/src/lib.rs`), so there's no `db.lck` contention.
        // But `Operation::merge` chains families in a fixed order
        // (`Apt → AptRepo → Aur → Pacman → …`), which means a shared
        // epoch consistently runs `paru -S` *before* `pacman -Syu` — the
        // wrong way round for ABI safety: an AUR package can end up
        // linked against a library version that `-Syu` then replaces,
        // requiring a rebuild. Operators who care should flip the order
        // via plan-level `requires` / `required_by` (e.g. give the
        // `@core/pacman` item an `id` and have the `@core/aur` item
        // require it).
        match change {
            AurChange::Install { package } => vec![CausalityTree::Leaf {
                node: Operation::Aur(AurOperation::Install {
                    packages: vec![package],
                }),
                meta: CausalityMeta::default(),
            }],
        }
    }
}
