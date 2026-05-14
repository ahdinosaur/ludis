use std::fmt::Display;

use async_trait::async_trait;
use lusid_causality::{CausalityMeta, CausalityTree};
use lusid_cmd::{Command, CommandError};
use lusid_ctx::Context;
use lusid_operation::{Operation, operations::systemd::SystemdOperation};
use lusid_params::{ParseError, ParseParams, StructFields};
use lusid_view::impl_display_render;
use rimu::{Spanned, Value};
use thiserror::Error;

use crate::ResourceType;

#[derive(Debug, Clone)]
pub struct SystemdParams {
    pub name: String,
    pub enabled: Option<bool>,
    pub active: Option<bool>,
    /// `None` defaults to the system instance.
    pub user: Option<bool>,
}

impl ParseParams for SystemdParams {
    fn parse_params(value: Spanned<Value>) -> Result<Self, Spanned<ParseError>> {
        let mut fields = StructFields::new(value)?;
        let name = fields.required_string("name")?;
        let enabled = fields.optional_bool("enabled")?;
        let active = fields.optional_bool("active")?;
        let user = fields.optional_bool("user")?;
        fields.finish()?;
        Ok(SystemdParams {
            name,
            enabled,
            active,
            user,
        })
    }
}

impl Display for SystemdParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            name,
            enabled,
            active,
            user,
        } = self;
        write!(
            f,
            "Systemd(name = {name}, enabled = {enabled:?}, active = {active:?}, user = {user:?})"
        )
    }
}

impl_display_render!(SystemdParams);

#[derive(Debug, Clone)]
pub struct SystemdResource {
    pub name: String,
    pub enabled: bool,
    pub active: bool,
    /// `true` ⇒ the per-user systemd instance, `false` ⇒ the system instance.
    /// Determines whether `systemctl` runs with `--user` and without `sudo`.
    pub user: bool,
}

impl Display for SystemdResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            name,
            enabled,
            active,
            user,
        } = self;
        write!(
            f,
            "Systemd(name = {name}, enabled = {enabled}, active = {active}, user = {user})"
        )
    }
}

impl_display_render!(SystemdResource);

#[derive(Debug, Clone)]
pub struct SystemdState {
    pub enabled: bool,
    pub active: bool,
}

impl Display for SystemdState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { enabled, active } = self;
        write!(f, "Systemd(enabled = {enabled}, active = {active})")
    }
}

impl_display_render!(SystemdState);

#[derive(Error, Debug)]
pub enum SystemdStateError {
    #[error(transparent)]
    Command(#[from] CommandError),

    #[error("failed to parse systemctl show output: missing {field}")]
    MissingField { field: &'static str },

    #[error("unknown systemd ActiveState: {state}")]
    UnknownActiveState { state: String },

    #[error("unknown systemd UnitFileState: {state}")]
    UnknownUnitFileState { state: String },
}

/// Desired-state delta for a systemd unit. `enable` / `active` are `Some(desired)` if a
/// transition is needed on that dimension, `None` if the current state already matches.
/// At least one field is `Some` — otherwise [`Systemd::change`] returns `None`.
#[derive(Debug, Clone)]
pub struct SystemdChange {
    pub name: String,
    pub enable: Option<bool>,
    pub active: Option<bool>,
    pub user: bool,
}

impl Display for SystemdChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            name,
            enable,
            active,
            user,
        } = self;
        let mut verbs: Vec<&'static str> = Vec::new();
        if let Some(enable) = enable {
            verbs.push(if *enable { "enable" } else { "disable" });
        }
        if let Some(active) = active {
            verbs.push(if *active { "start" } else { "stop" });
        }
        let scope = if *user { " --user" } else { "" };
        write!(f, "Systemd::{}({name}){scope}", verbs.join("+"))
    }
}

impl_display_render!(SystemdChange);

#[derive(Debug, Clone)]
pub struct Systemd;

#[async_trait]
impl ResourceType for Systemd {
    const ID: &'static str = "systemd";

    type Params = SystemdParams;
    type Resource = SystemdResource;

    fn resources(params: Self::Params) -> Vec<CausalityTree<Self::Resource>> {
        vec![CausalityTree::leaf(
            CausalityMeta::default(),
            SystemdResource {
                name: params.name,
                enabled: params.enabled.unwrap_or(true),
                active: params.active.unwrap_or(true),
                user: params.user.unwrap_or(false),
            },
        )]
    }

    type State = SystemdState;
    type StateError = SystemdStateError;

    async fn state(
        _ctx: &mut Context,
        resource: &Self::Resource,
    ) -> Result<Self::State, Self::StateError> {
        // `systemctl show` is the stable, scriptable interface: it always exits 0 and
        // emits `Key=Value` lines. For a missing unit it still exits 0 with
        // `LoadState=not-found`, so we detect missing units from the output rather than
        // from exit status.
        //
        // System and user instances are entirely separate — `systemctl show foo` on the
        // system bus reports `not-found` for a unit that exists on the user bus, and
        // vice versa — so we must probe whichever bus the resource targets. `--user`
        // also routes the IPC over `$XDG_RUNTIME_DIR/systemd/private` so it requires
        // no privilege escalation.
        //
        // Note(cc): user-bus access depends on the invoking process inheriting
        // `XDG_RUNTIME_DIR` (and, for some unit operations, `DBUS_SESSION_BUS_ADDRESS`)
        // from an active login session. A future remote-apply over SSH without a tty
        // session may need `loginctl enable-linger` on the target account, or an
        // explicit `XDG_RUNTIME_DIR=/run/user/<uid>` in the apply environment.
        let mut cmd = Command::new("systemctl");
        if resource.user {
            cmd.arg("--user");
        }
        let output = cmd
            .args([
                "show",
                "--property=LoadState,ActiveState,UnitFileState",
                &resource.name,
            ])
            .run()
            .await?;
        let output = String::from_utf8_lossy(&output);

        let mut load_state: Option<&str> = None;
        let mut active_state: Option<&str> = None;
        let mut unit_file_state: Option<&str> = None;
        for line in output.lines() {
            if let Some(v) = line.strip_prefix("LoadState=") {
                load_state = Some(v);
            } else if let Some(v) = line.strip_prefix("ActiveState=") {
                active_state = Some(v);
            } else if let Some(v) = line.strip_prefix("UnitFileState=") {
                unit_file_state = Some(v);
            }
        }

        let load_state =
            load_state.ok_or(SystemdStateError::MissingField { field: "LoadState" })?;
        if load_state == "not-found" {
            // An absent unit file is a valid "nothing here yet" state, not an error —
            // the same shape as apt/pacman's `NotInstalled`. State probing runs once
            // up front before any operations, so a systemd resource whose unit file
            // will be provided by an earlier epoch (e.g. `@resource/pacman` installing
            // `lightdm`) legitimately reports not-found at probe time. Represent it
            // as disabled+inactive; the later `systemctl enable|start` op then runs
            // after the unit is on disk. If the unit is still missing at operation
            // time, systemctl's own stderr surfaces through the apply.
            return Ok(SystemdState {
                enabled: false,
                active: false,
            });
        }

        let active_state = active_state.ok_or(SystemdStateError::MissingField {
            field: "ActiveState",
        })?;
        let active = parse_active_state(active_state)?;

        let unit_file_state = unit_file_state.ok_or(SystemdStateError::MissingField {
            field: "UnitFileState",
        })?;
        let enabled = parse_unit_file_state(unit_file_state)?;

        Ok(SystemdState { enabled, active })
    }

    type Change = SystemdChange;

    fn change(resource: &Self::Resource, state: &Self::State) -> Option<Self::Change> {
        let enable = (resource.enabled != state.enabled).then_some(resource.enabled);
        let active = (resource.active != state.active).then_some(resource.active);
        if enable.is_none() && active.is_none() {
            return None;
        }
        Some(SystemdChange {
            name: resource.name.clone(),
            enable,
            active,
            user: resource.user,
        })
    }

    fn operations(change: Self::Change) -> Vec<CausalityTree<Operation>> {
        let SystemdChange {
            name,
            enable,
            active,
            user,
        } = change;
        let mut ops: Vec<CausalityTree<Operation>> = Vec::new();
        if let Some(enable) = enable {
            let op = if enable {
                SystemdOperation::Enable {
                    name: name.clone(),
                    user,
                }
            } else {
                SystemdOperation::Disable {
                    name: name.clone(),
                    user,
                }
            };
            ops.push(CausalityTree::leaf(
                CausalityMeta::default(),
                Operation::Systemd(op),
            ));
        }
        if let Some(active) = active {
            let op = if active {
                SystemdOperation::Start { name, user }
            } else {
                SystemdOperation::Stop { name, user }
            };
            ops.push(CausalityTree::leaf(
                CausalityMeta::default(),
                Operation::Systemd(op),
            ));
        }
        ops
    }
}

/// Map systemd's `ActiveState` string to a boolean "is running" view.
///
/// Transitional states (`activating`, `reloading`) are treated as `active` to avoid
/// thrashing a service mid-transition. `failed` maps to `inactive` so that a user who
/// declared `active: true` still gets a `start` attempt — if the unit keeps failing,
/// the apply surfaces systemctl's stderr.
fn parse_active_state(state: &str) -> Result<bool, SystemdStateError> {
    match state {
        "active" | "reloading" | "activating" => Ok(true),
        "inactive" | "deactivating" | "failed" => Ok(false),
        other => Err(SystemdStateError::UnknownActiveState {
            state: other.to_string(),
        }),
    }
}

/// Map systemd's `UnitFileState` string to a boolean "is enabled at boot" view.
///
/// `static`, `alias`, `indirect`, `linked*`, `generated`, and `transient` all report
/// as enabled because their presence is authoritative — `systemctl enable` is a no-op
/// on these and `disable` refuses. `masked` reports as disabled (masking blocks
/// activation entirely, which is stricter than disable). Empty `UnitFileState` is
/// common for runtime-only units that have no install hook; treat as disabled.
fn parse_unit_file_state(state: &str) -> Result<bool, SystemdStateError> {
    match state {
        "enabled" | "enabled-runtime" | "static" | "alias" | "indirect" | "linked"
        | "linked-runtime" | "generated" | "transient" => Ok(true),
        "disabled" | "masked" | "masked-runtime" | "" => Ok(false),
        other => Err(SystemdStateError::UnknownUnitFileState {
            state: other.to_string(),
        }),
    }
}
