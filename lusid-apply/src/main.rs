//! `lusid-apply` CLI entry point. Tracing goes to stderr so stdout stays
//! clean for the [`AppUpdate`](lusid_apply_stdio::AppUpdate) JSON stream.
//!
//! Exit codes:
//! - `0` on a clean run (apply or `--parse-only`).
//! - `1` on plan / validation / cycle / secrets / host-path errors - the
//!   plan or its declared targets are wrong.
//! - `2` on IO / context / system / JSON-parameter errors - the operator's
//!   environment is wrong.
//! - `130` on user abort at a per-epoch confirm prompt (mirrors the SIGINT
//!   convention so shells / CI can distinguish "operator said no" from
//!   "something broke").

use clap::Parser;
use lusid_plan::PlanId;
use std::path::PathBuf;
use tracing::{debug, error};
use tracing_subscriber::{EnvFilter, fmt};

use lusid_apply::{ApplyError, ApplyOptions, apply};

#[derive(Parser, Debug)]
#[command(name = "lusid-apply", about = "Apply a Lusid plan.", version)]
struct Cli {
    /// Absolute or relative path to the lusid root.
    #[arg(long = "root")]
    root_path: PathBuf,

    /// Absolute or relative path to the .lusid plan file.
    #[arg(long = "plan")]
    plan_path: PathBuf,

    /// Parameters as a JSON string (top-level object).
    ///
    /// SECURITY: this string lands in the process's `argv[]` and is
    /// visible to any UID on the host via `/proc/<pid>/cmdline` (and
    /// `ps`). Do NOT pass secret values here. For sensitive material,
    /// use the `@resource/secret` resource - it resolves plaintext from
    /// the decrypted bundle on `Context`, which never crosses an
    /// argv boundary.
    #[arg(long = "params")]
    params_json: Option<String>,

    /// Path to the SSH private key used to decrypt project secrets. Omit
    /// to run without secrets (plans referencing `@resource/secret` will
    /// fail at apply time).
    #[arg(long = "identity")]
    identity_path: Option<PathBuf>,

    /// Directory containing `lusid-secrets.toml` and `*.age` ciphertexts.
    /// Defaults to `<root>/secrets`.
    #[arg(long = "secrets-dir")]
    secrets_dir: Option<PathBuf>,

    /// Decrypt every `*.age` under `--secrets-dir` with `--identity`,
    /// ignoring `lusid-secrets.toml`. Used on remote / dev-apply targets
    /// where the host has already filtered the ciphertext set to exactly
    /// what this guest should decrypt. Requires `--identity`.
    #[arg(long = "guest-mode")]
    guest_mode: bool,

    /// Parse and validate the plan without probing target state or running
    /// any operation. Emits the resource-params and atoms tree to stdout,
    /// runs `compute_epochs` to catch cyclic dependencies, then exits.
    #[arg(long = "parse-only")]
    parse_only: bool,

    /// Skip the per-epoch confirm prompt; auto-ack every epoch as Apply.
    /// Without this flag, the apply pauses before each non-empty epoch
    /// emits `EpochReady` and reads one line of [`AckAction`] JSON from
    /// stdin to decide whether to proceed.
    #[arg(long = "yes", short = 'y')]
    yes: bool,

    /// Log level (e.g., trace, debug, info, warn, error). Default: info.
    #[arg(long = "log", default_value = "info")]
    log: String,
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();
    install_tracing(&cli.log);
    debug!(cli = ?cli, "parsed cli");

    let plan_path = cli
        .plan_path
        .canonicalize()
        .unwrap_or(cli.plan_path.clone());
    let plan_id = PlanId::Path(plan_path.clone());
    let options = ApplyOptions {
        root_path: cli.root_path,
        plan_id,
        params_json: cli.params_json,
        identity_path: cli.identity_path,
        secrets_dir: cli.secrets_dir,
        guest_mode: cli.guest_mode,
        parse_only: cli.parse_only,
        yes: cli.yes,
    };

    if let Err(err) = apply(options).await {
        error!("{err}");
        std::process::exit(exit_code(&err));
    }
}

/// Map [`ApplyError`] to a process exit code. `1` means the plan or its
/// declared targets are wrong (validation, cycles, missing secrets, bad
/// host-paths, probe / apply failures); `2` means the operator's
/// environment is wrong (IO, context, system inspection, JSON parameters);
/// `130` means the operator aborted the per-epoch confirm prompt (mirrors
/// the SIGINT convention). Kept as an exhaustive `match` so new error
/// variants force a deliberate classification.
fn exit_code(error: &ApplyError) -> i32 {
    match error {
        ApplyError::Context(_)
        | ApplyError::GetSystem(_)
        | ApplyError::JsonParameters(_)
        | ApplyError::RimuParameters(_)
        | ApplyError::JsonOutput(_)
        | ApplyError::ReadOperationStdio(_)
        | ApplyError::WriteStdout(_)
        | ApplyError::FlushStdout(_) => 2,

        ApplyError::Plan(_)
        | ApplyError::Epoch(_)
        | ApplyError::ResourceState(_)
        | ApplyError::OperationApply(_)
        | ApplyError::Secrets(_)
        | ApplyError::HostPathValidation(_) => 1,

        ApplyError::AbortedByUser { .. } => 130,
    }
}

fn install_tracing(level: &str) {
    let filter = EnvFilter::try_new(level).unwrap_or_else(|_| EnvFilter::new("info"));
    fmt()
        .with_env_filter(filter)
        .with_target(true)
        .with_level(true)
        .with_ansi(true)
        .with_writer(std::io::stderr)
        .init();
}
