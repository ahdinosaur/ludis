//! `lusid-apply` CLI entry point. Tracing goes to stderr so stdout stays
//! clean for the [`AppUpdate`](lusid_apply_stdio::AppUpdate) JSON stream.
//! Exits non-zero on any pipeline error (the error is also logged).

use clap::Parser;
use lusid_plan::PlanId;
use std::path::PathBuf;
use tracing::{debug, error};
use tracing_subscriber::{EnvFilter, fmt};

use lusid_apply::{ApplyOptions, apply};

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

    /// Path to the age/SSH identity file used to decrypt project secrets.
    /// Omit to run without secrets (plans referencing `@resource/secret` will
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
    };

    if let Err(err) = apply(options).await {
        error!("{err}");
        std::process::exit(1);
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
