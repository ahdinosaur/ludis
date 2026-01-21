use clap::Parser;
use lusid_plan::PlanId;
use std::path::PathBuf;
use tracing::{debug, error};
use tracing_subscriber::{fmt, EnvFilter};

use lusid_apply::{apply, ApplyOptions};

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
    #[arg(long = "params")]
    params_json: Option<String>,

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
