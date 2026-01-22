use comfy_table::Table;
use lusid_machine::Machine;
use lusid_system::Hostname;
use serde::Deserialize;
use std::collections::BTreeMap;
use std::io;
use std::path::{Path, PathBuf};
use thiserror::Error;
use tokio::fs::read_to_string;
use toml::Value;

use crate::Cli;

#[derive(Error, Debug)]
pub enum ConfigError {
    #[error("lusid config not found at: {path}")]
    ConfigNotFound { path: PathBuf },

    #[error("local machine not found: {hostname}")]
    LocalMachineNotFound { hostname: Hostname },

    #[error("machine id not found: {machine_id}")]
    MachineIdNotFound { machine_id: String },

    #[error("failed to get hostname: {0}")]
    GetHostname(#[source] io::Error),

    #[error("failed to read machines file {path}: {source}")]
    Read {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("failed to parse machines file {path}: {source}")]
    Parse {
        path: PathBuf,
        #[source]
        source: toml::de::Error,
    },

    #[error("failed to resolve plan path: {base_path} + {plan_path}")]
    ResolvingPlanPath {
        base_path: PathBuf,
        plan_path: PathBuf,
    },
}

#[derive(Debug, Clone, Deserialize)]
struct ConfigToml {
    #[serde(default)]
    pub machines: BTreeMap<String, MachineConfigToml>,
    pub log: Option<String>,
    pub lusid_apply_linux_x86_64_path: Option<String>,
    pub lusid_apply_linux_aarch64_path: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Config {
    pub path: PathBuf,
    pub machines: BTreeMap<String, MachineConfig>,
    pub log: String,
    pub lusid_apply_linux_x86_64_path: String,
    pub lusid_apply_linux_aarch64_path: String,
}

#[derive(Debug, Clone, Deserialize)]
struct MachineConfigToml {
    #[serde(flatten)]
    pub machine: Machine,
    pub plan: PathBuf,
    pub params: Option<Value>,
}

#[derive(Debug, Clone)]
pub struct MachineConfig {
    pub machine: Machine,
    pub plan: PathBuf,
    pub params: Option<Value>,
}

impl Config {
    pub async fn load(path: &Path, cli: &Cli) -> Result<Self, ConfigError> {
        let config = Self::load_config(path).await?;
        let ConfigToml {
            machines,
            log,
            lusid_apply_linux_x86_64_path,
            lusid_apply_linux_aarch64_path,
        } = config;

        let machines = Self::resolve_machines(machines, path)?;

        let log = cli.log.clone().or(log).unwrap_or("error".into());

        let lusid_apply_linux_x86_64_path = cli
            .lusid_apply_linux_x86_64_path
            .clone()
            .or(lusid_apply_linux_x86_64_path.clone())
            .unwrap_or("lusid-apply-linux-x86-64".into());
        let lusid_apply_linux_aarch64_path = cli
            .lusid_apply_linux_aarch64_path
            .clone()
            .or(lusid_apply_linux_aarch64_path.clone())
            .unwrap_or("lusid-apply-linux-aarch64".into());

        Ok(Config {
            path: path.to_owned(),
            machines,
            log,
            lusid_apply_linux_x86_64_path,
            lusid_apply_linux_aarch64_path,
        })
    }

    pub fn get_machine(&self, machine_id: &str) -> Result<MachineConfig, ConfigError> {
        self.machines
            .get(machine_id)
            .cloned()
            .ok_or_else(|| ConfigError::MachineIdNotFound {
                machine_id: machine_id.to_string(),
            })
    }

    pub fn local_machine(&self) -> Result<MachineConfig, ConfigError> {
        let hostname = Hostname::get().map_err(ConfigError::GetHostname)?;
        self.machines
            .values()
            .find(|cfg| cfg.machine.hostname == hostname)
            .ok_or(ConfigError::LocalMachineNotFound { hostname })
            .cloned()
    }

    pub fn print_machines(&self) {
        let mut table = Table::new();
        table
            .load_preset(comfy_table::presets::UTF8_FULL)
            .apply_modifier(comfy_table::modifiers::UTF8_ROUND_CORNERS)
            .set_content_arrangement(comfy_table::ContentArrangement::Dynamic)
            .set_header(vec!["id", "plan", "hostname", "arch", "os"]);

        for (machine_id, config) in self.machines.iter() {
            let MachineConfig {
                machine,
                plan,
                params: _,
            } = config;
            let Machine {
                hostname,
                arch,
                os,
                vm: _,
            } = machine;
            table.add_row(vec![
                machine_id,
                &plan.to_string_lossy().to_string(),
                &hostname.to_string(),
                &arch.to_string(),
                &os.to_string(),
            ]);
        }

        println!("{table}")
    }

    pub fn root(&self) -> &Path {
        self.path.parent().unwrap()
    }

    async fn load_config(path: &Path) -> Result<ConfigToml, ConfigError> {
        let path = if path.is_dir() {
            path.join("lusid.toml")
        } else {
            path.to_owned()
        };
        let string = read_to_string(&path)
            .await
            .map_err(|source| ConfigError::Read {
                path: path.to_owned(),
                source,
            })?;
        let config = toml::from_str(&string).map_err(|source| ConfigError::Parse {
            path: path.to_owned(),
            source,
        })?;
        Ok(config)
    }

    fn resolve_machines(
        machines: BTreeMap<String, MachineConfigToml>,
        plan_path: &Path,
    ) -> Result<BTreeMap<String, MachineConfig>, ConfigError> {
        machines
            .into_iter()
            .map(|(name, config)| {
                let MachineConfigToml {
                    machine,
                    plan,
                    params,
                } = config;
                Ok((
                    name,
                    MachineConfig {
                        machine,
                        plan: Self::resolve_plan_path(plan_path, &plan)?,
                        params,
                    },
                ))
            })
            .collect::<Result<_, _>>()
    }

    fn resolve_plan_path(base_path: &Path, plan_path: &Path) -> Result<PathBuf, ConfigError> {
        if plan_path.is_absolute() {
            Ok(plan_path.to_path_buf())
        } else {
            base_path
                .parent()
                .map(|parent| parent.join(plan_path))
                .ok_or_else(|| ConfigError::ResolvingPlanPath {
                    base_path: base_path.to_owned(),
                    plan_path: plan_path.to_owned(),
                })
        }
    }
}
