//! Declarative description of a lusid *target* machine - distinct from [`lusid_system::System`],
//! which describes the machine lusid is currently running on.
//!
//! A `Machine` names the intended hostname/arch/OS (and, if it should be materialized as
//! a VM, [`MachineVmOptions`] covering cpu/memory/graphics). Wired into the `vm` crate as
//! the input to `Instance::start`.
//
// Note(cc): this crate is deliberately small. As the product picks up remote deployment,
// credentials, or lifecycle policies, those fields land here.

use std::path::PathBuf;

use lusid_system::{Arch, CpuCount, DiskSize, Hostname, MemorySize, Os};
use serde::{Deserialize, Serialize};

/// Declarative spec of a machine we want to provision.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Machine {
    pub hostname: Hostname,
    pub arch: Arch,
    pub os: Os,
    pub vm: Option<MachineVmOptions>,
    pub remote: Option<Remote>,
}

/// VM-specific knobs when `Machine::vm` is `Some`. All fields are optional so
/// defaults can apply per-backend.
#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct MachineVmOptions {
    pub memory_size: Option<MemorySize>,
    pub cpu_count: Option<CpuCount>,
    /// Virtual size (bytes) of the guest root disk overlay. Cloud images
    /// ship with a small partition (~2 GB); raise this when the plan
    /// installs a lot of software. Cloud-init expands the root partition +
    /// filesystem to fill the disk on first boot.
    pub disk_size: Option<DiskSize>,
    pub graphics: Option<bool>,
}

/// SSH connection details for `lusid remote apply` / `lusid remote ssh`.
///
/// `host` is the SSH-reachable address (DNS or IP) - distinct from
/// [`Machine::hostname`], which is the target's *self-name* (matched against
/// `$(hostname)` for `local apply` and seeded into VM cloud-init). The two
/// fields play different roles even though they often hold the same string.
///
/// Privilege model is inferred from `user`: when it equals `"root"`, the
/// remote `lusid-apply` runs directly; otherwise it's wrapped in `sudo -n`.
/// The non-root case requires the target to have passwordless sudo for the
/// SSH user, since the guest needs root to read
/// `/etc/ssh/ssh_host_ed25519_key` (the per-target age identity).
///
/// TODO(cc): the target age identity is hardcoded to
/// `/etc/ssh/ssh_host_ed25519_key` both at the CLI invocation (in
/// `cmd_remote_apply`) and implicitly in the operator's `[machines]`
/// recipient setup. Targets with only `ssh-rsa` host keys silently won't
/// decrypt. If we ever care about that, add a `host_key: Option<PathBuf>`
/// field here - making it optional from day one avoids a breaking config
/// change later.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Remote {
    pub host: String,
    pub port: Option<u16>,
    pub user: Option<String>,
    /// Path to the operator's SSH **private** key, used to authenticate the
    /// SSH session. Tilde-expanded at use site (CLI), not at parse. Defaults
    /// to `~/.ssh/id_ed25519`. Deliberately distinct from the `--identity`
    /// (age) flag elsewhere - these are unrelated identities.
    pub ssh_key: Option<PathBuf>,
}

impl Remote {
    pub fn port(&self) -> u16 {
        self.port.unwrap_or(22)
    }

    pub fn user(&self) -> &str {
        self.user.as_deref().unwrap_or("root")
    }

    pub fn is_root(&self) -> bool {
        self.user() == "root"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn remote_full_form() {
        let r: Remote = toml::from_str(
            r#"
host = "web-a.lan"
port = 2222
user = "mikey"
ssh_key = "/home/mikey/.ssh/custom_key"
"#,
        )
        .unwrap();
        assert_eq!(r.host, "web-a.lan");
        assert_eq!(r.port(), 2222);
        assert_eq!(r.user(), "mikey");
        assert!(!r.is_root());
        assert_eq!(
            r.ssh_key,
            Some(PathBuf::from("/home/mikey/.ssh/custom_key"))
        );
    }

    #[test]
    fn remote_defaults_when_only_host_given() {
        let r: Remote = toml::from_str(r#"host = "web-a.lan""#).unwrap();
        assert_eq!(r.port(), 22);
        assert_eq!(r.user(), "root");
        assert!(r.is_root());
        assert!(r.ssh_key.is_none());
    }

    #[test]
    fn remote_rejects_unknown_field() {
        let err = toml::from_str::<Remote>(
            r#"host = "web-a.lan"
hsot = "typo"
"#,
        )
        .unwrap_err();
        assert!(err.to_string().contains("unknown field"));
    }

    #[test]
    fn machine_with_remote_round_trips() {
        let m: Machine = toml::from_str(
            r#"
hostname = "web-a"
arch = "x86-64"
os = { type = "linux", linux = "debian", debian = 13 }
remote = { host = "web-a.lan", user = "mikey" }
"#,
        )
        .unwrap();
        let remote = m.remote.expect("remote field deserialized");
        assert_eq!(remote.host, "web-a.lan");
        assert_eq!(remote.user(), "mikey");
    }

    #[test]
    fn machine_without_remote_round_trips() {
        let m: Machine = toml::from_str(
            r#"
hostname = "web-a"
arch = "x86-64"
os = { type = "linux", linux = "debian", debian = 13 }
"#,
        )
        .unwrap();
        assert!(m.remote.is_none());
    }
}
