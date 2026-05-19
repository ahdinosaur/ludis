use std::fmt::{Debug, Write};
use std::{ffi::OsStr, net::Ipv4Addr, path::Path};
use thiserror::Error;
use tokio::process::{Child, Command};

use crate::instance::VmPort;

#[derive(Error, Debug)]
pub enum QemuError {
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

pub struct Qemu {
    command: Command,
}

impl Debug for Qemu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.command.fmt(f)
    }
}

impl Qemu {
    /// Create a new emulator for a specific QEMU binary path.
    pub fn new<S: AsRef<OsStr>>(qemu_binary: S) -> Qemu {
        let command = Command::new(qemu_binary);
        Qemu { command }
    }

    pub fn easy(&mut self) -> &mut Self {
        // Disable HPET to decrease idle CPU usage: -machine hpet=off
        self.command.args(["-machine", "hpet=off"]);

        // Enable virtio balloon with free-page-reporting.
        self.command
            .args(["-device", "virtio-balloon,free-page-reporting=on"]);

        self
    }

    // Enable KVM accelerator.
    pub fn kvm(&mut self, enabled: bool) -> &mut Self {
        if enabled {
            self.command.args(["-accel", "kvm"]).args(["-cpu", "host"]);
        }
        self
    }

    /// Set env var for QEMU process.
    #[allow(dead_code)]
    pub fn env(&mut self, k: &str, v: &str) -> &mut Self {
        self.command.env(k, v);
        self
    }

    /// Set CPU count: -smp <n>
    pub fn cpu_count(&mut self, cpus: impl ToString) -> &mut Self {
        self.command.args(["-smp", &cpus.to_string()]);
        self
    }

    /// Configure memory: -m <GB> and memfd NUMA backend for that size.
    pub fn memory(&mut self, memory_in_gb: u64) -> &mut Self {
        self.command
            .args(["-m", &format!("{memory_in_gb}G")])
            .args([
                "-object",
                &format!("memory-backend-memfd,id=mem0,merge=on,share=on,size={memory_in_gb}G"),
            ])
            .args(["-numa", "node,memdev=mem0"]);

        self
    }

    /// Add a virtio drive with explicit node name, format and file path.
    pub fn virtio_drive(&mut self, node_name: &str, format: &str, file: &Path) -> &mut Self {
        let file = file.display();
        self.command.args([
            "-drive",
            &format!("if=virtio,node-name={node_name},format={format},file={file}"),
        ]);

        self
    }

    /// Add UEFI pflash code and vars drives.
    pub fn plash_drives(&mut self, code_path: &Path, vars_path: &Path) -> &mut Self {
        let code_path = code_path.display();
        let vars_path = vars_path.display();
        self.command
            .args([
                "-drive",
                &format!("if=pflash,format=raw,unit=0,file={code_path},readonly=on",),
            ])
            .args([
                "-drive",
                &format!("if=pflash,format=qcow2,unit=1,file={vars_path}"),
            ]);

        self
    }

    /// QMP over UNIX socket.
    pub fn qmp_socket(&mut self, qmp_socket_path: &Path) -> &mut Self {
        let qmp_socket_path = qmp_socket_path.display();
        self.command
            .args(["-qmp", &format!("unix:{qmp_socket_path},server,wait=off")]);
        self
    }

    /// Add user-mode NIC with model 'virtio' and hostfwd rules based on VmPort.
    pub fn ports(&mut self, ports: &[VmPort]) -> &mut Self {
        let hostfwd: String = ports.iter().fold(String::new(), |mut s, p| {
            let _ = write!(
                s,
                ",hostfwd=:{}:{}-:{}",
                p.host_ip.unwrap_or(Ipv4Addr::UNSPECIFIED),
                p.host_port.unwrap_or(p.vm_port),
                p.vm_port
            );
            s
        });
        self.command
            .args(["-nic", &format!("user,model=virtio{hostfwd}")]);

        self
    }

    pub fn pid_file<P: AsRef<Path>>(&mut self, path: P) -> &mut Self {
        self.command.arg("-pidfile").arg(path.as_ref());
        self
    }

    pub fn graphics(&mut self, enabled: bool) -> &mut Self {
        if !enabled {
            // `-display none` instead of `-nographic`: the latter redirects serial
            // to stdio and conflicts with `-daemonize`.
            self.command.args(["-display", "none"]);
        }
        self
    }

    pub async fn spawn(self) -> Result<Child, QemuError> {
        let mut command = self.command;

        command.arg("-daemonize");

        let child = command.spawn()?;

        Ok(child)
    }
}
