//! QEMU-based VM provisioning for local development of lusid plans.
//!
//! Given a [`Machine`](lusid_machine::Machine) spec (OS, arch, hostname) and an
//! instance id, [`Vm::run`] boots a UEFI Linux guest under QEMU and returns a
//! [`Vm`] whose `ssh_port` is reachable on `127.0.0.1`. From there the caller
//! drives it via [`lusid_ssh`].
//!
//! ## Lifecycle
//!
//! 1. **Setup** (first run only) - download + hash-validate the guest image
//!    ([`image`]), build a qcow2 overlay on top of it, convert OVMF UEFI vars to
//!    qcow2, extract the kernel/initrd with `virt-get-kernel`, mint an ed25519
//!    SSH keypair, and produce a cloud-init ISO seeding hostname + authorized
//!    key + `openssh` package.
//! 2. **Save** - serialize [`Vm`] to `<instance_dir>/state.json`.
//! 3. **Start** - spawn `qemu-system-*` daemonized, with the overlay and
//!    cloud-init ISO as virtio drives, UEFI pflash, a QMP socket, and a
//!    user-mode NIC forwarding the guest's port 22 to a freshly-picked host
//!    port (plus any caller-supplied [`VmPort`]s).
//! 4. **Wait** - poll `127.0.0.1:<ssh_port>` until SSH answers.
//! 5. **Stop** - `SIGKILL` the qemu pid stored in `<instance_dir>/qemu.pid`.
//!
//! Subsequent runs with the same `instance_id` skip setup and only re-`start`
//! if qemu isn't already running.
//!
//! ## Dependencies (must be on `PATH`)
//!
//! `qemu-system-x86_64`, `qemu-system-aarch64`, `qemu-img`, `virt-get-kernel`
//! (libguestfs), `mkisofs` (genisoimage). See the crate README for install
//! instructions.

mod context;
mod image;
mod instance;
mod paths;
mod qemu;
mod utils;

pub use instance::{Vm, VmError, VmOptions};
