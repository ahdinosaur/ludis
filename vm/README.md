# lusid-vm

Local QEMU VMs for developing lusid plans before deploying to real hardware.

[`Vm::run`] takes a [`Machine`] spec + instance id and returns a running VM
reachable over SSH at `127.0.0.1:<ssh_port>`. Behind that single call:

1. **Image** ([`image/`](src/image)) - look the machine's `(arch, os)` up in
   the compiled-in [`images.toml`](images.toml); download + SHA-validate the
   qcow2 and sums file into `cache_dir/vm/images/`.
2. **Setup** ([`instance/setup/`](src/instance/setup)) - create `overlay.qcow2`
   backed by the cached image, copy OVMF UEFI vars into a per-VM qcow2,
   extract `vmlinuz` (and `initrd.img` if present) with `virt-get-kernel`,
   mint an ed25519 SSH keypair, and produce a cloud-init seed ISO that
   injects the hostname, the public key, and `openssh` at first boot.
3. **Save** - serialize [`Vm`] to `<instance_dir>/state.json` so future calls
   with the same `instance_id` skip setup.
4. **Start** ([`instance/start.rs`](src/instance/start.rs)) - assemble and
   daemonize `qemu-system-<arch>` via [`qemu/mod.rs`](src/qemu/mod.rs):
   UEFI pflash, overlay + cloud-init virtio drives, QMP socket, KVM +
   `-cpu host`, user-mode NIC with hostfwd for `ssh_port → 22` plus any
   caller-supplied [`VmPort`]s.
5. **Wait** - poll `127.0.0.1:<ssh_port>` until SSH accepts TCP.

[`Vm::stop`] kills qemu via `SIGKILL` on the pid in `qemu.pid`; [`Vm::remove`]
deletes the whole instance directory.

## File layout

- Cached, shared across instances: `<cache_dir>/vm/images/{arch}_{os}.{qcow2,shaNsums}`
- Per-instance, disposable: `<data_dir>/vm/instances/<id>/` - see
  [`instance/paths.rs`](src/instance/paths.rs) for the full list.

## Dependencies

External binaries looked up on `PATH` at context init (missing any fails
fast with a clear error - see [`ExecutablePaths`](src/paths.rs)):

- `qemu-system-x86_64`, `qemu-system-aarch64`, `qemu-img` (qemu)
- `virt-get-kernel` (libguestfs)
- `mkisofs` (genisoimage)

OVMF firmware is read from `/usr/share/OVMF/OVMF_{CODE,VARS}_4M.fd`.

### Debian

```shell
sudo apt install qemu-system ovmf libguestfs-tools genisoimage
sudo usermod -aG kvm $USER
```

## References

- [`cubic-vm/cubic`](https://github.com/cubic-vm/cubic), MIT / Apache-2.0, Copyright (c) 2025 Roger Knecht
- [`archlinux/vmexec`](https://gitlab.archlinux.org/archlinux/vmexec), MIT, Copyright (c) 2025 Sven-Hendrik Haase - source of the `virt-get-kernel` and OVMF-vars-to-qcow2 recipes (cited inline in the setup modules).
