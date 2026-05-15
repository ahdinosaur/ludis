# Apply modes

lusid has three ways to apply a plan, depending on where the target machine is.

## Local

Apply to the host you're sitting at.

```sh
lusid --config ./lusid.toml local apply
```

lusid picks the entry in `lusid.toml` whose `hostname` matches `$(hostname)`. If no entry matches, it errors out.

Use this for the machine in front of you — laptop dotfiles, dev workstation, the box you're SSH'd into for a one-off change.

## Dev VM

Boot a local QEMU VM matching the machine spec, copy the plan in, apply over SSH.

```sh
lusid --config ./lusid.toml dev apply --machine my-server
```

The first run downloads a cloud image for the machine's `arch` + `os` combination (around 400 MB for Debian, 700 MB for Arch); subsequent runs reuse it. The VM persists between runs — re-applying with the same `--machine` reuses the same VM.

Open an interactive shell on the VM:

```sh
lusid --config ./lusid.toml dev ssh --machine my-server
```

This is the iteration loop: change your plan, `dev apply`, see what happens, repeat — without touching real hardware.

### Resources / prerequisites

You need QEMU, libguestfs, and mkisofs installed locally — see [installation](../installation.md#for-dev-apply-local-qemu-vms).

## Remote

Apply over SSH to a real machine.

```toml
# lusid.toml
[machines.my-server]
hostname = "my-server"
arch = "x86-64"
os = { type = "linux", linux = "debian", debian = 13 }
plan = "./server.lusid"
remote = { host = "my-server.example.com" }
```

```sh
lusid --config ./lusid.toml remote apply --machine my-server
lusid --config ./lusid.toml remote ssh --machine my-server   # interactive shell
```

`remote` accepts:

- `host` (required) — the SSH target.
- `port` (default `22`).
- `user` (default `"root"`).
- `ssh_key` — path to the operator's SSH private key. Defaults to `~/.ssh/id_ed25519` if unset.

When `user` is not `root`, lusid wraps the remote `lusid-apply` invocation in `sudo -n`. The SSH user must have passwordless sudo configured — otherwise the apply blocks on a prompt and fails.

### Host-key verification

Trust-on-first-use against `~/.ssh/known_hosts`. The first connection pins whatever key the server presents (matching OpenSSH's `StrictHostKeyChecking=accept-new`). Subsequent connections refuse mismatches.

> **The first apply assumes the network path between you and the target is clean.** If there's any doubt — applying across the public internet, on captive Wi-Fi, to a fresh cloud VM — verify the target's `/etc/ssh/ssh_host_ed25519_key.pub` out-of-band and seed `~/.ssh/known_hosts` yourself before the first run.

## Which one when

| Use case | Mode |
| --- | --- |
| Editing your own laptop's config | `local apply` |
| Iterating on a plan before deploying it | `dev apply` |
| Configuring a real server (cloud, homelab, SBC) | `remote apply` |
| Quick shell on the target | `remote ssh` / `dev ssh` |
| One-off CI run that applies to a fresh ephemeral box | `remote apply` |
