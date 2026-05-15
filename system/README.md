# lusid-system

Runtime detection of the host machine.

`System` bundles hostname, CPU arch, OS (with distro + version on Linux), and current user - the same struct plans receive as the `system` argument to `setup(params, system)` (serialized through [`rimu-interop`](../rimu-interop)).

Contrast with [`lusid-machine`](../machine): `System` describes the *current* host; `Machine` describes the *target*.

Detection covers:

- **Arch**: `cfg(target_arch)` → `X86_64` / `Aarch64`.
- **OS**: On Linux, parses `/etc/os-release` via `etc-os-release`; recognises Ubuntu / Debian / Arch. Unknown distros error rather than silently defaulting.
- **Hostname**: via the `hostname` crate.
- **User**: `$USER` / `$HOME` on Unix, `$USERNAME` / `$USERPROFILE` on Windows.

Types are `#[non_exhaustive]` where variant growth is expected, so adding a new OS variant is non-breaking.
