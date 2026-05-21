# Secrets

lusid's secrets system is age-encrypted, agenix-style: ciphertext lives in your repo, your SSH key decrypts it at apply time, and plaintext only reaches the target's filesystem through `@resource/secret`'s atomic write.

This page is a practical guide. For the full threat model, schema rules, and CLI reference, see the [secrets crate README](../../secrets/README.md).

## Setup

### 1. Use your existing SSH key

lusid uses `~/.ssh/id_ed25519` to decrypt project secrets. If you don't have one yet:

```sh
ssh-keygen -t ed25519
```

There is no separate lusid-managed key. The same private key authenticates your SSH sessions and decrypts your project secrets.

To override the default, pass `--identity <path>` or set `LUSID_IDENTITY`.

### 2. Declare yourself as an operator

Get your public key:

```sh
cat ~/.ssh/id_ed25519.pub
```

Create `secrets/lusid-secrets.toml` at your project root:

```toml
[operators]
mikey = "ssh-ed25519 AAAA... mikey@laptop"    # paste the line above

[machines]
# Add per-machine entries when you have remote targets.

[groups]
# Optional machine groups, referenced as @name in `[files].recipients`.

[files]
# Each *.age file gets an entry here.
```

The alias (`mikey`) is whatever you want to call yourself.

### 3. Add a machine recipient (optional)

For `remote apply`, list the target's SSH host key:

```toml
[machines]
web-a = "ssh-ed25519 AAAA..."   # /etc/ssh/ssh_host_ed25519_key.pub on the target
```

Get the pubkey from the target: `cat /etc/ssh/ssh_host_ed25519_key.pub`.

On apply, lusid reads the matching **private** key at `/etc/ssh/ssh_host_ed25519_key` on the target to decrypt. That file is root-only, so `remote apply` with a non-root SSH user needs passwordless sudo - see [apply modes](./apply-modes.md#remote).

### 4. Create a secret

```sh
lusid secrets edit api_token
```

Opens `$EDITOR` on a mode-`0600` tmpfile in `$XDG_RUNTIME_DIR`. Save and quit - lusid re-encrypts and writes `secrets/api_token.age`. The tmpfile is scrubbed even if your editor crashes.

You also need a `[files]` entry naming the recipients:

```toml
[files]
"api_token" = { recipients = ["web-a"] }
```

You (operator `mikey`) are an implicit recipient on every file. The `recipients` list is for *additional* recipients - machines that should be able to decrypt this on apply.

## Use in a plan

```yaml
- module: "@resource/secret"
  params:
    name: "api_token"           # → secrets/api_token.age
    path: "/etc/myapp/token"    # where plaintext lands
    mode: 0o600                 # optional; default 0o600
    user: "myapp"               # optional
    group: "myapp"              # optional
```

That's it. On apply, lusid decrypts `secrets/api_token.age`, writes the plaintext to `/etc/myapp/token` atomically with mode `0o600`, and zeroes the in-memory plaintext.

## Where plaintext lives

- **In your repo:** never. `secrets/` is ciphertext only.
- **On the operator's filesystem:** never (except briefly via `lusid secrets edit`).
- **On the wire to dev/remote targets:** never. Bundles are re-encrypted to the destination's key alone before being SFTP'd.
- **On the target:** at the `path` you declared, with the mode you set (default `0o600`).
- **In RAM during apply:** wrapped in a zeroising container, lifespan = one atomic write.

Prefer a `/run/...` path when the consumer doesn't need plaintext to survive reboots - `/run` is tmpfs on every distro lusid targets, so the bytes never touch disk and never end up in backups.

## CLI reference

See [`secrets <subcommand>`](../reference/cli.md#secrets-subcommand) in the CLI reference for every subcommand.

## Drift behaviour

When the recipient set changes (you add an operator, replace a machine's SSH key, etc.), existing `*.age` files don't update themselves. `lusid secrets check` flags every file that drifts; `rekey` rewrites them.

**Removing an operator doesn't revoke their access to existing ciphertexts** - their key material is still in the header. Always `rekey` after removing someone you actually want to lock out.

## Threat model - short version

`@resource/secret` defends against:

- Same-host non-root processes (mode `0o600` by default).
- Reading the repo without an identity (it's all ciphertext).
- Wire interception during `dev apply` / `remote apply`.

It does **not** defend against:

- Root on the target.
- Stolen disks / removed SD cards (use full-disk encryption, or write to a tmpfs path).
- Backups copying plaintext (exclude the path, or use tmpfs).
- Your SSH private key (`~/.ssh/id_ed25519`) leaking - it decrypts every project secret you have access to, in addition to the SSH access it already grants. Standard SSH-key hygiene applies.

See the [secrets crate README](../../secrets/README.md) for the full threat model and invariants.

## ⚠️ Don't put secrets in `params`

CLI `--params` JSON ends up in `/proc/<pid>/cmdline` of `lusid-apply`, visible to any UID on the target via `ps`. **Don't put secret values in `lusid.toml`'s `params`** - only in `secrets/*.age`, referenced by name from `@resource/secret`.
