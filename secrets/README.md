# lusid-secrets

Age-encrypted project secrets, agenix-style: ciphertext lives in-repo, the operator's identity decrypts at apply time, and `@resource/secret` is the only path to plaintext on the target.

## Flow

```
                              lusid-secrets.toml
                              [operators] [machines] [groups] [files]
                                     |
                                     v
 host identity  -->  Secrets::load(secrets_dir, identity_path, guest_mode)
 (x25519 / ssh)                     |
                                    v
                    Secrets { HashMap<String, SecretBox<String>> }
                                    |
                                    v
                          Context::set_secrets(...)
                                    |
                                    v
            plan refers to secret by name  -->  @resource/secret { name, path, ... }
                                    |
                                    v
                    FileSource::Secret(name), atomic-write
```

## Threat model

What `@resource/secret` defends against — and what it doesn't.

**Defends against:**

- **Same-host non-root processes.** Default mode is `0o600`. A compromised low-privileged service account can't `cat` the secret.
- **Reading the repo without an identity.** Everything in `secrets/` is ciphertext. Without an `[operators]` or `[machines]` identity, the repo yields nothing.
- **Wire interception.** Bundles shipped to dev VMs / remote targets are re-encrypted to the destination's key alone; plaintext never crosses a wire.
- **Operator iteration in dev.** `dev apply --machine X` scopes the bundle to exactly what `remote apply --machine X` would ship — no "dev sees more" privilege expansion.

**Does NOT defend against:**

- **Stolen disk / removed SD card.** Mount the rootfs on a peer and mode bits don't matter. Mitigation: full-disk encryption, or write plaintext to a `/run/...` (tmpfs) path.
- **Backups.** `rsync -a` / `restic` / `borg` running as root copy plaintext verbatim. Mitigation: encrypt the backup destination, exclude the plaintext path, or use a tmpfs path.
- **Other root-equivalent processes.** Mode `0o600` doesn't beat root.

**Operator-side risks:**

- The operator's identity (`~/.config/lusid/identity`) is the trust root; losing it = full plaintext access to every project secret. Treat it like an SSH private key.
- `lusid secrets cat` writes plaintext to stdout. Terminal scrollback, `script(1)`, and shell history capture it. Avoid in shared sessions.
- `--params` JSON ends up in `/proc/<pid>/cmdline` of `lusid-apply`. **Don't put secret values in `lusid.toml`'s `params`** — they leak via `ps` to any UID on the host.

## Invariants

- **Plans never see plaintext.** `@resource/secret` is the only path from ciphertext to filesystem.
- **Plaintext lives in memory only during apply.** Wrapped in `SecretBox<String>` (redacted `Debug`, zeroised on drop); the target filesystem is the only disk location plaintext reaches, via an atomic write.
- **Selective decryption in host mode.** Only the files the matched alias is a recipient for are opened. Operators are implicit recipients on every file; machines see only the files that name them. Guest mode relies on upstream filtering.
- **UTF-8 only.** Non-UTF-8 payloads error at decrypt (`DecryptError::NotUtf8`).
- **Missing secrets are fatal**, not a silent empty file — both at state-probe time (`FileStateError::MissingSecret`) and apply time (`FileApplyError::MissingSecret`).

## Data model

Project layout:

```
<root>/
  secrets/
    lusid-secrets.toml
    api_token.age
    db_password.age
```

`lusid-secrets.toml`:

```toml
[operators]
mikey = "age1..."                     # x25519 public key

[machines]
rpi4b-1 = "ssh-ed25519 AAAA..."       # SSH host key

[groups]
prod = ["rpi4b-1"]                    # machine groups only

[files]
"api_token"   = { recipients = ["@prod"] }     # effective: mikey, rpi4b-1
"db_password" = { recipients = ["rpi4b-1"] }   # effective: mikey, rpi4b-1
"admin_only"  = { recipients = [] }            # effective: mikey
```

### Schema rules

- **Operators are implicit recipients on every file.** Listing an operator under `[files].recipients` or `[groups]` is a load error.
- `[machines]` and `[operators]` share one alias namespace; collisions are rejected at load.
- `[groups]` members must be machine aliases. Empty groups are rejected.
- `@name` in `[files].recipients` expands via `[groups]`; expansion is shallow (no nested groups).
- `[files].recipients` may be `[]` — yields an operator-only secret. No operators *and* no recipients is rejected per file (`EmptyEffectiveRecipients`).
- Duplicate pubkey values across `[operators]` ∪ `[machines]` are rejected.

### Identities

- `AGE-SECRET-KEY-1...` — age x25519.
- `-----BEGIN OPENSSH PRIVATE KEY-----` — OpenSSH ed25519 or RSA. Passphrase-protected keys are rejected up-front (prompting at apply time is out of scope).

### Drift behaviour

- Adding an operator causes drift on every `*.age` (the new stanza isn't in existing headers). `lusid secrets check` flags every file; `rekey` rewrites them.
- Removing an operator does **not** revoke their access to existing ciphertexts — their key material is still in each header. Run `rekey` to re-encrypt without them.
- Adding/removing/swapping a machine in `[machines]` is symmetric: drift on every file the machine is a recipient of, until `rekey`.
- Moving an alias between `[operators]` and `[machines]` triggers drift on every file the alias was on. Rekey resolves.

## Plan integration

Plans refer to secrets by name via `@resource/secret`:

```rimu
- module: "@resource/secret"
  params:
    name: "api_token"           # -> secrets/api_token.age on the host
    path: "/etc/myapp/token"    # where the plaintext lands on the target
    mode: 0o600                 # optional; default 0o600
    user: "myapp"               # optional
    group: "myapp"              # optional
```

Prefer a `/run/...` path (tmpfs on every distro lusid targets) when the consumer doesn't need plaintext to survive reboots — that keeps bytes out of backups and off persistent disk. The parent directory must exist; declare it with `@resource/directory` if needed.

`@resource/secret` delegates to `@resource/file`'s machinery, with a `FileSource::Secret(name)` variant that resolves against `ctx.secrets()` inside the apply-time operation. The plaintext copy lives only for the duration of one atomic write. `ctx.secrets` is not exposed to Rimu.

## Apply-time decryption

`Secrets::load(secrets_dir, identity_path, guest_mode)` behaves one of three ways:

- **Host mode** (`identity_path = Some`, `guest_mode = false`) — the normal `lusid-apply` path: reads `lusid-secrets.toml`, matches the identity against `[operators]` / `[machines]` (no match is fatal), and decrypts only the files the matched alias is a recipient for.
- **Guest mode** (`identity_path = Some`, `guest_mode = true`) — used by `dev apply` / `remote apply` targets: skips `lusid-secrets.toml` and decrypts every `*.age` with the supplied identity. The host has already filtered the bundle via per-target re-encryption.
- **No identity** (`identity_path = None`) — returns an empty bundle. Plans referencing `@resource/secret` fail later with a missing-secret error.

## Per-target re-encryption

`reencrypt_for_target(host_identity, secrets_dir, machine_id, target_pubkey)` scopes the bundle to what `machine_id` is a recipient of, decrypts with the operator identity, and re-encrypts to `target_pubkey` alone.

Two callers:

- `remote apply` — target IS the declared machine; caller passes `machine_id`'s own key from `[machines]`.
- `dev apply` — target SHADOWS the declared machine (ephemeral VM keypair). Caller passes `machine_id` for scoping and the VM's pubkey as the recipient.

Unknown machines yield `UnknownMachine`; machines with no `[files]` entries yield `Ok(vec![])`. Both warn-log; call sites degrade gracefully so partial configs still apply and typo'd `--machine` values surface.

Callers SFTP the bundle to the guest and run `lusid-apply --guest-mode --identity=<guest identity>` there.

- **Operator identity never leaves the host.** The guest holds only ciphertext encrypted to its own key, plus the identity to decrypt it.
- **Dev = production scope.** `dev apply --machine X` ships exactly what `remote apply --machine X` would ship.
- **Multi-operator caveat.** Decryption uses the operator's identity; in the implicit-operators schema every operator can decrypt every file, so this is fine. Revisit if scoped operator access ever lands.

## Redactor

`Secrets::redactor()` builds a `Redactor` that substring-replaces every decrypted plaintext with `<redacted>`. `lusid-apply` wraps every per-operation stdout/stderr line through it before streaming to the TUI.

Limitations:

- Substring-only. Base64-encoded, JSON-escaped, or boundary-split secrets are not caught.
- Secrets shorter than `REDACT_MIN_LEN` (8 bytes) are skipped, to avoid false positives on common short sequences.
- Longest-first ordering handles nested matches but not the interleaved case where two secrets share a suffix with a prefix.

## CLI

`lusid secrets <subcommand>`:

| Command        | Needs identity | Action                                                                      |
| -------------- | :------------: | --------------------------------------------------------------------------- |
| `ls`           | no             | List `*.age` files and their effective recipients.                          |
| `cat <name>`   | yes            | Decrypt to stdout.                                                          |
| `edit <name>`  | yes            | Decrypt into a mode-0600 tmpfile in `$XDG_RUNTIME_DIR`, `$EDITOR`, re-encrypt on save. Tmpfile is scrubbed even on editor failure. |
| `rekey [name]` | yes            | Re-encrypt to the current recipient list. No-op when the header already matches. Without `<name>`, rekeys every `[files]` entry. |
| `keygen [-o]`  | no             | Generate an x25519 identity at `$XDG_CONFIG_HOME/lusid/identity` (or `$HOME/.config/lusid/identity`). Refuses to overwrite. |
| `check`        | no             | Audit `secrets/` against `lusid-secrets.toml`: orphan ciphertexts, missing ciphertexts, recipient drift. Non-zero exit on any finding; suits CI. |

`ls` orders recipients as: operators first, then machines in first-mention order through the file's recipients list, with `@group` refs expanded.
