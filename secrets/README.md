# lusid-secrets

Age-encrypted project secrets for lusid plans. Agenix-style flow: secrets
live as ciphertext in-repo, the operator's identity decrypts them at apply
time, and plaintext only ever reaches the target filesystem through
`@resource/secret`'s atomic write.

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

A `Redactor` hangs off the same `Secrets` bundle; every per-operation
stdout/stderr line is substring-scrubbed before emit.

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

- **Operators are implicit recipients on every `[files]` entry.** Don't
  list operator aliases under `[files].recipients` or in `[groups]` —
  both are load errors. The operator-implicit rule is the whole point:
  if you can decrypt, you can re-encrypt for any target.
- `[machines]` aliases and `[operators]` aliases share one namespace;
  collisions are rejected at load.
- `[groups]` members must be machine aliases. Empty groups are rejected.
- `@name` references in `[files].recipients` expand via `[groups]`;
  expansion is shallow (no nested groups).
- `[files].recipients` may be empty (`[]`) — yields an operator-only
  secret. The both-empty case (no operators *and* empty recipients) is
  rejected per file (`EmptyEffectiveRecipients`).
- Duplicate pubkey *values* across `[operators]` ∪ `[machines]` are
  rejected, with the diagnostic naming both alias kinds.

### Drift behaviour

- Adding an operator to `[operators]` causes drift on every `*.age` file
  (the new operator's stanza isn't in the existing headers). `lusid
  secrets check` flags every file; `rekey` rewrites them.
- Removing an operator does **not** revoke their access to existing
  ciphertexts — their key material is still in each header. Run `rekey`
  to re-encrypt without them.
- Adding/removing/swapping a machine in `[machines]` is symmetric: drift
  on every file the machine is a recipient of, until `rekey`.
- Moving an alias between `[operators]` and `[machines]` triggers drift
  on every file the alias was on. Rekey resolves.

Identities come in two shapes:

- `AGE-SECRET-KEY-1...` — age x25519.
- `-----BEGIN OPENSSH PRIVATE KEY-----` — OpenSSH ed25519 or RSA. Passphrase-
  protected keys are rejected up-front (prompting at apply time is out of scope).

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

Prefer a `/run/...` path (tmpfs on every distro lusid targets) when the
consumer doesn't need the plaintext to survive reboots — that keeps the
bytes out of backups and off persistent disk. The parent directory must
exist; declare it with `@resource/directory` if needed.

`@resource/secret` delegates to `@resource/file`'s state/change/operation
machinery, adding a `FileSource::Secret(name)` variant that resolves
against `ctx.secrets()` inside the apply-time operation. The plaintext
copy lives only for the duration of one atomic write. Plans never see
plaintext — `ctx.secrets` is not exposed to Rimu.

## Apply-time decryption

`Secrets::load(secrets_dir, identity_path, guest_mode)` is the single entry
point. Its behaviour depends on the two flags:

**Host mode** (`identity_path = Some`, `guest_mode = false`) — the normal
`lusid-apply` path: reads `lusid-secrets.toml`, matches the identity's public
key against `[operators]` / `[machines]` (no match is a hard error), and
decrypts only the files the matched alias is declared as a recipient for.

**Guest mode** (`identity_path = Some`, `guest_mode = true`) — used by `dev
apply` / `remote apply` targets: skips `lusid-secrets.toml` and decrypts
every `*.age` under `secrets_dir` with the supplied identity. The host has
already filtered the bundle to exactly what this guest should see via
per-target re-encryption, so no Recipients config is needed on the guest.

**No identity** (`identity_path = None`, `guest_mode = false`) — returns an
empty bundle. Plans referencing `@resource/secret` will fail later with a
missing-secret error.

Callers then wrap the result with `secrets.redactor()` (for per-operation
output scrubbing) and hand the bundle to `ctx.set_secrets(...)` before
planning.

## Per-target re-encryption

`reencrypt_for_target(host_identity, secrets_dir, machine_id, target_pubkey)`
scopes the bundle to what `machine_id` is declared a recipient of in
`[files]`, decrypts each with the operator identity, and re-encrypts to
`target_pubkey` alone.

Two callers:

- `remote apply` — target IS the declared machine; caller passes
  `machine_id`'s own key from `[machines]`.
- `dev apply` — target SHADOWS the declared machine (ephemeral VM
  keypair). Caller passes `machine_id` for `[files]` scoping and the
  VM's pubkey as the cryptographic recipient.

`Recipients::files_for_alias(machine_id)` produces the file list. A
machine that's in `[machines]` but on no `[files]` entry yields
`Ok(vec![])` (warn-logged). A machine that's not in `[machines]` at all
yields `UnknownMachine` — call sites typically degrade gracefully (no
secrets shipped, warn-logged) so a partially-configured project still
applies and typo'd `--machine` values surface.

Callers SFTP the resulting bundle to the guest and run `lusid-apply
--guest-mode --identity=<guest identity>` there.

- **Operator identity never leaves the host.** The guest only ever holds
  ciphertext encrypted to its own key, plus the identity file it decrypts
  them with.
- **Dev = production scope.** `dev apply --machine X` ships exactly what
  `remote apply --machine X` would ship — there's no "dev sees more"
  privilege expansion.
- **Multi-operator caveat.** Decryption uses the operator's identity
  first; if the running operator isn't a recipient on a file, decryption
  fails on that file. Fine in
  the implicit-operators schema (every operator can decrypt every file
  by definition); revisit if scoped operator access ever lands.

## Redactor

`Secrets::redactor()` builds a `Redactor` that substring-replaces every
decrypted plaintext with `<redacted>`. `lusid-apply` wraps every per-operation
stdout/stderr line through it before streaming to the TUI.

Limitations, read before trusting:

- Substring-only. Secrets that appear base64-encoded, JSON-escaped, or split
  across read boundaries are not caught.
- Secrets shorter than `REDACT_MIN_LEN` (8 bytes) are skipped, to avoid false
  positives on common short sequences.
- Longest-first ordering handles nested matches (one secret is a substring of
  another) but not the interleaved case where two secrets share a suffix
  with a prefix.

## CLI

`lusid secrets <subcommand>`:

| Command        | Needs identity | Action                                                                      |
| -------------- | :------------: | --------------------------------------------------------------------------- |
| `ls`           | no             | List `*.age` files and their *effective* recipients (operators always come first, then machines in first-mention order through the file's recipients list, `@group` refs expanded). |
| `cat <name>`   | yes            | Decrypt to stdout.                                                          |
| `edit <name>`  | yes            | Decrypt into a mode-0600 tmpfile in `$XDG_RUNTIME_DIR`, `$EDITOR`, re-encrypt on save. Tmpfile is scrubbed even on editor failure. |
| `rekey [name]` | yes            | Re-encrypt to the current recipient list. No-op when the header already matches. Without `<name>`, rekeys every `[files]` entry. |
| `keygen [-o]`  | no             | Generate an x25519 identity at `$XDG_CONFIG_HOME/lusid/identity` (or `$HOME/.config/lusid/identity`). Refuses to overwrite. |
| `check`        | no             | Audit `secrets/` against `lusid-secrets.toml`: orphan ciphertexts, missing ciphertexts, recipient drift. Non-zero exit on any finding; suits CI. |

## Threat model

What `@resource/secret` defends against — and what it doesn't.

**Defends against:**

- **Same-host non-root processes.** Default mode is `0o600`. A
  compromised low-privileged service account can't `cat` the secret.
- **Reading the repo without an identity.** Everything in `secrets/` is
  ciphertext. Without an `[operators]` or `[machines]` identity, the repo
  yields nothing.
- **Wire interception.** Re-encrypted bundles shipped to dev VMs / remote
  targets are encrypted to the destination's key alone; plaintext never
  crosses a wire.
- **Operator iteration in dev.** `dev apply --machine X` scopes the
  bundle to exactly what `remote apply --machine X` would ship — the dev
  VM doesn't see secrets the production target wouldn't see.

**Does NOT defend against:**

- **Stolen disk / removed SD card.** Mount the rootfs on a peer and mode
  bits don't matter. Mitigation: full-disk encryption, or write the
  plaintext to a `/run/...` (tmpfs) path so it never lives on disk.
- **Backups.** `rsync -a` / `restic` / `borg` running as root copy
  plaintext bytes verbatim. Mitigation: encrypt the backup destination,
  exclude the plaintext path, or use a tmpfs path.
- **Other root-equivalent processes.** Mode `0o600` doesn't beat root.

**Operator-side risks:**

- The operator's identity (`~/.config/lusid/identity`) is the trust root;
  losing it = full plaintext access to every project secret. Treat it
  like an SSH private key.
- `lusid secrets cat` writes plaintext to stdout. Terminal scrollback,
  `script(1)`, and shell history captures it. Avoid in shared sessions.
- `--params` JSON ends up in `/proc/<pid>/cmdline` of `lusid-apply`.
  **Don't put secret values in `lusid.toml`'s `params`** — they leak via
  `ps` to any UID on the host.

## Invariants

- **Plans never see plaintext.** `@resource/secret` is the only path from
  ciphertext to filesystem.
- **Plaintext lives in memory only during apply.** Wrapped in
  `SecretBox<String>` (redacted `Debug`, zeroised on drop); the target
  filesystem is the only disk location plaintext reaches, via an atomic write.
- **Selective decryption in host mode.** Only the files the alias is a
  recipient for are opened. For an operator alias, that's every file
  in `[files]` (operators are implicit recipients on all of them);
  for a machine alias, only the files that name it. Guest mode relies on
  upstream filtering.
- **UTF-8 only.** Non-UTF-8 payloads error loudly at decrypt
  (`DecryptError::NotUtf8`). Binary support is a later change.
- **Missing secrets are fatal**, not a silent empty file — both at state-probe
  time (`FileStateError::MissingSecret`) and apply time
  (`FileApplyError::MissingSecret`).
