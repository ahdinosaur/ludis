# lusid-secrets

Age-encrypted project secrets for lusid plans. Agenix-style flow: secrets
live as ciphertext in-repo, the operator's identity decrypts them at apply
time, and plaintext only ever reaches the target filesystem through
`@core/secret`'s atomic write.

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
            plan refers to secret by name  -->  @core/secret { name, path, ... }
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

Plans refer to secrets by name via `@core/secret`:

```rimu
- module: "@core/secret"
  params:
    name: "api_token"          # -> secrets/api_token.age
    path: "/etc/myapp/token"
    mode: 384                   # optional; default 0o600
    user: "myapp"               # optional
    group: "myapp"              # optional
```

`@core/secret` delegates to `@core/file`'s state/change/operation machinery,
adding a `FileSource::Secret(name)` variant that resolves against
`ctx.secrets()` inside the apply-time operation. The plaintext copy lives
only for the duration of one atomic write. Plans never see plaintext —
`ctx.secrets` is not exposed to Rimu.

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
empty bundle. Plans referencing `@core/secret` will fail later with a
missing-secret error.

Callers then wrap the result with `secrets.redactor()` (for per-operation
output scrubbing) and hand the bundle to `ctx.set_secrets(...)` before
planning.

## Per-target re-encryption

Two flavours, depending on whether the target is declared in
`lusid-secrets.toml`:

- **`reencrypt_all(host_identity, secrets_dir, recipient_pubkey)`** —
  walks `secrets_dir` and re-encrypts every `*.age` to `recipient_pubkey`
  alone. Used by `dev apply`: VM keypair is ephemeral and not in
  `[machines]`, so we trust the operator to test their plan with every
  secret.
- **`reencrypt_for_machine(host_identity, secrets_dir, machine_id)`** —
  looks `machine_id` up in `[machines]`, computes the file set via
  `files_for_alias(machine_id)` from `[files]`, decrypts each, re-encrypts
  to the machine's key alone. Used by `remote apply`: long-lived target
  declared in the toml only gets the secrets it's listed for.

Callers SFTP the resulting bundle to the guest and run `lusid-apply
--guest-mode --identity=<guest identity>` there.

- **Operator identity never leaves the host.** The guest only ever holds
  ciphertext encrypted to its own key, plus the identity file it decrypts
  them with.
- **Multi-operator caveat (both flavours).** Both functions decrypt with
  the operator's identity first; if the running operator isn't a
  recipient on a file the function tries to re-encrypt, decryption fails
  on that file. Fine in the implicit-operators schema (every operator can
  decrypt every file by definition); revisit if scoped operator access
  ever lands.

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
| `ls`           | no             | List `*.age` files and their *effective* recipients: `<operators>  +  <listed machines>`. The `+` and machines column are omitted when no machines are listed. |
| `cat <name>`   | yes            | Decrypt to stdout.                                                          |
| `edit <name>`  | yes            | Decrypt into a mode-0600 tmpfile in `$XDG_RUNTIME_DIR`, `$EDITOR`, re-encrypt on save. Tmpfile is scrubbed even on editor failure. |
| `rekey [name]` | yes            | Re-encrypt to the current recipient list. No-op when the header already matches. Without `<name>`, rekeys every `[files]` entry. |
| `keygen [-o]`  | no             | Generate an x25519 identity at `$XDG_CONFIG_HOME/lusid/identity` (or `$HOME/.config/lusid/identity`). Refuses to overwrite. |
| `check`        | no             | Audit `secrets/` against `lusid-secrets.toml`: orphan ciphertexts, missing ciphertexts, recipient drift. Non-zero exit on any finding; suits CI. |

## Invariants

- **Plans never see plaintext.** `@core/secret` is the only path from
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
