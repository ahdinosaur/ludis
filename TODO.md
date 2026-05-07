# Lusid: bigger-than-a-commit follow-ups

Design sketches for items raised by the multi-agent security review of
secret handling. Each entry: *what*, *why it didn't land in the
follow-up commits*, *one viable design*, *cost / open questions*. None
of these are urgent for the personal-infra audience; they're listed in
rough priority order so future work has somewhere to start.

---

## 1. Drop the secrets bundle from `Context` mid-pipeline

**What.** Today `lusid-apply::apply` calls `Secrets::load(...)` once,
hands the bundle to `Context::set_secrets(...)`, and the bundle lives
on `Context` for the entire apply (planning → state probe → change →
operations → … → emit). The bundle is only *needed* during the
operations layer where `@core/secret`'s `FileOperation::Write`
materialises plaintext. Everywhere else, plaintext sits in memory
unused — visible via `/proc/<pid>/maps` to any peer root, in the
process's heap snapshot, in core dumps, and (worst case) in pages
swapped out before the apply finishes.

**Why not now.** `Context` is plumbed through every phase and the
borrow checker doesn't make "release this resource at a phase
boundary" trivial. Touching the apply pipeline shape is bigger than
the security-fix scope of this round.

**Design sketch.**

- Split `Context` into `Context` (everything else) and
  `SecretsContext` (the bundle + redactor), composed by the apply
  driver.
- `apply()` keeps `SecretsContext` in a local; passes it into the
  operations phase by reference; drops it as soon as the operation
  futures resolve.
- The bundle's `Drop` (already wired via `SecretBox`/`Zeroize`) clears
  plaintext immediately on phase exit instead of at process exit.
- For the `Redactor`: its lifetime needs to span at least the stdout
  streaming for any operation that might emit stale plaintext. Either
  clone the redactor into the streaming task (it owns `Arc`s, cheap)
  or keep it on `Context` separately from the bundle.

**Cost / risk.** Touches `lusid-ctx`, `lusid-apply`, every
`OperationType` impl that pulls from `ctx.secrets()`. Risk: dropping
too early breaks future `@core/secret` operations that haven't run
yet. Need to hold the bundle until the **last** secret-touching op
finishes, which means examining the operation queue ahead of time.

**Win.** Memory window for plaintext shrinks from "whole apply" to
"the seconds the operations phase runs." Material against
`/proc/<pid>/mem` and core-dump exposure.

---

## 2. Zeroise the `Vec<u8>` plaintext copy in `FileOperation::Write`

**What.** In `operation/src/operations/file.rs:243`,
`FileSource::Secret` resolves to `WriteSource::SecretBytes(secret.expose_secret().as_bytes().to_vec())`.
The `Vec<u8>` is a plain heap allocation — when the future drops, the
bytes are deallocated but **not** overwritten. They linger in freed
heap until the allocator reuses the pages.

**Why not now.** `WriteSource::SecretBytes` is held inside an `async
move` block whose `Drop` runs after the atomic-write completes. To
zeroise, we'd need either:
- A custom wrapper type implementing `Zeroize` + `Drop`.
- Or `Zeroizing<Vec<u8>>` (already available via `secrecy::zeroize`).

The `Zeroizing` wrapper is one-line, but the call site touches the
`WriteSource` enum and its match arms — small ripple, didn't fit in
the focused security-fix batch.

**Design sketch.**

```rust
enum WriteSource {
    Bytes(Vec<u8>),
    SecretBytes(Zeroizing<Vec<u8>>),
    Copy(FilePath),
}
```

Apply path: `fs::write_file_atomic_with_initial_mode(path, &bytes, ...)`
already takes `&[u8]`, so the inner `Vec` deref via `Zeroizing`'s
`Deref` works without other changes. On future drop, `Zeroizing`'s
`Drop` overwrites the bytes before deallocation.

**Cost / risk.** ~10-line patch in `operation/src/operations/file.rs`.
No public API change. Defensive only — won't show up in any test, and
the secret was already in a `SecretBox` upstream, so the *actual* leak
window is microseconds. Worth doing for completeness, not urgent.

---

## 3. systemd-creds passthrough as a first-class `@core/systemd-credential`

**What.** systemd-creds gives per-service kernel-isolated credential
delivery (mode 0700 tmpfs, restricted by UID, optionally TPM-sealed)
via `LoadCredentialEncrypted=`. For systemd-managed consumers it's
strictly stronger than lusid's tmpfs default.

**Why not now.** Locks lusid to systemd ≥ 250, applies only to
systemd-managed consumers (no help for cron jobs, static configs,
`ssh_host_*_key` provisioning). Reasonable as an *opt-in* alongside
the existing `@core/secret`, not a replacement.

**Design sketch.**

A new resource `@core/systemd-credential`:

```yaml
- module: "@core/systemd-credential"
  params:
    name: "api_token"        # the *.age stem
    unit: "myapp.service"    # the consumer
    credential: "API_TOKEN"  # the LoadCredentialEncrypted= name
    tpm2: true               # optional, default false
```

Implementation:

1. Decrypt the named secret on the host (or guest, in apply context).
2. Re-encrypt via `systemd-creds encrypt --name=<credential> [--tpm2-pcrs=...]`
   into `/etc/credstore.encrypted/<unit>.<credential>.cred`.
3. Patch the consumer unit (or drop-in) with
   `LoadCredentialEncrypted=<credential>:<credential>.cred`.
4. Trigger a `daemon-reload` + `restart <unit>` if the credential
   changed.

systemd reads from `/run/credentials/@<unit>/` at unit start;
plaintext lives in tmpfs under that path, owned by the unit's User=,
unreadable by other UIDs.

**Cost / open questions.**
- TPM-sealed credentials are host-bound — re-encryption must happen on
  the target. Can't pre-build in the repo.
- Need to detect systemd version and capabilities at apply time.
- Interaction with the existing `@core/systemd` resource: probably
  emit drop-ins rather than mutate the unit file directly.
- Docs need to explain when to use this vs `@core/secret`.

---

## 4. `--params` JSON over an FD instead of `argv[]`

**What.** The CLI today does `lusid-apply --params <json>`, leaving
the JSON in `/proc/<pid>/cmdline` for the lifetime of the apply. We
warn against putting secrets in params (commit `<this-batch>` adds
the warning), but it's a footgun — operators reach for the simplest
mechanism and don't always read the docs.

**Why not now.** Touches the CLI surface and `lusid-apply` argument
parsing; affects the dev/remote SSH command construction (argv
serialisation). Wanted to keep the security-fix batch tight.

**Design sketch.**

- Add `--params-fd <N>`: read JSON from file descriptor `N` instead of
  inline.
- Add `--params-file <path>`: read JSON from a file. Less safe (path
  might land on persistent disk), but useful for `lusid local apply`
  where there's no SSH wrapper to set up an FD.
- The `lusid` CLI wraps `lusid-apply` invocations; for local it can
  pipe params over a unix pipe and pass the FD; for remote/dev (via
  SSH), the simplest is to write params to a tmpfs file inside the
  guest's working dir and pass `--params-file`. Cleanup on apply
  exit.
- Keep `--params <json>` as the deprecated form (or remove it
  outright — it predates `lusid local apply` having any users).

**Cost / open questions.**
- SSH doesn't trivially forward FDs; the `--params-file` route is
  required for remote/dev unless we want to invent a side-channel.
- Validation: does the JSON pass cleanly through whatever transport
  we choose?
- Migration: any existing `params = "..."` in `lusid.toml` keeps
  working; the change is internal to how the CLI passes them on.

---

## 5. Better `Redactor`: handle base64/JSON-escaped/short secrets

**What.** Current redactor (`secrets/src/redactor.rs`) is substring-only:

- Skips secrets shorter than 8 bytes (avoids false positives, but a
  legitimately-short secret leaks through).
- Doesn't catch base64- or JSON-escaped versions of the secret.
- Doesn't handle interleaved suffix/prefix overlap between two
  secrets that share characters.

**Why not now.** Each of these is a separate scanning pass with its
own perf and false-positive trade-offs. Worth doing as a focused
follow-up after some real-world apply output to inform priorities.

**Design sketch.**

- Compute base64 + JSON-escape variants of every secret at
  `Redactor::new` time and add them to the substring set.
- Lower the min-length to 4 with an escape hatch (per-secret
  `redact: false` opt-out) — false positives are surprising but
  recoverable; missed redactions are the real risk.
- Replace the linear-scan substring matcher with Aho-Corasick
  (already in the dep tree via `regex`) — gives multi-pattern,
  longest-match, and overlap-aware scanning in one pass.

**Cost / open questions.**
- Aho-Corasick adds a small build-time cost per `Redactor` (one-shot
  per apply) and modest memory.
- False positives: legitimate output containing a substring of a
  secret gets `<redacted>`. Edge case but real.

---

## 6. memfd-only delivery for credential FDs

**What.** Some consumers accept credentials via FD or environment
(systemd via `LoadCredential=`, `pass --clip`, anything taking an
`stdin` of secrets). For those, plaintext genuinely never names a
filesystem path: a `memfd_create(2)` returns an FD backed by anonymous
RAM with no directory entry, visible only via `/proc/<pid>/fd/<n>` to
peers with `CAP_SYS_PTRACE` or matching UID.

**Why not now.** Niche. Per-consumer integration; doesn't generalise
to the static-config-file shape `@core/secret` covers today.

**Design sketch.**

A new resource `@core/secret-fd`:

```yaml
- module: "@core/secret-fd"
  params:
    name: "api_token"
    consumer: ["my-cli", "--token-fd", "{fd}"]  # fd substituted
```

`{fd}` is replaced with the integer FD of the memfd by the
`@core/command` family at exec time.

**Cost / open questions.**
- Restricted to consumers that can take an FD argument or read fd 0.
- Lifecycle: how long does the memfd live? Until the consumer exits.
  What if it forks into a daemon?
- Probably better solved by `@core/systemd-credential` for the
  systemd-service case (item 3); this is for the long tail.

---

## Lower-priority items not yet sketched

- **Zeroise intermediate `Vec<u8>` / `String` in `decrypt_bytes`.**
  Trivial wrap with `Zeroizing<_>`; done together with item 2.
- **Wrap the SFTP `FileBytes` Vec carrying VM private PEM bytes in a
  zeroising wrapper.** Touches `lusid-ssh`'s `SshVolume` enum.
- **`lusid secrets check --strict`** to flag operator identity files
  outside `~/.config/lusid/`. Skipped from the recent batch by user
  preference.
- **Encrypted swap detection at apply time** — warn when the target
  has unencrypted swap and the plan declares persistent-disk secret
  paths. Cosmetic but useful.
