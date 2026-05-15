# lusid-ssh

Async SSH client for lusid's VM provisioning pipeline.

Built on [`russh`]. The public surface is [`Ssh`]:

- `Ssh::connect` - retry/backoff on transient IO errors, then ed25519 public-key auth.
- `Ssh::command` - start a remote command; returns `SshCommandHandle` with stdout/stderr as [`tokio::io::AsyncRead`], stdin as `AsyncWrite`, and the exit code as an `async_promise::Promise`.
- `Ssh::sync` - SFTP upload (directory, file, or raw bytes). Symlinks are skipped with a `warn!`.
- `Ssh::terminal` - forward the current TTY (with `SIGWINCH` for window resize) to a remote interactive shell.
- `Ssh::disconnect` - clean channel teardown.

[`SshKeypair`] manages a local ed25519 keypair (`id_ed25519[.pub]`) - load from disk or generate and save.

## Host key verification

`NoCheckHandler` skips host key verification. This is intentional for the current use case: lusid connects only to VMs it has just booted. Revisit if SSHing into arbitrary remote machines becomes a use case.

## References

- [`hydro-project/async-ssh2-russh`](https://github.com/hydro-project/async-ssh2-russh), Apache-2.0 - original inspiration for the `AsyncSession` / `AsyncChannel` shape.
