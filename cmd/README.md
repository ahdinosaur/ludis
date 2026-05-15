# lusid-cmd

Thin wrapper around `tokio::process::Command` used by lusid operations.

- **Stdio routing.** `stdout(bool)` / `stderr(bool)` flip between piped (captured) and inherited (streamed to parent).
- **`sudo()`.** Rewraps as `sudo -n <cmd>`, forwarding explicitly-set env vars and the working dir. The `-n` ensures non-interactive failure rather than a blocked password prompt.
- **`handle()`.** Returns the exit status as a value, for commands where a non-zero exit is meaningful (e.g. an `is_installed` probe).
- **`FromStr` via `shell-words`.** Plan authors write command strings; lusid parses them into program + args.
- **`new_sh()`.** Shortcut for `sh -c "..."` when shell features are needed.
