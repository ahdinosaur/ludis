# lusid-operation

Concrete mutations that run on the target machine - `apt install`, `write file`, `git clone`. Operations are the leaves of the per-epoch causality tree applied by `lusid-apply`.

## `OperationType` trait

Every operation family implements [`OperationType`]:

- **`merge`** - coalesce same-family operations within an epoch. Package managers union their install sets; order-sensitive families (file, command, git) keep operations as-is.
- **`apply`** - start the operation and return `(completion_future, stdout_stream, stderr_stream)`. The caller drives all three concurrently so output streams live to the TUI.

## Privileged operations

`apt` and `pacman` wrap commands with `Command::sudo()`; `git` and `command` do not. When adding a family: only escalate when the tool actually needs root.

## Streaming output

Process-spawning families (apt, pacman, command, git) expose the child's `ChildStdout` / `ChildStderr` directly. Non-process families (`file`) return `tokio::io::empty()`.
