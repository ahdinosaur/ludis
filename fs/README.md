# lusid-fs

Async filesystem helpers for lusid operations and resources.

Each function wraps a `tokio::fs` / `nix` / `filetime` call and maps the underlying error into a rich [`FsError`] variant that always carries the offending path - so error messages don't need to re-construct context downstream.

- **`write_file_atomic` / `copy_file_atomic`** - write to a sibling temp file, copy metadata (from destination and source respectively), and rename. Readers never observe a partial write.
- **`change_owner` / `change_owner_by_id`** - Unix-only uid/gid changes, resolving user/group names via `nix`.
- **`copy_dir`** - shells out to `cp --recursive`; assumes GNU coreutils. macOS/BSD will misbehave.
