# lusid documentation

Configure your computers with the exact setup you describe.

This is the documentation for [lusid](https://github.com/ahdinosaur/lusid) - a declarative machine configuration tool, written in Rust, with plans authored in the [Rimu](https://rimu.dev) language.

> Status: MAD SCIENCE 🧪 - pre-1.0, fast-moving. Expect rough edges.

## Start here

- **[Introduction](./introduction.md)** - what lusid is, who it's for, how it compares.
- **[Installation](./installation.md)** - download a release or build from source.
- **[Quickstart](./quickstart.md)** - your first plan in five minutes.

## Concepts

- **[Plans](./concepts/plans.md)** - what a `.lusid` file looks like.
- **[Resources](./concepts/resources.md)** - declaring desired state.
- **[Operations](./concepts/operations.md)** - running imperative actions.
- **[Dependencies](./concepts/dependencies.md)** - ordering with `requires`, epochs, and `on_change` hooks.

## Guides

- **[Apply modes](./guides/apply-modes.md)** - local, dev VM, remote SSH.
- **[Multi-machine setups](./guides/multi-machine.md)** - one plan, many targets.
- **[Files and directories](./guides/files-and-directories.md)** - `sourced` vs `linked`.
- **[Secrets](./guides/secrets.md)** - age-encrypted secrets with `@resource/secret`.
- **[`on_change` hooks](./guides/hooks.md)** - reloading services after config edits.

## Reference

- **[CLI](./reference/cli.md)** - every `lusid` subcommand.
- **[`lusid.toml`](./reference/lusid-toml.md)** - config schema.
- **[Plan syntax](./reference/plan-syntax.md)** - the Rimu plan shape.
- **[Resources](./reference/resources.md)** - every built-in resource type.
- **[The `system` object](./reference/system.md)** - what plans receive at evaluation.

## Contributing

- **[Architecture](./contributing.md)** - how the codebase fits together.

## See also

- **[Examples](../examples/)** - runnable end-to-end examples (nginx cluster, Arch desktop, dotfiles).
- **[Rimu](https://rimu.dev)** - the language plans are written in.
- **Source READMEs** - each crate has its own README; the docs here link out for deep dives.
