# Quickstart

A minimal end-to-end walkthrough: write a plan, apply it locally.

This assumes you've finished [Installation](./installation.md) and have `lusid` on your PATH.

## 1. Make a project

A lusid project is a directory with a `lusid.toml` and one or more `.lusid` plan files.

```sh
mkdir my-machine && cd my-machine
```

## 2. Declare the machine

Create `lusid.toml`:

```toml
[machines.dev]
hostname = "dev"       # must match the output of `hostname` on the target
arch = "x86-64"
os = { type = "linux", linux = "debian", debian = 13 }
plan = "./dev.lusid"
```

Set `hostname` to whatever `hostname` prints on the box you'll apply to. `lusid local apply` matches the host's `hostname` against the `hostname` field — so one `lusid.toml` can describe many machines.

## 3. Write the plan

Create `dev.lusid`:

```yaml
name: "dev"
version: "0.1.0"

setup: (params, system) =>
  - module: "@resource/apt"
    params:
      packages: ["curl", "git", "htop"]
```

This says: ensure `curl`, `git`, and `htop` are installed via `apt`. Plans are written in [Rimu](https://rimu.dev) — a small expression language. The `setup` function returns a list of items.

## 4. Apply

```sh
lusid --config ./lusid.toml local apply
```

You'll see a live TUI showing each phase: planning, observing current state, computing changes, and running the apt install. On a fresh machine this installs the packages; on a machine that already has them it prints "no changes" and exits.

## 5. Re-apply

```sh
lusid --config ./lusid.toml local apply
```

Same plan, second time: nothing happens. Re-applying is always safe — lusid only runs operations needed to close the gap between current state and what you declared.

## What just happened

1. lusid loaded `dev.lusid`, evaluated `setup(params, system)`, and got back a list of resources.
2. Each resource probed the system (`dpkg -l curl` etc.) to find current state.
3. lusid diffed current vs. desired — first run had three missing packages.
4. The three diffs merged into one `apt install curl git htop` operation.
5. The operation ran with its stdout streamed back to the TUI.

## Next

- Try the [runnable examples](../examples/) — nginx cluster, Arch desktop, dotfiles.
- Learn the **[Plan structure](./concepts/plans.md)**.
- Add **[dependencies between resources](./concepts/dependencies.md)**.
- Apply to a **[VM or remote machine](./guides/apply-modes.md)**.
