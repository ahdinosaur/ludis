# Multi-machine setups

One project can manage many machines. Each entry in `lusid.toml` is one target; each can use a different plan, or the same plan with different params.

## Two machines, one plan

```toml
# lusid.toml

[machines.web-a]
hostname = "web-a"
arch = "x86-64"
os = { type = "linux", linux = "debian", debian = 13 }
plan = "./web-server.lusid"
params = { greeting = "Hello from web-a!" }

[machines.web-b]
hostname = "web-b"
arch = "x86-64"
os = { type = "linux", linux = "debian", debian = 13 }
plan = "./web-server.lusid"
params = { greeting = "Hello from web-b!" }
```

The shared plan reads `params.greeting`; each machine gets a different value through `lusid.toml`. See [`examples/nginx-cluster/web-server.lusid`](../../examples/nginx-cluster/web-server.lusid) for the complete plan.

Apply to each:

```sh
lusid --config ./lusid.toml dev apply --machine web-a
lusid --config ./lusid.toml dev apply --machine web-b
```

You can run these in two terminals in parallel - they're independent VMs.

See the [`nginx-cluster` example](../../examples/nginx-cluster/) for a complete working version.

## Different plans per machine

Just point each machine entry at its own plan file:

```toml
[machines.web]
hostname = "web"
plan = "./plans/web.lusid"

[machines.db]
hostname = "db"
plan = "./plans/db.lusid"
```

You can still share components - `web.lusid` and `db.lusid` can both `module: "./shared/common.lusid"` to pull in monitoring, base packages, whatever's common.

## Listing machines

```sh
lusid --config ./lusid.toml machines list
```

Prints a table of every configured machine.

## ⚠️ Don't put secrets in `params`

The `params` block in `lusid.toml` is forwarded to `lusid-apply --params <json>`. That puts the values into the process's `argv[]` - visible via `ps` and `/proc/<pid>/cmdline` to any UID on the target.

For actual secrets, use [`@resource/secret`](./secrets.md). The `params` block is fine for things like hostnames, ports, group names, packages - anything that isn't sensitive.
