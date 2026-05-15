# Example: nginx cluster

Two Debian 13 servers, both running nginx, each serving its own greeting page.

The point of this example is to show:

- How to define **multiple machines** in one `lusid.toml`.
- How to feed **per-machine parameters** (`params`) into a shared plan so one
  plan definition can drive a fleet.
- How to compose `@resource/*` resources (`apt`, `command`, `systemd`) with
  **dependency ordering** (`requires`) inside a plan.

## Files

- [`lusid.toml`](./lusid.toml) - declares the two machines (`web-a`, `web-b`)
  and passes each a different `greeting` string.
- [`web-server.lusid`](./web-server.lusid) - the plan both machines apply.
  Installs nginx, writes a custom `/var/www/html/index.html`, then enables
  and starts the nginx service.

## What the plan does

```text
install-nginx ──► publish-index ──► systemd: enable + start nginx
```

1. `@resource/apt` installs the `nginx` package.
2. `@resource/command` writes `/var/www/html/index.html` with `params.greeting` and `system.hostname`. The `is_installed` check (`grep -qF` for the greeting) keeps re-applies a no-op.
3. `@resource/systemd` ensures `nginx.service` is `enabled` (on boot) and `active` (right now).

## Try it (local dev VMs)

From the repo root. These commands boot a QEMU VM per machine, upload the
plan, and apply it over SSH. The first run downloads the Debian cloud image
(~400 MB per arch) and takes a few minutes.

```sh
# List the machines defined in this example's config.
just nginx-cluster-list

# Apply to each server. You can run them in parallel in two terminals.
just nginx-cluster-apply-a
just nginx-cluster-apply-b

# Once apply completes, SSH in and verify nginx is serving the page.
just nginx-cluster-ssh-a
# inside the VM:
#   curl localhost
#   → <!doctype html>...<h1>Hello from web-a!</h1>...Served by web-a via lusid.
```

The dev VM's port 80 is **not** forwarded to the host by default, which is
why you `curl localhost` from inside the VM rather than from your host
browser. (Adding host→guest port forwarding is on the roadmap.)

## Try it (on a real machine)

The same plan works on any Debian 13 host you can reach - no VM required.
The intended flow is:

1. SSH into the target machine.
2. Copy `web-server.lusid` and a matching `lusid.toml` onto it.
3. Run `lusid local apply --config ./lusid.toml`. (`local apply` picks the
   entry whose `hostname` matches the host's own `$(hostname)`.)

You'll need `sudo` on the target, since nginx installation and the
`publish-index` command use it. The idempotent `is_installed` check means
re-running the plan after a success is fast and makes no changes.

## Things to try next

- Add a third machine entry with its own greeting - no plan changes required.
- Change a greeting in `lusid.toml` and re-apply: the `publish-index` command
  should re-run because the `grep` check now misses; nginx will reload its
  file on the next request.
- Comment out the `@resource/systemd` item (Rimu supports `#` line comments)
  and re-apply: nginx is installed but not running - a useful state for
  staging.
