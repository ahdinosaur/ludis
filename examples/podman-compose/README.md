# Example: podman-compose project

A small `@resource/podman-compose` example that brings up a two-service compose
project (nginx + redis) under the local podman runtime.

The point of this example is to show:

- How to declare a **podman-compose project** via `@resource/podman-compose`.
- How **drift detection** works: edit `compose.yaml` and re-apply to see the
  project recreated.
- The lifecycle counterpart `state: "absent"` for tearing it down.

## Files

- [`lusid.toml`](./lusid.toml) - declares a single local target.
- [`app.lusid`](./app.lusid) - the plan, declares one `@resource/podman-compose`
  pointing at the file below.
- [`compose.yaml`](./compose.yaml) - standard Compose v3 spec with two
  services and one named volume.

## Prerequisites

- `podman` and `podman-compose` installed on the target machine.
- No conflicting `lusid_demo` project already up on the same runtime.

## Try it

From this directory:

```sh
# Apply: lusid hashes the compose files, decides an `Up` is needed,
# brings the project up via podman-compose, and then installs a small
# marker network (lusid-compose-marker-lusid_demo) carrying the hash.
lusid local apply --config ./lusid.toml

# Verify:
podman ps --filter label=com.docker.compose.project=lusid_demo
curl localhost:8080
```

To watch drift detection in action:

```sh
# Edit compose.yaml - bump the nginx image tag, change a service name,
# or just append a blank line. Save.

lusid local apply --config ./lusid.toml
# The hash now differs from what is in the marker network. Lusid
# emits a Recreate change: down, then up, with the new hash
# installed on the marker.
```

To tear down:

```sh
# Change `state: "present"` to `state: "absent"` in app.lusid (and drop
# the `files:` line, which `absent` doesn't need). Re-apply.

lusid local apply --config ./lusid.toml
# Containers and networks bearing the project label are removed.
# Named volumes (here: `redis-data`) are **preserved** by default;
# wipe them with `podman volume rm lusid_demo_redis-data` if you mean it.
```

## What lusid is not doing

- It does not parse the compose YAML itself - service names, image
  references, etc. are decided by `podman-compose`. Lusid hashes the file
  bytes for drift detection and shells out for everything else.
- It does not pull images on a schedule; for that, attach an
  `@operation/podman-compose action: "pull"` to whichever resource drives
  your update cadence (e.g. an `on_change` of a timer or a manually-
  versioned `@resource/file`).
- It does not support `--build`, profiles, or service-level commands. Use
  `@operation/command` shelling out to `podman-compose` for those.

## Rootless vs rootful

The example sets `sudo: false` (rootless). To run under rootful podman
instead, flip `sudo: true` in `app.lusid`. The flag is part of the
drift-detection hash, so flipping it on an existing project triggers a
clean rebuild under the new runtime.
