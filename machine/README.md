# lusid-machine

Declarative description of a lusid *target* machine.

Distinct from [`lusid-system`](../system): `System` describes the *current* host; `Machine` describes the *target*.

A `Machine` bundles its intended hostname, arch, OS, an optional `remote = { host = "..." }` block (for SSH targets), and optional `MachineVmOptions` (cpu, memory, graphics — used by `dev apply`).
