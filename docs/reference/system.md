# The `system` object

`system` is the second argument every plan's `setup` function receives. It's populated from runtime detection on the machine running apply - so plans can branch on hostname, OS, arch, or the current user.

```yaml
setup: (params, system) =>
  - module: "@resource/file"
    params:
      state: "linked"
      source: "./zshrc"
      path: system.user.home + "/.zshrc"
```

## Fields

```text
system
├── hostname        string         e.g. "my-laptop"
├── arch            string         "x86-64" or "aarch64"
├── os
│   ├── type        string         "linux" (only Linux today)
│   ├── linux       string         "debian" / "ubuntu" / "arch"
│   ├── debian      number         only when linux = "debian"; major version (12, 13, …)
│   ├── ubuntu      string         only when linux = "ubuntu"; "YY.MM" (e.g. "22.04")
│   └── (none)                     when linux = "arch" - Arch is rolling
└── user
    ├── name        string         current user running apply
    ├── home        string         current user's $HOME
    └── primary_group string       primary Unix group
```

The shape is `non_exhaustive` - new fields may be added without breaking existing plans.

## Branching on OS

`system.os.linux` is a string you can match on to pick distro-specific resources. The exact conditional syntax is Rimu's - see [rimu.dev](https://rimu.dev). The simplest portable pattern is to read fields directly:

```yaml
setup: (params, system) =>
  - module: "@resource/file"
    params:
      state: "linked"
      source: "./" + system.os.linux + "/zshrc"   # ./debian/zshrc, ./arch/zshrc
      path: system.user.home + "/.zshrc"
```

## Where it's detected

| Field | How |
| --- | --- |
| `hostname` | `hostname` syscall via the `hostname` crate. |
| `arch` | Compile-time `cfg(target_arch)`. |
| `os.type` / `os.linux` / version | `/etc/os-release` on Linux, via the `etc-os-release` crate. Unknown distros error rather than silently defaulting. |
| `user.name` / `user.home` | `$USER` / `$HOME` on Unix, `$USERNAME` / `$USERPROFILE` on Windows. |
| `user.primary_group` | `getgid()` + NSS group lookup. |

## Caveat

`system` reflects the machine running apply, not the target machine. For `local apply` these are the same; for `dev apply` it's the dev VM's view *during apply*; for `remote apply` it's the remote target's view.

The fields on `lusid.toml`'s `[machines.<id>]` are the *declared* machine spec - what you want it to be. `system` is what apply observes at runtime. Usually they match, but a plan that mismatches its declared OS will see the actual one in `system`.
