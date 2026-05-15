# Example: arch desktop

A single Arch Linux machine provisioned as a minimal graphical workstation:
X.org, XFCE, and LightDM. You log in at the LightDM greeter and land in an
XFCE session.

The point of this example:

- Applying a plan to a **graphical VM** (XFCE is visible in the QEMU window opened by `dev apply`).
- Installing a **group of packages** in one go with `@resource/pacman`.
- Creating a **login account with password** via `@resource/user` + a `chpasswd` shell-out.
- Sequencing a **package install** before a **service enable** with `requires`.

## Files

- [`lusid.toml`](./lusid.toml) — declares one machine (`desktop`) targeting Arch Linux x86-64, plus the username/password the plan seeds.
- [`desktop.lusid`](./desktop.lusid) — installs X + XFCE + LightDM, creates a login user with a password, then enables and starts `lightdm.service`.

## What the plan does

```text
pacman install xorg-server xorg-xinit xfce4 lightdm lightdm-gtk-greeter ──┐
                                                                          │
user create <username> ──► command chpasswd <username>:<password> ───┐    │
                                                                     ▼    ▼
                                                    systemd enable + start lightdm
```

1. `@resource/pacman` installs `xorg-server`, `xorg-xinit`, `xfce4`, `lightdm`, `lightdm-gtk-greeter` in a single transaction.
2. `@resource/user` creates the login account named by `params.username`. The cloud image's default `arch` user has a locked password and can't sign in at the greeter, so the plan adds a real account.
3. `@resource/command` sets the password with `chpasswd`. An `is_installed` check against `passwd -S` keeps it idempotent — once the account has a password, re-applies skip this step.
4. `@resource/systemd` enables and starts `lightdm.service` after the user is in place. A QEMU window shows the LightDM greeter a few seconds after apply finishes.

## Try it (local dev VM)

From the repo root. The first run downloads the Arch cloud image (~700 MB)
and takes a few minutes; later runs reuse it.

```sh
# Apply the plan. A QEMU window opens during boot; the apply itself streams
# in your terminal.
just arch-desktop-apply

# After apply, the QEMU window will show the LightDM login greeter. Log in
# with the credentials from `lusid.toml` — by default that's `me` / `me`.
# If you need a shell instead (as the cloud image's default `arch` user,
# key-authenticated):
just arch-desktop-ssh
```

## Try it (on a real Arch machine)

Same plan, no VM. Copy `desktop.lusid` and `lusid.toml` onto the target
Arch host (making sure the `hostname` in `lusid.toml` matches the host's
own hostname, and picking a real `username` / `password` in the `params`
block — the defaults are demo-grade). Then run:

```sh
lusid local apply --config ./lusid.toml
```

You'll need `sudo` on the target — `pacman -S`, `useradd`, `chpasswd`, and
`systemctl enable/start` all use it. Once the plan completes, reboot (or
just log out of the console) to reach the LightDM greeter and sign in as
the user the plan created.

## Things to try next

- Swap XFCE for LXQt: change the packages to
  `["xorg-server", "xorg-xinit", "lxqt", "sddm"]` and the systemd unit to
  `sddm` (LXQt's typical greeter is SDDM, not LightDM). Re-apply and you
  get a different desktop with the same plan shape.
- Give the login user `sudo`: add `append_groups: ["wheel"]` to the
  `@resource/user` item, then uncomment the `%wheel ALL=(ALL:ALL) ALL` line
  in `/etc/sudoers` (e.g. via a second `@resource/command` item).
- Add your dotfiles (vimrc, gitconfig, etc.) via `@resource/file` items, each
  sourced relative to the plan directory.
