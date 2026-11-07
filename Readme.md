# `dotfiles`

Nix flake-based dotfiles for my Mac and home server.

## Current Setup

| Area                  | Current setup                                                 |
| --------------------- | ------------------------------------------------------------- |
| Mac host              | `srt-l03-shire`                                             |
| Home server           | `srt-n01-rivendell`                                           |
| User config           | [Home Manager](https://nixos.wiki/wiki/Home_Manager)          |
| macOS system config   | [nix-darwin](https://github.com/LnL7/nix-darwin)              |
| NixOS server config   | NixOS flake output with Home Manager and nixarr               |
| Terminal emulator     | [Kitty](https://sw.kovidgoyal.net/kitty/) plus `kitty-dev`    |
| Shell                 | [Nushell](https://www.nushell.sh/) in Kitty                   |
| Server login shell    | Bash, with Nushell installed for interactive use              |
| Font                  | Hack Nerd Font Mono                                           |
| Editor                | [Neovim](https://neovim.io/)                                  |
| Browser               | Firefox via Home Manager, Google Chrome via Mac GUI packages  |
| Home server services  | Jellyfin, Radarr, Prowlarr, qBittorrent, Seerr, and Pi-hole   |

The main flake outputs are:

- `homeConfigurations.srt-l03-shire`
- `darwinConfigurations.srt-l03-shire`
- `nixosConfigurations.srt-n01-rivendell`

## Well these look different from normal `dotfiles` mate

Ok so I am one of those people who are all in on the "nix hype train". That is
why this might look unfamiliar to someone who does not know what nix is.

Nix is a philosophy, a programming language, a package manager and an OS. All
this culminates in a unique approach to package management and system
configuration. The end result is **reproducible**, **declarative** and
**reliable** systems.

Think of it like git but for your system, package management, etc.
`:mind_blown:`

## Color me interested! Where can I know more?

Now if you are still with me and want to know more about it. Here are a few
helpful links that you can visit to know more about it.

- Watch first 6 minutes of
  [this video](https://www.youtube.com/watch?v=oPymb2-IXbg) to get a gist of nix
  package management and how it's different from standard package management and
  then building on that what is NixOS)
- If the previous video peeks your curiosity then I recommend looking at
  [this](https://www.youtube.com/watch?v=6iVXaqUfHi4) to get a deeper
  understanding
- Finally, if you decide that this is something you want for yourself. Then
  [this brilliant playlist](https://www.youtube.com/watch?v=QKoQ1gKJY5A&list=PL-saUBvIJzOkjAw_vOac75v-x6EzNzZq-)
  might just be the perfect place to start.

## Setup

These commands assume the repo lives at `~/.dotfiles`.

### Repo commands

Operational commands are defined in the root `justfile`. Run `just` from the
repo to list them. Recipes follow `<verb>-<object> [host]`; the host argument
accepts a canonical host name (`srt-l03-shire`) or the short alias from the
flake's `hostRegistry` (`shire`).

Common recipes:

| Command                     | Purpose                                                   |
| --------------------------- | --------------------------------------------------------- |
| `just apply`                | Apply every layer for this machine                        |
| `just apply shire`          | Apply the Mac: nix-darwin system, then Home Manager       |
| `just apply rivendell`      | Apply the NixOS server (remote from the Mac, local on it) |
| `just apply-home [host]`    | Home Manager only (Darwin)                                |
| `just apply-system [host]`  | OS config only                                            |
| `just build [host]`         | Build every layer without activating                      |
| `just forecast-home [host]` | Preview what a Home Manager switch would build            |
| `just update-flake`         | Update all flake inputs                                   |
| `just check-layout`         | Check the repo dependency direction                       |

`sync-hledger` is deliberately not a just recipe; Home Manager installs it as a
normal command so it can be run from anywhere on the Mac.

### Mac config

Apply the nix-darwin system config and Home Manager together:

```sh
cd ~/.dotfiles
just apply
```

The recipe uses this machine's hostname by default; pass a host alias to
override: `just apply shire`. To apply a single layer, use `just apply-system`
or `just apply-home`.

### Home server config

The NixOS server flake output is `nixosConfigurations.srt-n01-rivendell`.
The host config lives under `hosts/srt-n01-rivendell/`.

Apply it from the Mac:

```sh
cd ~/.dotfiles
just apply rivendell
```

This evaluates the flake on the Mac, builds on Rivendell over Tailscale, and
activates the result there. The same command works on the server itself and
switches locally.

### Adding a machine

Add an entry to `hostRegistry` in `flake.nix` with `platform`, `system`, `user`,
and `aliases`, then add `hosts/<host-name>/configuration.nix` (and `home.nix`
for standalone Home Manager hosts). The flake outputs and just recipes pick the
new host up automatically.

### Updating inputs

```sh
cd ~/.dotfiles
just update-flake
```

### Validation

See `AGENTS.md` → "Tests / Validation" for the evaluation and build checks.

### Caveat

Rivendell homelab secrets under `services/secrets/` are encrypted with
[agenix](https://github.com/ryantm/agenix). If you plan to use these dotfiles,
replace those files with your own secrets or remove the references from the
codebase.

## Code Structure

- `flake.nix` wires the Mac Home Manager output, Mac nix-darwin output, and the
  NixOS home server output.
- Reusable user/program modules live under `modules/home/`; shared system
  modules live under `modules/system/`.
- NixOS service modules live under `services/`.
- Per-host configuration lives under `hosts/<host>/`.
- Neovim Lua config lives under `modules/home/nvim/lua/`.
- Homelab service docs live in `services/README.md`.
