# Repository guidance for coding agents

Nix flake-based dotfiles for macOS (nix-darwin and Home Manager) and a NixOS
home server. Commands assume the repository lives at `~/.dotfiles`.

## Entrypoints and layout

- `flake.nix`: inputs, host registry, and configuration outputs.
- `justfile`: operational commands; run `just` to list recipes.
- `Readme.md`: setup overview; `services/README.md`: homelab runbook.
- `pkgs/`: custom derivations, repacked binaries, and fork overlays.
- `colorschemes/`: authored data using `builtins` only.
- `modules/home/`: reusable user/program configuration.
- `modules/system/{darwin,nixos}/`: reusable system configuration.
- `services/`: reusable NixOS service modules.
- `hosts/<host>/`: machine identity, hardware, and host-only overrides.

Imports flow from hosts toward reusable configuration and packages:

```text
hosts → services → modules → pkgs / colorschemes
```

Hosts may also import modules directly. `pkgs/` and `colorschemes/` never import
higher layers; modules never import services or hosts; services never import
hosts; hosts never import each other. Enforce with `just check-layout`.
Keep a module's helpers beside it; use a folder with `default.nix` for
multi-file modules.

## Build and apply

Host arguments accept canonical names or aliases from `hostRegistry`:
`srt-l03-shire` / `shire` (Darwin), `srt-n01-rivendell` / `rivendell` (NixOS).
Omitting the host uses the current hostname.

| Command | Purpose |
| --- | --- |
| `just apply [host]` | Apply every layer; Darwin system first, then Home Manager |
| `just apply-home [host]` | Standalone Home Manager only (Darwin) |
| `just apply-system [host]` | OS configuration only |
| `just build [host]` | Build every layer without activation |
| `just build-home [host]` | Build the Home Manager activation package (Darwin) |
| `just build-system [host]` | Build the OS configuration |
| `just forecast-home [host]` | Preview Home Manager builds; output in `/tmp/nix-forecast.txt` |
| `just update-flake` | Update flake inputs and lockfile |
| `just check-layout` | Check dependency boundaries |

`just apply rivendell` and `just build rivendell` build on the server over
Tailscale when run from the Mac, and operate locally on Rivendell. Local
`aarch64-darwin` cannot build Linux-only derivations. Remote commands use
`~/.ssh/github-key`; activation prompts for sudo.

### Server constraints

- SSH: `ssh nixypanda@srt-n01-rivendell` over Tailscale.
- Keep XFCE/LightDM available for local recovery.
- Keep the server login shell as Bash. Nushell is installed for interactive use,
  but using it as the login shell breaks ordinary SSH remote commands.
- Keep Linux-only `usbutils` and `usb-modeswitch` in `modules/home/cli.nix`
  guarded with `lib.optionals stdenv.hostPlatform.isLinux`.

### Removing flake inputs

Remove the input, its `outputs` argument, and referencing overlays/modules.
When editing `flake.lock` manually, remove both its node and the corresponding
`nodes.root.inputs` entry. Search for remaining references with
`rg -n -i "<name>|<name variants>" .`.

## Tests / Validation

This repository uses Nix evaluation/builds and targeted linting rather than a
conventional unit-test suite. For small changes, evaluate the affected output:

```sh
nix eval --raw .#homeConfigurations.srt-l03-shire.activationPackage.drvPath
nix eval --raw .#darwinConfigurations.srt-l03-shire.system.drvPath
nix eval --raw .#nixosConfigurations.srt-n01-rivendell.config.system.build.toplevel.drvPath
```

Rerun failures with `--show-trace`. Use `just build-home shire`,
`just build-system shire`, or `just build-system rivendell` when a build is
needed. For direct validation builds, use `nix build --no-link` to avoid
changing the `result` symlink.

Allow 120 seconds for evaluation, 600 seconds for builds, or 1200 seconds after
large input updates. Nix needs access to `~/.cache/nix` and the store; resolve
sandbox/cache access errors through the harness rather than changing project files.

### Formatting and linting

Run checks on changed files. For Nix-only structure/formatting changes, use
`nixfmt`, `statix`, and `deadnix`; for Lua changes, use `stylua` and a quick
Neovim open/load.

| Files | Commands |
| --- | --- |
| Nix | `nixfmt <file>`, `statix check <file>`, `deadnix --fail <file>` |
| Lua | `stylua <file>`; config: `modules/home/nvim/lua/stylua.toml` |
| Shell | `shellcheck <script.sh>`; optional formatting: `shfmt -w <script.sh>` |
| Markdown | `rumdl check <file>`, `vale <file>` |
| Commit messages | `committed` (optional) |

Markdown tool configuration lives in `modules/home/rumdl/rumdl.toml` and
`modules/home/vale/vale.ini`. `nixd` is a language server, not a repository checker.

If Nix tools are missing from `PATH`, look for existing store binaries before
using `nix shell nixpkgs#<tool> -c <tool> ...`:

```sh
zsh -lc 'for p in /nix/store/*/bin/{nixfmt,statix,deadnix,nixd}(N); do print -- $p; done'
```

## Editing conventions

- Keep diffs focused and follow surrounding module patterns.
- Avoid editing or committing secrets, including agenix `.age` files under
  `services/secrets/`, tokens, and credentials.
- Keep machine-local artifacts such as `.DS_Store` and build outputs out of commits.
- Nix uses `nixfmt` and two-space indentation. Group repeated attribute prefixes;
  prefer `inherit (expr) attr;` and `_:` for unused arguments where appropriate.
- Keep `...` in flake `outputs` patterns when omitting arguments such as `self`;
  otherwise evaluation can fail with an unexpected-argument error.
- Check generated shell semantics after formatting Nix strings with heredocs.
  When moving builder code into another language, read variables such as `$out`
  from the environment (for example, `os.environ["out"]`).
- Prefer `#!/bin/sh` with `set -eu`; use Bash with `set -euo pipefail` when needed.
- Nushell configuration lives in `modules/home/nu/` and is concatenated into
  `programs.nushell.extraConfig`; avoid non-Nix runtime dependencies.

## Neovim

Plugin declarations and wiring live in `modules/home/nvim/default.nix`; focused
Lua configs live in `modules/home/nvim/lua/`.

- Add plugin configs through the existing `plug(...)` / `lazy_plug(...)` helpers.
- Follow immediate `require("plugin").setup(...)` or lazy
  `require("lz.n").load({ ... after = function() ... end })` patterns nearby.
- Match Lua indentation (often tabs) and use double-quoted `require` calls.
- Use `vim.keymap.set` with `desc`; keep leader mappings under existing groups
  such as `<leader>g` and `<leader>l`.
- Guard optional plugin imports with `pcall(require, "...")` only when necessary.

Formatter and linter wiring is maintained in `lua/conform.lua` and `lua/lint.lua`
under the Neovim module.
