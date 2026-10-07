# AGENTS.md (repo guidance for coding agents)

This repository is a Nix flake-based dotfiles setup for macOS using:
- Home Manager for user configuration
- nix-darwin for system configuration
- Neovim config managed via Home Manager (Lua configs stored in-repo)

This file is written for agentic coding tools working in this repo.

## Repository entrypoints

- Flake entrypoint: `flake.nix`
- Home Manager host: `homeConfigurations.srt-l03-shire`
- nix-darwin host: `darwinConfigurations.srt-l03-shire`
- NixOS home server: `nixosConfigurations.srt-n01-rivendell`

Key module roots:
- Reusable Home Manager modules: `modules/home/*`
- Reusable system (NixOS/nix-darwin) modules: `modules/system/{darwin,nixos}/`
- NixOS service modules: `services/*`
- Per-host overrides: `hosts/<host>/`
- Neovim Lua configs: `modules/home/nvim/lua/*.lua`

## Repo layout (top-level segregation)

Top-level directories are split by provenance:

- `pkgs/` — packages this repo builds from upstream sources (custom derivations,
  repacked binaries, fork overlays). Never imports a higher layer.
- `colorschemes/` — authored data; `builtins` only.
- `modules/` — reusable, host-agnostic configuration. `modules/home/` is user
  and program config; `modules/system/{darwin,nixos}/` is machine config.
- `services/` — NixOS service modules (nixarr, Pi-hole, Caddy, hedger, …).
  Host-agnostic; imported by the hosts that run them.
- `hosts/` — machine instances: identity, hardware, and host-only overrides.
  A host imports the modules and services it needs.

A module's own helpers live beside it in the same folder; a piece with several
files is a folder with `default.nix`.

Dependency direction (`A → B` means "A may import B"):

```
pkgs         ┐
             ├─► modules ──► services ──► hosts
colorschemes ┘
```

Invariants: `pkgs/` and `colorschemes/` never import a higher layer; `modules/`
never imports `services/` or `hosts/`; `services/` never imports `hosts/`; hosts
never import each other. A host imports `modules/` and `services/` directly.
Enforce with `sh modules/home/system-management/check-layout.sh` (or the installed
`check-layout` command).

Notes:
- Avoid editing/committing secrets (agenix `.age` files under `services/secrets/`, tokens, credentials).
- A `result` path may exist as a Nix build output/symlink; it may be broken/missing.
- Avoid committing machine-local artifacts (e.g. `.DS_Store`).

---

## Build / Apply commands

### Apply user (Home Manager)
Preferred:
- `home-manager switch --flake "./#srt-l03-shire" -b backup`

Readme-compatible:
- `nix run --no-write-lock-file --inputs-from . home-manager#home-manager -- switch --flake "./#srt-l03-shire"`

Script equivalent:
- `./modules/home/system-management/apply-user.sh`

### Apply system (nix-darwin)
Preferred:
- `sudo darwin-rebuild switch --flake ~/.dotfiles/.#srt-l03-shire`

Script equivalent:
- `./modules/home/system-management/apply-darwin.sh`

### Apply home server (NixOS)
Host:
- Hostname: `srt-n01-rivendell`
- Flake output: `nixosConfigurations.srt-n01-rivendell`
- Host config root: `hosts/srt-n01-rivendell/`
- Normal remote access: SSH over Tailscale, `ssh nixypanda@srt-n01-rivendell`
- Current known Tailscale IP: `100.127.3.54`
- Current known LAN IP: `192.168.1.76`

Preferred remote switch from the Mac:
- `env NIX_SSHOPTS='-i /Users/nixypanda/.ssh/github-key -o StrictHostKeyChecking=accept-new' nix run nixpkgs#nixos-rebuild -- switch --flake .#srt-n01-rivendell --build-host nixypanda@100.127.3.54 --target-host nixypanda@100.127.3.54 --no-reexec --use-substitutes --ask-sudo-password`

Build only on the server over SSH:
- `env NIX_SSHOPTS='-i /Users/nixypanda/.ssh/github-key -o StrictHostKeyChecking=accept-new' nix run nixpkgs#nixos-rebuild -- build --flake .#srt-n01-rivendell --build-host nixypanda@100.127.3.54 --target-host nixypanda@100.127.3.54 --no-reexec --use-substitutes`

Local on-server equivalent:
- `sudo nixos-rebuild switch --flake ~/.dotfiles#srt-n01-rivendell`

Server notes:
- Tailscale is enabled declaratively on both the Mac and NixOS server.
- The server keeps XFCE/LightDM enabled for local monitor/keyboard recovery.
- The `nixypanda` login shell on the server should remain Bash. Nushell is installed and configured, but making it the login shell breaks ordinary SSH remote commands such as `ssh host 'echo ---; hostname'`.
- `usbutils` and `usb-modeswitch` are Linux-only packages in `modules/home/cli.nix`; keep them guarded with `lib.optionals stdenv.hostPlatform.isLinux`.

### Update flake lockfile
Preferred:
- `nix flake update --flake .`

Script equivalent:
- `./modules/home/system-management/update-flake.sh`

When removing a flake input manually:
- Remove it from `inputs` in `flake.nix`.
- Remove it from the `outputs` argument pattern and any overlays/modules that reference it.
- Remove the node from `flake.lock`.
- Remove the corresponding entry from `nodes.root.inputs` in `flake.lock`.
- Verify with `rg -n -i "<name>|<name variants>" .`.

### “What will build?” forecast (optional)
- `./modules/home/system-management/forecast-build.sh`

This uses `nix-forecast` and writes full output to `/tmp/nix-forecast.txt`.

---

## Tests / Validation (the “single test” equivalents)

This repo doesn’t have conventional unit tests.
Treat “tests” as:
- Nix evaluation/build validation
- Lint/format checks (Nix/Lua/shell/markdown)

### Fastest: evaluate only (preferred for small changes)
Home Manager activation derivation path:
- `nix eval --raw .#homeConfigurations.srt-l03-shire.activationPackage.drvPath`

nix-darwin system derivation path:
- `nix eval --raw .#darwinConfigurations.srt-l03-shire.system.drvPath`

NixOS server system derivation path:
- `nix eval --raw .#nixosConfigurations.srt-n01-rivendell.config.system.build.toplevel.drvPath`

If evaluation fails, re-run with:
- `--show-trace`

### Build only what you need
Build Home Manager activation package:
- `nix build .#homeConfigurations.srt-l03-shire.activationPackage`

Build nix-darwin system derivation:
- `nix build .#darwinConfigurations.srt-l03-shire.system`

Build NixOS server system derivation:
- On the server or with a Linux remote builder:
  `nix build .#nixosConfigurations.srt-n01-rivendell.config.system.build.toplevel`
- From the Mac, prefer `nixos-rebuild --build-host nixypanda@100.127.3.54`
  because local `aarch64-darwin` cannot build Linux-only derivations.

For validation builds, prefer avoiding `result` symlink churn:
- `nix build --no-link .#homeConfigurations.srt-l03-shire.activationPackage`
- `nix build --no-link .#darwinConfigurations.srt-l03-shire.system`
- `nix build --no-link .#nixosConfigurations.srt-n01-rivendell.config.system.build.toplevel`

Sandbox note:
- `nix eval` / `nix build` may need access to `~/.cache/nix` and the Nix store. If evaluation fails with an SQLite cache error under `~/.cache/nix`, rerun with the appropriate sandbox escalation rather than changing project files.

### Tool timeouts (for agentic CLI runs)
Many agent CLIs default to a ~2 minute command timeout. That is fine for `nix eval`, but it is often too short for `nix build` (especially after updating flake inputs, when caches may miss and local builds kick in).

Recommended timeouts:
- `nix eval ...`: 2 minutes (120s)
- `nix build .#homeConfigurations.srt-l03-shire.activationPackage`: 10 minutes (600s)
- `nix build .#darwinConfigurations.srt-l03-shire.system`: 10 minutes (600s)
- `nix build .#nixosConfigurations.srt-n01-rivendell.config.system.build.toplevel`: 10 minutes (600s) when run on Linux or with a Linux build host
- After `nix flake update` / lockfile updates: consider 20 minutes (1200s) for builds if large deps rebuild

If your tool takes timeouts in milliseconds, use:
- eval: `timeout=120000`
- build: `timeout=600000` (or `timeout=1200000` after input bumps)

### “Single file” sanity checks
- If you changed only Nix formatting/structure: run `nixfmt` + `statix`/`deadnix` on that file.
- If you changed only Lua: run `stylua` on that file and do a quick Neovim open/load.

---

## Lint / Format commands

Tools are generally expected to be available via a Nix-managed environment.
If a tool is missing, run it ad-hoc via:
- `nix shell nixpkgs#<tool> -c <tool> ...`

On this machine, Nix tools may already exist in `/nix/store` even when they are not on `PATH`.
Before falling back to `nix shell`, search for existing binaries and use the newest matching version:
- `zsh -lc 'for p in /nix/store/*/bin/{nixfmt,statix,deadnix,nixd}(N); do print -- $p; done'`
- `zsh -lc 'for p in /nix/store/*{nixfmt,statix,deadnix,nixd}*/bin/*(N); do print -- $p; done'`

Known-good examples from this repo:
- `nixfmt 1.2.0`
- `statix 0-unstable-2026-05-03`
- `deadnix 1.3.1`
- `nixd 2.9.0`

`nixd` is a language server here, not a standalone repository checker. Use `nixd --version` to verify the binary, and use `nix eval` / `nix build` for actual flake validation.

### Nix
Format:
- `nixfmt <file-or-dir>`
- Whole repo: `rg --files -g '*.nix' -0 | xargs -0 nixfmt`

Static analysis:
- `statix check <file-or-dir>`
- Whole repo: `statix check .`
- `deadnix <file-or-dir>`
- Whole repo with failure on findings: `deadnix --fail .`

Suggested single-target runs:
- `nixfmt flake.nix`
- `statix check flake.nix`
- `deadnix flake.nix`

### Lua (Neovim config)
Format all plugin configs:
- `stylua modules/home/nvim/lua`

Format a single file:
- `stylua modules/home/nvim/lua/which-key.lua`

Stylua config lives at `modules/home/nvim/lua/stylua.toml`.

### Shell
Lint:
- `shellcheck <script.sh>`

Format (if desired/available):
- `shfmt -w <script.sh>`

### Markdown / Prose
Markdown lint:
- `rumdl check <file-or-dir>`

Vale:
- `vale <file-or-dir>`

Repo provides:
- `modules/home/rumdl/rumdl.toml`
- `modules/home/vale/vale.ini`

### Git (optional)
- `committed` (commit message linting)

---

## Code style guidelines

### General
- Prefer small, reviewable diffs; do not reformat unrelated files.
- Prefer editing existing modules/scripts over adding new tooling.
- Avoid changing hostnames/paths unless explicitly requested.
- Avoid editing/committing secrets (agenix `.age` files, tokens, credentials).

### Nix style
- Format with `nixfmt`.
- Indentation: 2 spaces.
- Prefer `let ... in` for local bindings; keep bindings close to usage.
- Keep attribute sets readable; group related options together.
- Prefer explicit names over cleverness.
- Avoid repeated top-level attribute prefixes in the same module when practical. Prefer `home = { activation...; packages = ...; file = ...; };` over separate `home.activation`, `home.packages`, and `home.file` assignments when `statix` flags repeated keys.
- Prefer `inherit (expr) attr;` when `statix` flags `attr = expr.attr;`.
- For unused lambda args in overrides, use `_:` instead of naming the argument.
- For modules that need no arguments, use `_:` instead of `{ ... }:` if `statix` flags an empty pattern.
- In flake `outputs`, keep `...` in the argument pattern if omitting unused inputs like `self`; flakes still pass `self`, and without `...` evaluation fails with “unexpected argument 'self'”.
- Be careful running `nixfmt` on shell heredocs embedded in Nix strings. If formatting changes heredoc indentation, verify the generated script still has the same semantics.
- If moving shell-expanded code into a generated script, do not leave shell variables like `$out` as string literals. Read them explicitly from the environment, e.g. `os.environ["out"]`, when the Nix builder provides them.

**Imports / module structure**
- Keep `imports = [ ... ]` lists stable and grouped by area.
- When adding modules under `modules/*`, follow existing patterns (`default.nix` roots, etc.).

**Types / naming**
- Use consistent option names; follow surrounding module conventions.
- Prefer `snake_case` for Nix local bindings when the surrounding file does.

**Error handling**
- Use `builtins.throw` for hard failures (pattern exists in `modules/home/nvim/default.nix`).
- When guarding optional inputs, prefer clear failure messages.

### Lua style (Neovim)
- Match existing indentation (many files use tabs).
- Prefer `require("...")` with double quotes.
- Use `local` variables; avoid globals except for `vim.*`.
- Keep each file focused on one plugin/area.

**Keymaps**
- Use `vim.keymap.set(...)`.
- Always include `{ desc = "..." }`.
- Prefer leader mappings under existing group prefixes (`<leader>g`, `<leader>l`, etc.).

**Error handling**
- Guard optional plugin requires with `pcall(require, "...")` only when necessary.

### Shell scripts
- Prefer `#!/bin/sh` unless bash features are required.
- Use strict mode where appropriate:
  - bash: `set -euo pipefail`
  - sh: `set -eu` (pipefail not portable)
- Quote variables and paths.
- Be careful with destructive operations (`rm -rf`, pruning caches, etc.).
- Repo scripts assume dotfiles live at `~/.dotfiles`.

### Nushell
- Files live in `modules/home/nu/` and are concatenated into `programs.nushell.extraConfig`.
- Keep changes compatible with nushell scripting; avoid adding non-Nix runtime deps.

---

## Neovim-specific notes

### Where plugin configs live
- Plugin list + plugin wiring: `modules/home/nvim/default.nix`
- Plugin configs (Lua): `modules/home/nvim/lua/*.lua`

Common plugin config patterns:
- Immediate setup: `require("plugin").setup(...)`
- Lazy load via `lz.n`: `require("lz.n").load({ ... after = function() ... end })`

When adding a new plugin:
- Prefer a Lua config file under `modules/home/nvim/lua/`.
- Reference it from `modules/home/nvim/default.nix` via existing `plug(...)` / `lazy_plug(...)` helpers.
- Keep plugin list ordering/comments consistent with surrounding entries.

### Linting/formatting inside Neovim
Neovim config wires these tools:
- Format on save via `conform.nvim` (`modules/home/nvim/lua/conform.lua`)
  - lua: `stylua`
  - nix: `nixfmt`
  - css/html/javascript/typescript/json: `biome`
  - markdown: `rumdl`
  - yaml: `yamlfmt`
- Linting via `nvim-lint` (`modules/home/nvim/lua/lint.lua`)
  - markdown: `vale`, `rumdl`
  - nix: `statix`
  - sh/bash: `shellcheck`
  - dockerfile: `hadolint`
  - yaml: `yamllint`

---

## Cursor/Copilot rules

No Cursor rules found (`.cursor/rules/` or `.cursorrules`).
No GitHub Copilot rules found (`.github/copilot-instructions.md`).
