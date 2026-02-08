# AGENTS.md (repo guidance for coding agents)

This repository is a Nix flake-based dotfiles setup for macOS using:
- Home Manager for user configuration
- nix-darwin for system configuration
- Neovim config managed via Home Manager (Lua configs stored in-repo)

This file is written for agentic coding tools working in this repo.

## Repository entrypoints

- Flake entrypoint: `flake.nix`
- Home Manager host: `homeConfigurations.srt-l02-sekhmet`
- nix-darwin host: `darwinConfigurations.srt-l02-sekhmet`

Key module roots:
- Home Manager modules: `modules/*` (mix of `default.nix` modules and single-file modules)
- macOS (nix-darwin) modules: `modules/mac/*.nix`
- Neovim Lua configs: `modules/nvim/lua/*.lua`

Notes:
- `.secrets` is expected to be git-crypt’d (see `Readme.md`). Avoid editing/committing secrets.
- A `result` path may exist as a Nix build output/symlink; it may be broken/missing.
- Avoid committing machine-local artifacts (e.g. `.DS_Store`).

---

## Build / Apply commands

### Apply user (Home Manager)
Preferred:
- `home-manager switch --flake "./#srt-l02-sekhmet" -b backup`

Readme-compatible:
- `nix run home-manager --no-write-lock-file -- switch --flake "./#srt-l02-sekhmet"`

Script equivalent:
- `./modules/system-management/apply-user-mac.sh`

### Apply system (nix-darwin)
Preferred:
- `sudo darwin-rebuild switch --flake ~/.dotfiles/.#srt-l02-sekhmet`

Script equivalent:
- `./modules/system-management/apply-system-mac.sh`

### Update flake lockfile
Preferred:
- `nix flake update --flake .`

Script equivalent:
- `./modules/system-management/update-dots.sh`

### “What will build?” forecast (optional)
- `./modules/system-management/build-forecast-user.sh`

This uses `nix-forecast` and writes full output to `/tmp/nix-forecast.txt`.

### Dangerous cleanup script (do not run unless asked)
- `./modules/system-management/clean-system.sh`

This prunes docker volumes/images, deletes caches, and runs Nix garbage collection.

---

## Tests / Validation (the “single test” equivalents)

This repo doesn’t have conventional unit tests.
Treat “tests” as:
- Nix evaluation/build validation
- Lint/format checks (Nix/Lua/shell/markdown)

### Fastest: evaluate only (preferred for small changes)
Home Manager activation derivation path:
- `nix eval --raw .#homeConfigurations.srt-l02-sekhmet.activationPackage.drvPath`

nix-darwin system derivation path:
- `nix eval --raw .#darwinConfigurations.srt-l02-sekhmet.system.drvPath`

If evaluation fails, re-run with:
- `--show-trace`

### Build only what you need
Build Home Manager activation package:
- `nix build .#homeConfigurations.srt-l02-sekhmet.activationPackage`

Build nix-darwin system derivation:
- `nix build .#darwinConfigurations.srt-l02-sekhmet.system`

### “Single file” sanity checks
- If you changed only Nix formatting/structure: run `nixfmt` + `statix`/`deadnix` on that file.
- If you changed only Lua: run `stylua` on that file and do a quick Neovim open/load.

---

## Lint / Format commands

Tools are generally expected to be available via a Nix-managed environment.
If a tool is missing, run it ad-hoc via:
- `nix shell nixpkgs#<tool> -c <tool> ...`

### Nix
Format:
- `nixfmt <file-or-dir>`

Static analysis:
- `statix check <file-or-dir>`
- `deadnix <file-or-dir>`

Suggested single-target runs:
- `nixfmt flake.nix`
- `statix check flake.nix`
- `deadnix flake.nix`

### Lua (Neovim config)
Format all plugin configs:
- `stylua modules/nvim/lua`

Format a single file:
- `stylua modules/nvim/lua/which-key.lua`

Stylua config lives at `modules/nvim/lua/stylua.toml`.

### Shell
Lint:
- `shellcheck <script.sh>`

Format (if desired/available):
- `shfmt -w <script.sh>`

### Markdown / Prose
Markdown lint:
- `markdownlint <file-or-dir>`

Vale:
- `vale <file-or-dir>`

Repo provides:
- `modules/nvim/markdown_lint.json`
- `modules/nvim/vale.ini`

### Git (optional)
- `gitlint`

---

## Code style guidelines

### General
- Prefer small, reviewable diffs; do not reformat unrelated files.
- Prefer editing existing modules/scripts over adding new tooling.
- Avoid changing hostnames/paths unless explicitly requested.
- Avoid editing/committing secrets (`.secrets`, tokens, credentials).

### Nix style
- Format with `nixfmt`.
- Indentation: 2 spaces.
- Prefer `let ... in` for local bindings; keep bindings close to usage.
- Keep attribute sets readable; group related options together.
- Prefer explicit names over cleverness.

**Imports / module structure**
- Keep `imports = [ ... ]` lists stable and grouped by area.
- When adding modules under `modules/*`, follow existing patterns (`default.nix` roots, etc.).

**Types / naming**
- Use consistent option names; follow surrounding module conventions.
- Prefer `snake_case` for Nix local bindings when the surrounding file does.

**Error handling**
- Use `builtins.throw` for hard failures (pattern exists in `modules/nvim/default.nix`).
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

**Diagnostics / types**
- Lua Language Server globals are configured in `modules/nvim/lua/.luarc.json`.

### Shell scripts
- Prefer `#!/bin/sh` unless bash features are required.
- Use strict mode where appropriate:
  - bash: `set -euo pipefail`
  - sh: `set -eu` (pipefail not portable)
- Quote variables and paths.
- Be careful with destructive operations (`rm -rf`, pruning caches, etc.).
- Repo scripts assume dotfiles live at `~/.dotfiles`.

### Nushell
- Files live in `modules/nu/` and are concatenated into `programs.nushell.extraConfig`.
- Keep changes compatible with nushell scripting; avoid adding non-Nix runtime deps.

---

## Neovim-specific notes

### Where plugin configs live
- Plugin list + plugin wiring: `modules/nvim/default.nix`
- Plugin configs (Lua): `modules/nvim/lua/*.lua`

Common plugin config patterns:
- Immediate setup: `require("plugin").setup(...)`
- Lazy load via `lz.n`: `require("lz.n").load({ ... after = function() ... end })`

When adding a new plugin:
- Prefer a Lua config file under `modules/nvim/lua/`.
- Reference it from `modules/nvim/default.nix` via existing `plug(...)` / `lazy_plug(...)` helpers.
- Keep plugin list ordering/comments consistent with surrounding entries.

### Linting/formatting inside Neovim
Neovim config wires these tools:
- Format on save via `conform.nvim` (`modules/nvim/lua/conform.lua`)
  - lua: `stylua`
  - nix: `nixfmt`
  - markdown: `prettier`, `markdownlint`
- Linting via `nvim-lint` (`modules/nvim/lua/lint.lua`)
  - markdown: `vale`, `markdownlint`
  - nix: `statix`
  - sh/bash: `shellcheck`
  - dockerfile: `hadolint`
  - yaml: `yamllint`

---

## Cursor/Copilot rules

No Cursor rules found (`.cursor/rules/` or `.cursorrules`).
No GitHub Copilot rules found (`.github/copilot-instructions.md`).
