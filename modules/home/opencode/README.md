# OpenCode Home Manager module

`modules/home/opencode/` manages two things on `srt-l03-shire`:

- `~/.config/opencode/opencode.jsonc` — global OpenCode settings: server
  `permissions` (mirrored into each tool-running agent) and the explicit
  `plugins` entry.
- `~/.config/opencode/plugins/rex-activity` — the installed Rex activity
  plugin (production sources only).

The plugin source lives in a separate repository and is pinned as a non-flake
input:

- Repository: <https://github.com/nixypanda/opencode-rex-activity>
- Input: `opencode-rex-activity` (`github:nixypanda/opencode-rex-activity`,
  `flake = false`) in `flake.nix`; the commit is pinned in `flake.lock`.
- The install filter copies only `index.ts`, `runtime.ts`, `state.ts`, and
  `tui.ts`. Tests, README, and package metadata are not installed.

The `plugins` entry stays `./plugins/rex-activity`, so the TUI and background
service resolve the same directory and activate the plugin once. Global server
permissions and `~/.config/opencode/cli.json` preferences are intentionally not
managed here.

## Update

```sh
nix flake update opencode-rex-activity
```

This updates only the plugin input and its lock node. Confirm evaluation with:

```sh
nix eval --raw .#homeConfigurations.srt-l03-shire.activationPackage.drvPath
```

## Tests

Tests are not vendored in this repository. Run them from the standalone plugin
checkout:

```sh
cd ~/Documents/dev-tools/opencode-rex-activity
npm test
```

Behavior, OSC 7501 states, and the renderer `writeOut` caveat are documented in
the plugin repository README.

## Apply

```sh
just apply-home shire
```

Restart OpenCode afterward so the plugin and service configuration reload.
