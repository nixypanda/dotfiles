# OpenCode Home Manager module

`modules/home/programming.nix` installs OpenCode from nixpkgs on macOS and
Linux. Its version follows the nixpkgs input pinned in `flake.lock`.

`modules/home/opencode/` manages these files on `srt-l03-shire`:

- `~/.config/opencode/opencode.jsonc` — global OpenCode settings: server
  `permissions` (mirrored into each tool-running agent), the cache-hit plugin,
  and the Playwright MCP server.
- `~/.config/opencode/cache-hit.json` — cache-hit display settings.
- `~/.config/opencode/plugins/opencode-rex-status` — pinned TUI plugin that
  reports finished and failed sessions to Rex.

The global settings load the local `plugins/no-rate-limit-retries/` server
plugin directly from its Nix store directory. This keeps its entrypoint beside
the plugin directory even when Home Manager symlinks the configuration file.
It disables retries for HTTP 429 rate-limit failures in all sessions, including
subagents. Other failures retain OpenCode's retry policy.

The `~/.config/opencode/cli.json` file is not managed by Home Manager; selected
terminal preferences are overridden through Nushell as described below.

## Browser automation

This module installs `playwright-mcp` from nixpkgs and configures it globally
under `mcp.servers.playwright`. Its version follows `flake.lock`; there is no
custom npm package, browser automation skill, or OpenCode plugin to maintain.

Ask OpenCode to use Playwright to inspect a site, review a local HTML prototype,
or test browser interactions. Check the connection after applying Home Manager:

```sh
opencode mcp list
```

The server runs headless with an isolated browser profile. On macOS, it uses
the Nix-managed Chrome executable rather than expecting Chrome in
`/Applications`. Other platforms use the browsers supplied by the nixpkgs
package. Code Mode remains enabled by default. Agents sharing one connection
also share browser state, so coordinate browser actions within a project.
Keep saved authentication state out of version control.

## Terminal activity

OpenCode reports activity directly to terminals supporting OSC 7501, including
Rex. The native implementation reports working, waiting for permission or an
answer, and idle states. With session tabs disabled, it follows the current
session and emits a root summary and a per-session record.

The native implementation never reports `done` or `error`, so
`modules/home/opencode/` also installs the
[opencode-rex-status](https://github.com/martinmiglio/opencode-rex-status) TUI
plugin. It fills that gap: a session that finishes or fails in a background tab
stays flagged until it is focused. The plugin only runs when `REX_SESSION` is
set, so it is inert outside Rex, and it requires OpenCode 2.2 or later. Rex
controls its own sounds and notifications for the reported states.

## Rex status plugin

The plugin is pinned as the `opencode-rex-status` flake input in `flake.nix`, so
its version follows `flake.lock`. Home Manager symlinks that source tree to
`~/.config/opencode/plugins/opencode-rex-status`. OpenCode discovers TUI plugins
as directories under `~/.config/opencode/plugins`, so `cli.json` stays
unmanaged and needs no `plugins` entry. Symlinking the whole tree keeps
`rex-status.mjs` beside the `package.json` that exports it as `./tui`.

Check that OpenCode loaded it after applying Home Manager, or watch the plugin
logs at `~/.local/share/opencode/log/opencode.log` for a `rex.status` setup
entry:

```sh
opencode --standalone
```

The upstream unit tests also run against the pinned source:

```sh
cd ~/.config/opencode/plugins/opencode-rex-status && node --test
```

Update the plugin with `nix flake update opencode-rex-status`, then rebuild and
apply Home Manager. Reopen the TUI to load the new copy.

## Terminal preferences

`modules/home/nu/env.nu` sets `OPENCODE_CLI_CONFIG_CONTENT`, which overrides
the corresponding settings in `cli.json`:

- `tabs.mode = "off"` disables OpenCode session tabs.
- `attention.sound = false` disables all OpenCode attention sounds.
- `attention.notifications = false` disables OpenCode system notifications.

OpenCode continues emitting native OSC 7501 reports for Rex. No custom sound
files or per-event sound overrides are needed.

## Update and apply

Update the nixpkgs input to pick up newer OpenCode packages:

```sh
nix flake update nixpkgs
```

Build and apply the Home Manager configuration:

```sh
just build-home shire
just apply-home shire
```

OpenCode watches its configuration and reloads MCP servers after changes.
Use `/reload` to force a full configuration reload, then `/mcps` to check
the Playwright connection. Reopen the TUI when switching to a new CLI package.
