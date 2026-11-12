# OpenCode Home Manager module

`modules/home/programming.nix` installs OpenCode from nixpkgs on macOS and
Linux. Its version follows the nixpkgs input pinned in `flake.lock`.

`modules/home/opencode/` manages these files on `srt-l03-shire`:

- `~/.config/opencode/opencode.jsonc` — global OpenCode settings: server
  `permissions` (mirrored into each tool-running agent) and the cache-hit plugin.
- `~/.config/opencode/cache-hit.json` — cache-hit display settings.

The `~/.config/opencode/cli.json` file is not managed by Home Manager; selected
terminal preferences are overridden through Nushell as described below.

## Terminal activity

OpenCode reports activity directly to terminals supporting OSC 7501, including
Rex. The native implementation reports working, waiting for permission or an
answer, and idle states. With session tabs disabled, it follows the current
session and emits a root summary and a per-session record.

Native reporting replaces the custom Rex activity plugin. It does not report
`done` or `error` activity states; completed sessions return to idle. Rex controls
its own sounds and notifications for the reported states.

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

Restart the OpenCode service and reopen the TUI afterward so the package and
configuration changes take effect:

```sh
opencode service restart
```
