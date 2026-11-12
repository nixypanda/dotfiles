# Nushell Home Manager module

`modules/home/nu/` is concatenated into `programs.nushell.extraConfig` (and
`extraEnv` for `env.nu`) by `default.nix`.

## Program status (OSC 7501)

Program status support is provided by the external `nu-program-status` repository,
integrated as a non-flake flake input (`nu-program-status`).

- Repository: <https://github.com/nixypanda/nu-program-status>
- Flake input: `nu-program-status.url = "github:nixypanda/nu-program-status";`
  with `flake = false`.
- Runtime files are loaded from the Nix store:
  `use ${runtime}/mod.nu *` and `use ${runtime}/hooks.nu *`, followed by
  `program-status install-hooks --min-duration 0sec`.
  The store path copies the whole `nu/` directory, including
  `nu/data/exclusions.nuon`.
- The vendored `program-status.nu` file is not used.

### Hook overrides

```nu
program-status install-hooks --min-duration 0sec
program-status install-hooks --exclude ["my-agent"] --include ["claude"] --no-default-exclusions
program-status exclusions
```

Rules may be single commands or explicit prefixes such as `"docker agent"`.
Included prefixes remove only an exactly matching exclusion. Omitted flags
preserve existing values; explicit empty lists clear user lists. The curated
default exclusions now use only `prefix` and `reason`; personal threshold is
`0sec`, so all eligible foreground commands receive immediate `working` status
and a terminal outcome.

### Updating

Update only the program status input and lock entry:

```sh
nix flake update nu-program-status
```

### Tests

Run upstream tests in the standalone repository:

```sh
nu tests/protocol.nu
python3 tests/hooks_pty.py
```

### Apply

Build or apply Home Manager as usual after changing the input or module:

```sh
just build-home shire
just apply-home shire
```
