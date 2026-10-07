{
  pkgs,
  nodejs,
  dsh,
  desktopRoot,
  installLines,
}:
pkgs.writeShellApplication {
  name = "dsh-plugins-sync";
  runtimeInputs = [
    nodejs
    pkgs.pnpm
    dsh
  ];
  text = ''
    DSH_HOME="''${DSH_HOME:-$HOME/.dsh}"
    desktop_root=${pkgs.lib.escapeShellArg desktopRoot}

    # Report whether a package is installed in a profile, and (when it is
    # declared as a bundle) selected in dsh.profile.bundles. Exit 2 means the
    # manifest could not be read, which falls through to a normal install.
    is_selected() {
      node - "$1" "$2" "$3" <<'NODE'
    const fs = require('node:fs')
    const [pkg, bundle, manifest] = process.argv.slice(2)
    let parsed
    try {
      parsed = JSON.parse(fs.readFileSync(manifest, 'utf8'))
    } catch {
      process.exit(2)
    }
    if (bundle === '1') {
      const bundles = parsed?.dsh?.profile?.bundles ?? []
      process.exit(bundles.includes(pkg) ? 0 : 1)
    }
    const deps = parsed.dependencies ?? {}
    process.exit(Object.hasOwn(deps, pkg) ? 0 : 1)
    NODE
    }

    maybe_install() {
      profile="$1"
      spec="$2"
      pkg="$3"
      bundle="$4"
      use_desktop="$5"

      manifest="$DSH_HOME/profiles/$profile/package.json"
      if [ -f "$manifest" ] && is_selected "$pkg" "$bundle" "$manifest"; then
        printf 'dsh: %s already present in profile %s\n' "$pkg" "$profile"
        return 0
      fi

      printf 'dsh: installing %s into profile %s\n' "$spec" "$profile"
      if [ "$use_desktop" = "1" ]; then
        # The npm-installed CLI cannot mutate the application-owned desktop
        # profile; only the copy bundled inside the app may. It runs while the
        # app is closed.
        cli="$(find "$desktop_root" -type f -path '*/runtime/cli/bin/dsh' -print -quit)"
        if [ -z "$cli" ]; then
          printf 'dsh: bundled CLI not found under %s\n' "$desktop_root" >&2
          exit 1
        fi
        "$cli" plugin --profile "$profile" add "$spec"
      else
        dsh plugin --profile "$profile" add "$spec"
      fi
    }

  ''
  + installLines;
}
