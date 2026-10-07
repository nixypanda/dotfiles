{ pkgs }:
let
  # Give the experimental app bundle its own macOS identity so it can coexist
  # with the normal Kitty app in Home Manager Apps.
  kittyDevInfoPlistPatch = pkgs.writeText "kitty-dev-info-plist-patch.py" ''
    import plistlib
    import os
    from pathlib import Path

    plist_path = Path(os.environ["out"]) / "Applications/kitty-dev.app/Contents/Info.plist"
    with plist_path.open("rb") as f:
        info = plistlib.load(f)

    info["CFBundleIdentifier"] = "net.kovidgoyal.kitty.dev"
    info["CFBundleName"] = "kitty-dev"
    info["CFBundleDisplayName"] = "kitty-dev"

    with plist_path.open("wb") as f:
        plistlib.dump(info, f)
  '';

  # GUI entry point: Home Manager copies Applications/kitty-dev.app into
  # ~/Applications/Home Manager Apps for Finder, Spotlight, and Dock launches.
  kittyDevApp = pkgs.runCommand "kitty-dev-app" { } ''
    mkdir -p "$out/Applications"
    cp -R "${pkgs.kitty-dev}/Applications/kitty.app" "$out/Applications/kitty-dev.app"
    chmod -R u+w "$out/Applications/kitty-dev.app"

    "${pkgs.python3}/bin/python3" "${kittyDevInfoPlistPatch}"
  '';

  # CLI entry point: expose a `kitty-dev` command that forwards arguments to
  # the experimental Kitty binary.
  kittyDevBin = pkgs.writeShellScriptBin "kitty-dev" ''
    exec "${pkgs.kitty-dev}/bin/kitty" "$@"
  '';
in
{
  inherit kittyDevApp kittyDevBin;
}
