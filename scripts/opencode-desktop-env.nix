{ pkgs }:
# The Darwin build of opencode-desktop exposes `bin/OpenCode`, which collides
# with the V2 CLI's `opencode` on macOS's case-insensitive filesystems and
# silently drops the CLI from PATH. Keep only the app bundle (copyApps picks
# up `$out/Applications`) so the CLI keeps the name. A buildEnv avoids
# rebuilding the Electron app just to drop its bin symlink.
pkgs.buildEnv {
  name = "opencode-desktop-app";
  paths = [ pkgs.opencode-desktop ];
  pathsToLink = [ "/Applications" ];
}
