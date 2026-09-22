{
  lib,
  pkgs,
  ownai,
  ...
}:
let
  # The Darwin build of opencode-desktop exposes `bin/OpenCode`, which collides
  # with the V2 CLI's `opencode` on macOS's case-insensitive filesystems and
  # silently drops the CLI from PATH. Keep only the app bundle (copyApps picks
  # up `$out/Applications`) so the CLI keeps the name. A buildEnv avoids
  # rebuilding the Electron app just to drop its bin symlink.
  opencode-desktop = pkgs.buildEnv {
    name = "opencode-desktop-app";
    paths = [ pkgs.opencode-desktop ];
    pathsToLink = [ "/Applications" ];
  };
in
{
  _module.args = {
    colorscheme = import ../../colorschemes/tokyonight.nix;
  };

  home = {
    homeDirectory = "/Users/nixypanda";
    username = "nixypanda";
    stateVersion = "25.11";
    packages = with pkgs; [
      bitwarden-desktop
      google-chrome
      opencode-desktop
      # Personal app, intentionally Mac-only; not expected to build on the
      # Linux hosts.
      ownai.packages.${pkgs.system}.default
    ];
  };

  nixpkgs.config = {
    allowUnfreePredicate =
      pkg:
      builtins.elem (lib.getName pkg) [
        "zoom"
        "claude-code"
        "google-chrome"
        "firefox-bin"
        "firefox-bin-unwrapped"
        "vim-table-mode"
      ];
  };

  imports = [
    ../../modules/cli.nix
    ../../modules/env.nix
    ../../modules/firefox
    ../../modules/fonts.nix
    ../../modules/git
    ../../modules/kitty
    ../../modules/kitty/dev.nix
    ../../modules/nu
    ../../modules/nvim
    ../../modules/programming
    ../../modules/system-management
  ];

  xdg.configFile."nix/nix.conf".text = ''
    experimental-features = nix-command flakes
  '';
}
