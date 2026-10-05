{
  lib,
  pkgs,
  codect,
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
      codect.packages.${pkgs.system}.default
    ];
  };

  programs.dsh = {
    enable = true;

    # Home-level DSH operator patch layer. DSH composes a profile tree from, in
    # order: each bundle's patch, the profile's own cordis.patch.yml (the file
    # the Models UI writes), this home-level layer, then any --patch overlays.
    # Because a later layer replaces the whole entry config, this restates the
    # full llm-pi-ai provider.
    #
    # OpenCode Go is a built-in `dsh-llm-pi-ai` catalog route, so the installed
    # pi-ai catalog already supplies the base URL, wire protocol, and model
    # list. We only add the credential reference and the session header OpenCode
    # Go uses to route and prompt-cache requests. Without x-opencode-session the
    # adapter sends no session id, so Go cannot cache the conversation prefix
    # and monthly usage is consumed roughly 20x faster than the published
    # estimates.
    patch = [
      {
        id = "llm-pi-ai";
        config.providers.opencode-go = {
          apiKeyEnv = "OPENCODE_GO_API_KEY";
          headers.x-opencode-session = "e277fced-b8d2-4a41-9162-c71d8692c04d";
        };
      }
    ];

    # Official signed/notarized Electron shell, installed from the pinned
    # nightly-channel ZIP; see ../programming/dsh/desktop.nix.
    desktop.enable = true;
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
