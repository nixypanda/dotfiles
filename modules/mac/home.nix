{
  lib,
  pkgs,
  codect,
  ...
}:
let
  opencode-desktop = import ../../scripts/opencode-desktop-env.nix { inherit pkgs; };
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
      nerd-fonts.hack
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
    # nightly-channel ZIP; see ../../pkgs/deepseek-harness-desktop.nix.
    desktop.enable = true;

    # Codect sidebar plugin: focused projection (Show) and focused diff
    # (Diff) tabs in the right sidebar. The bundle is built by the codect flake
    # (`packages.<system>.codect-dsh`) and installed into the desktop profile.
    # `binary` is pinned to the Nix store path because the Desktop app does not
    # inherit the login shell's PATH.
    plugins."@nixypanda/dsh-codect" = {
      spec = "${codect.packages.${pkgs.system}.codect-dsh}/codect-dsh.tgz";
      id = "codect";
      profiles = [ "desktop" ];
      config = {
        binary = "${codect.packages.${pkgs.system}.default}/bin/codect";
      };
    };
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
    ../../modules/claude
    ../../modules/cli.nix
    ../../modules/dsh.nix
    ../../modules/env.nix
    ../../modules/firefox
    ../../modules/git.nix
    ../../modules/hledger.nix
    ../../modules/kitty
    ../../modules/kitty/dev.nix
    ../../modules/nu
    ../../modules/nvim
    ../../modules/programming.nix
    ../../modules/rumdl
    ../../modules/vale
    ../../scripts/system-management
  ];

  xdg.configFile."nix/nix.conf".text = ''
    experimental-features = nix-command flakes
  '';
}
