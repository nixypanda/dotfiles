{
  lib,
  pkgs,
  codect,
  ...
}:
{
  home = {
    homeDirectory = "/Users/nixypanda";
    username = "nixypanda";
    stateVersion = "25.11";
    packages = with pkgs; [
      bitwarden-desktop
      google-chrome
      nerd-fonts.hack
      # Personal app, intentionally Mac-only; not expected to build on the
      # Linux hosts.
      codect.packages.${pkgs.system}.default
    ];
  };

  nixpkgs.config = {
    allowUnfreePredicate =
      pkg:
      builtins.elem (lib.getName pkg) [
        "claude-code"
        "google-chrome"
        "firefox-bin"
        "firefox-bin-unwrapped"
      ];
  };

  imports = [
    ../../modules/home/claude
    ../../modules/home/cli.nix
    ../../modules/home/dsh
    ../../modules/home/env.nix
    ../../modules/home/firefox
    ../../modules/home/git.nix
    ../../modules/home/hledger.nix
    ../../modules/home/kitty
    ../../modules/home/kitty/dev.nix
    ../../modules/home/nu
    ../../modules/home/nvim
    ../../modules/home/opencode
    ../../modules/home/programming.nix
    ../../modules/home/rumdl
    ../../modules/home/system-management
    ../../modules/home/theme.nix
    ../../modules/home/vale
  ];

  xdg.configFile."nix/nix.conf".text = ''
    experimental-features = nix-command flakes
  '';
}
