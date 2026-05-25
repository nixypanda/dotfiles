{ lib, pkgs, ... }:
{
  _module.args = {
    colorscheme = import ../../colorschemes/tokyonight.nix;
  };

  home = {
    homeDirectory = "/Users/nixypanda";
    username = "nixypanda";
    stateVersion = "25.11";
    packages = with pkgs; [
      google-chrome
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
    ../../modules/nu
    ../../modules/nvim
    ../../modules/programming
    ../../modules/system-management
  ];

  xdg.configFile."nix/nix.conf".text = ''
    experimental-features = nix-command flakes
  '';
}
