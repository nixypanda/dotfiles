{ ... }:
{
  imports = [
    ../../modules/home/cli.nix
    ../../modules/home/env.nix
    ../../modules/home/git.nix
    ../../modules/home/hledger.nix
    ../../modules/home/nvim/minimal.nix
    ../../modules/home/theme.nix
  ];

  home = {
    homeDirectory = "/home/nixypanda";
    username = "nixypanda";
    stateVersion = "25.11";
  };
}
