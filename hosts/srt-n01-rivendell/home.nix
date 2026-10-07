{ ... }:
{
  imports = [
    ../../profiles/homelab/home.nix
  ];

  home = {
    homeDirectory = "/home/nixypanda";
    username = "nixypanda";
    stateVersion = "25.11";
  };
}
