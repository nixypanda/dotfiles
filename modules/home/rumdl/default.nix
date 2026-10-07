{ pkgs, ... }:
{
  home = {
    packages = [ pkgs.rumdl ];
    file.".config/rumdl/rumdl.toml".source = ./rumdl.toml;
  };
}
