{ config, pkgs, lib, colorscheme, ... }:
{
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    stdlib = ''
      ${builtins.readFile ./project_layouts/poetry.sh}
    '';
  };
}
