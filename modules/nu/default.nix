{ pkgs, ... }:
let
  nu_scripts = "${pkgs.nu_scripts}/share/nu_scripts";
in
{
  programs.nushell = {
    enable = true;

    extraConfig = ''
      ${builtins.readFile ./config.nu}
      ${builtins.readFile ./aliases.nu}

      # modules
      use ${nu_scripts}/modules/nix/nix.nu *

      ${builtins.readFile ./functions.nu}

    '';
    extraEnv = ''
      ${builtins.readFile ./env.nu}
    '';
  };
}
