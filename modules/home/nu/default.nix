{ pkgs, nu-program-status, ... }:
let
  nu_scripts = "${pkgs.nu_scripts}/share/nu_scripts";
  program_status = builtins.path {
    path = "${nu-program-status}/nu";
    name = "nu-program-status";
  };
in
{
  programs.nushell = {
    enable = true;
    loadSessionVariables = false;

    extraConfig = ''
      ${builtins.readFile ./config.nu}
      ${builtins.readFile ./aliases.nu}

      use ${nu_scripts}/modules/nix/nix.nu *
      use ${nu_scripts}/custom-completions/uv/uv-completions.nu *

      ${builtins.readFile ./functions.nu}

      use ${program_status}/mod.nu *
      use ${program_status}/hooks.nu *
      program-status install-hooks --min-duration 10sec

    '';
    extraEnv = ''
      ${builtins.readFile ./env.nu}
    '';
  };
}
