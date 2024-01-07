{ pkgs, ... }:
let nu_scripts = "${pkgs.nu_scripts}/share/nu_scripts";
in {
  programs.nushell = {
    enable = true;

    # crashes 
    # Error: nu::parser::module_not_found
    # 
    #   × Module not found.
    #    ╭─[/nix/store/lxi2bl5y0n94cg2mkasnrdg2fhd693gc-nu_scripts-unstable-2023-11-22/share/nu_scripts/modules/git/git-v2.nu:1:1]
    #  1 │ use argx.nu
    #    ·     ───┬───
    #    ·        ╰── module not found
    #  2 │
    #    ╰────
    #   help: module files and their paths must be available before your script is run as parsing occurs before anything is evaluated
    # 
    # use ${nu_scripts}/modules/git/git-v2.nu *
    extraConfig = ''
      ${builtins.readFile ./config.nu}
      ${builtins.readFile ./aliases.nu}

      # modules
      use ${nu_scripts}/modules/docker/docker.nu *
      use ${nu_scripts}/modules/nix/nix.nu *
      use ${./functions.nu} *

    '';
    extraEnv = ''
      ${builtins.readFile ./env.nu}
    '';
  };
}
