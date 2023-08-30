{ pkgs, lib, ... }:
let
  completions = [ "btm" "git" "cargo" "man" "nix" "poetry" "zellij" ];
  mk_completion_str = c:
    "use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/${c}/${c}-completions.nu *";
in {
  programs.nushell = {
    enable = true;
    extraConfig = ''
      ${builtins.readFile ./config.nu}
      ${builtins.readFile ./aliases.nu}
      # completions
      ${lib.concatStringsSep "\n" (builtins.map mk_completion_str completions)}
    '';
    extraEnv = ''
      ${builtins.readFile ./env.nu}
    '';
  };
}
