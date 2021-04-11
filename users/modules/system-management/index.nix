{ config, pkgs, libs, ... }:
let
  apply-system = pkgs.writeScriptBin "apply-system" ''
    ${builtins.readFile ./apply-system.sh}
  '';
  apply-user = pkgs.writeScriptBin "apply-user" ''
    ${builtins.readFile ./apply-user.sh}
  '';
  update-system = pkgs.writeScriptBin "update-system" ''
    ${builtins.readFile ./update-system.sh}
  '';
  update-user = pkgs.writeScriptBin "update-user" ''
    ${builtins.readFile ./update-user.sh}
  '';
in
{
  home.packages = [
    apply-system
    apply-user
    update-system
    update-user
  ];
}
