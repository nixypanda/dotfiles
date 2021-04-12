{ config, pkgs, lib, ... }:
let
  apply-system = pkgs.writeScriptBin "apply-system" ''
    ${builtins.readFile ./apply-system.sh}
  '';
  apply-user = if (pkgs.stdenv.isDarwin)
  then (pkgs.writeScriptBin "apply-user" ''${builtins.readFile ./apply-user-mac.sh}'')
  else (pkgs.writeScriptBin "apply-user" ''${builtins.readFile ./apply-user-nixos.sh}'');
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
