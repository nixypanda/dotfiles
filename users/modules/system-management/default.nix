{ config, pkgs, lib, ... }:
let
  apply-system = pkgs.writeScriptBin "apply-system" ''
    ${builtins.readFile ./apply-system.sh}
  '';

  apply-user-mac = pkgs.writeScriptBin "apply-user" ''
    ${builtins.readFile ./apply-user-mac.sh}
  '';

  apply-user-nixos = pkgs.writeScriptBin "apply-user" ''
    ${builtins.readFile ./apply-user-nixos.sh}
  '';

  update-system = pkgs.writeScriptBin "update-system" ''
    ${builtins.readFile ./update-system.sh}
  '';

  update-user = pkgs.writeScriptBin "update-user" ''
    ${builtins.readFile ./update-user.sh}
  '';

in
{
  home.packages =
    if pkgs.stdenv.isLinux then [
      apply-system
      apply-user-nixos
      update-system
      update-user
    ] else [
      update-user
      apply-user-mac
    ];
}
