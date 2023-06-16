{ pkgs, ... }:
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

  update-dots = pkgs.writeScriptBin "update-dots" ''
    ${builtins.readFile ./update-dots.sh}
  '';
in {
  home.packages = if pkgs.stdenv.isLinux then [
    apply-system
    apply-user-nixos
    update-dots
  ] else [
    update-dots
    apply-user-mac
  ];
}
