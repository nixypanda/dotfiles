{ pkgs, ... }:
let
  apply-system-mac = pkgs.writeScriptBin "apply-system" ''
    ${builtins.readFile ./apply-system-mac.sh}
  '';
  apply-user-mac = pkgs.writeScriptBin "apply-user" ''
    ${builtins.readFile ./apply-user-mac.sh}
  '';
  update-dots = pkgs.writeScriptBin "update-dots" ''
    ${builtins.readFile ./update-dots.sh}
  '';
in
{
  home.packages = [
    update-dots
    apply-user-mac
    apply-system-mac
  ];
}
