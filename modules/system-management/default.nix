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
  build-forecast-user = pkgs.writeScriptBin "build-forecast" ''
    ${builtins.readFile ./build-forecast-user.sh})
  '';
in
{
  home.packages = [
    update-dots
    apply-user-mac
    apply-system-mac
    build-forecast-user
  ];
}
