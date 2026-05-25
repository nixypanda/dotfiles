{ pkgs, ... }:
let
  apply-darwin = pkgs.writeScriptBin "apply-darwin" ''
    ${builtins.readFile ./apply-darwin.sh}
  '';
  apply-user = pkgs.writeScriptBin "apply-user" ''
    ${builtins.readFile ./apply-user.sh}
  '';
  apply-rivendell = pkgs.writeScriptBin "apply-rivendell" ''
    ${builtins.readFile ./apply-rivendell.sh}
  '';
  update-flake = pkgs.writeScriptBin "update-flake" ''
    ${builtins.readFile ./update-flake.sh}
  '';
  forecast-build = pkgs.writeScriptBin "forecast-build" ''
    ${builtins.readFile ./forecast-build.sh}
  '';
  sync-hledger = pkgs.writeScriptBin "sync-hledger" ''
    ${builtins.readFile ./sync-hledger.sh}
  '';
in
{
  home.packages = [
    apply-darwin
    apply-user
    apply-rivendell
    update-flake
    forecast-build
    sync-hledger
  ];
}
