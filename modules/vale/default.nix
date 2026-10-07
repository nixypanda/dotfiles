{ pkgs, ... }:
let
  vale_styles = import ../../scripts/vale-styles.nix { inherit pkgs; };
in
{
  home = {
    packages = [ pkgs.vale ];
    file = {
      ".config/vale/config.ini".source = ./vale.ini;
      ".local/share/vale/styles".source = vale_styles;
    };
  };
}
