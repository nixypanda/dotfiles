{ pkgs ? import <nixpkgs> {}}:
pkgs.mkShell {
  buildInputs = with pkgs; [
    pkg-config
    cairo
    gobject-introspection
  ];
}
