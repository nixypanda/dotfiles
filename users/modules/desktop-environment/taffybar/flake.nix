{
  description = "Taffybar Config";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    taffybar.url = "github:sherubthakur/taffybar";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs @ { self, nixpkgs, flake-utils, taffybar, ... }:
    let
      haskellDeps = ps: with ps; [
        gtk3
        haskell-language-server
        taffybar
        cabal-install
      ];
    in
    flake-utils.lib.simpleFlake {
      inherit self nixpkgs;
      name = "Taffybar Dev environment";
      # overlay = taffybar.overlay;
      shell = { pkgs ? import <nixpkgs> }:
        pkgs.mkShell {
          buildInputs = with pkgs; [ (ghc.withPackages haskellDeps) ];
        };
    };
}

