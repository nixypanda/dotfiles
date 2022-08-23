{
  description = "Taffybar Config";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    taffybar-flake = {
      url = "github:sherubthakur/taffybar";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = inputs @ { self, nixpkgs, flake-utils, taffybar-flake, ... }:
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
      inherit (taffybar-flake) overlay;

      name = "Taffybar Dev environment";
      shell = { pkgs ? import <nixpkgs> }:
        pkgs.mkShell {
          buildInputs = with pkgs; [ (ghc.withPackages haskellDeps) ];
        };
    };
}

