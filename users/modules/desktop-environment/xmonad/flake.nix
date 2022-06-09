{
  description = "XMonad Config";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    taffybar.url = "github:sherubthakur/taffybar";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs @ { self, nixpkgs, flake-utils, taffybar, ... }:
    let
      haskellDeps = ps: with ps; [
        xmonad
        xmonad-contrib
        xmonad-extras
        haskell-language-server
        taffybar
      ];
    in
    flake-utils.lib.simpleFlake {
      inherit self nixpkgs;
      name = "XMonad Dev environment";
      overlay = taffybar.overlay;
      shell = ({ pkgs ? import <nixpkgs> }:
        pkgs.mkShell {
          buildInputs = with pkgs; [ (ghc.withPackages haskellDeps) ];
        });
    };
}
