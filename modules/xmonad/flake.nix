{
  description = "XMonad Config";
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
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      taffybar-flake,
      ...
    }:
    let
      haskellDeps =
        ps: with ps; [
          xmonad
          xmonad-contrib
          xmonad-extras
          haskell-language-server
          taffybar
        ];
    in
    flake-utils.lib.simpleFlake {
      inherit self nixpkgs;
      inherit (taffybar-flake) overlay;
      name = "XMonad Dev environment";
      shell =
        {
          pkgs ? import <nixpkgs>,
        }:
        pkgs.mkShell { buildInputs = with pkgs; [ (ghc.withPackages haskellDeps) ]; };
    };
}
