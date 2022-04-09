{ config, pkgs, lib, colorscheme, ... }:
{
  home.packages = with pkgs; [
    # C
    gcc

    # Clojure
    clojure

    # go
    go
    golangci-lint

    # Haskell
    ghc
    haskellPackages.cabal-install
    haskellPackages.stack

    # JavaScript
    nodejs
    yarn

    # lua
    lua

    # python
    (python3.withPackages (ps: with ps; [ setuptools pip debugpy ]))
    poetry
    autoflake
    python3Packages.ipython
    python3Packages.parso
    python3Packages.twine

    # rust
    rustc
    cargo
    cargo-tarpaulin
    perl # perl (this is required by rust)
  ];
}
