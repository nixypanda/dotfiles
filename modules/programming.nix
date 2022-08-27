{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # C
    gcc

    # Clojure
    clojure

    # docker
    docker

    # go
    go
    golangci-lint

    # Haskell
    cabal2nix
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
    autoflake
    poetry
    python3Packages.ipython
    python3Packages.parso
    python3Packages.twine

    # rust
    cargo
    cargo-tarpaulin
    perl # this is required by rust
    rustc
  ];
}
