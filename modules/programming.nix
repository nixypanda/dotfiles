{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # docker
    docker
    docker-compose

    # Haskell
    cabal2nix
    ghc
    haskellPackages.cabal-install
    haskellPackages.stack

    # lua
    lua

    # python
    (python3.withPackages (
      ps: with ps; [
        setuptools
        pip
      ]
    ))
    poetry
    uv
    python3Packages.ipython

    # rust
    cargo
    cargo-tarpaulin
    perl # this is required by rust
    rustc
  ];
}
