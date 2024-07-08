{ pkgs, ... }: {
  home.packages = with pkgs; [
    # common
    gnumake

    # C
    gcc

    # Clojure
    clojure
    leiningen

    # dhall
    dhall
    haskellPackages.dhall-json
    haskellPackages.dhall-yaml

    # docker
    docker
    docker-compose

    # go
    go
    golangci-lint

    # Haskell
    cabal2nix
    ghc
    haskellPackages.cabal-install
    haskellPackages.stack

    # Java
    jdk17
    maven

    # JavaScript
    nodejs
    yarn
    nodePackages.prettier

    # lua
    lua

    # python
    (python3.withPackages (ps: with ps; [ setuptools pip ]))
    poetry
    python3Packages.ipython

    # rust
    cargo
    cargo-tarpaulin
    perl # this is required by rust
    rustc

    # shell
    shfmt
  ];
}
