{ pkgs, ... }: {
  home.packages = with pkgs; [
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

    # lua
    lua

    # python
    (python3.withPackages (ps: with ps; [ setuptools pip debugpy ]))
    autoflake
    poetry
    pipenv
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
