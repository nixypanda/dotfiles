{ config, pkgs, libs, ... }:
{
  home.packages = with pkgs; [
    # C
    gcc

    # Clojure
    clojure
    clojure-lsp

    # Elm
    elmPackages.elm-language-server

    # go
    go
    gopls

    # Haskell
    ghc
    haskellPackages.cabal-install
    haskellPackages.stack
    haskellPackages.haskell-language-server

    # JavaScript
    nodejs
    nodePackages.livedown
    yarn

    # lua
    lua
    sumneko-lua-language-server

    # Nix
    rnix-lsp

    # python
    (python3.withPackages (ps: with ps; [ setuptools pip ]))
    poetry
    autoflake
    python3Packages.pip
    python3Packages.black
    python3Packages.ipython
    python3Packages.isort
    python3Packages.parso
    python3Packages.twine
    nodePackages.pyright

    # rust
    rustc
    rust-analyzer
    clippy
    cargo
    rustfmt
    # perl (this is required by rust)
    perl
  ];
}
