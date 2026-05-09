{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # docker
    docker
    docker-compose

    # Haskell
    ghc

    # python
    (python3.withPackages (
      ps: with ps; [
        setuptools
        pip
      ]
    ))
    claude-code
    codex
  ];
}
