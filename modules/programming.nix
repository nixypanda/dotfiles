{ pkgs, ... }:

let
  # Personal CLI wrapper over Rope for Python refactors.
  ropify = pkgs.callPackage ../pkgs/ropecli.nix { };

  # OpenCode — nixpkgs only packages the V1 CLI, so Darwin hosts build the V2
  # binary from opencode.ai (see ../pkgs/opencode.nix); Linux hosts keep pkgs.opencode.
  opencode =
    if pkgs.stdenv.hostPlatform.isDarwin then
      pkgs.callPackage ../pkgs/opencode.nix { }
    else
      pkgs.opencode;
in
{
  home.packages = with pkgs; [
    # Docker
    docker
    docker-compose
    hadolint
    ropify

    # Rust
    rust-analyzer
    rustfmt
    clippy

    # Haskell
    ghc
    haskellPackages.haskell-language-server
    haskellPackages.hoogle
    haskellPackages.fast-tags
    haskellPackages.cabal-gild
    haskellPackages.hlint

    # Python
    (python3.withPackages (
      ps: with ps; [
        setuptools
        pip
      ]
    ))
    ruff
    # ty

    # Shell
    shellcheck
    shfmt
    bash-language-server

    # Docker (language server)
    dockerfile-language-server

    # HTML/CSS/JS
    biome # vscode-langservers-extracted, typescript-language-server, prettier

    # Lua
    lua-language-server
    stylua

    # Make
    neocmakelsp # cmake-language-server

    # Nix
    nixd
    deadnix
    statix
    nixfmt

    # Elm
    # elmPackages.elm
    elmPackages.elm-language-server
    # elmPackages.elm-format
    # elmPackages.elm-test
    # elmPackages.elm-review

    # TOML
    taplo

    # YAML
    yaml-language-server
    yamllint
    yamlfmt # prettier (yaml)

    # SQL
    postgresql

    # Git / Build tools
    committed # gitlint
    just

    # AI coding assistants
    opencode
    # Codex deliberately keeps its default ~/.codex. Its auth is a file
    # ($CODEX_HOME/auth.json, unlike Claude Code which uses the macOS Keychain)
    # and it holds seven sqlite databases, so pointing CODEX_HOME elsewhere logs
    # you out and hides all local state. Upstream has no XDG support to migrate
    # toward, so programs.codex is skipped entirely and the package is installed
    # plainly here: the module's only remaining effect would be exporting
    # that CODEX_HOME.
    codex
  ];
}
