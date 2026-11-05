{ pkgs, ... }:

let
  # Personal CLI wrapper over Rope for Python refactors.
  ropify = pkgs.callPackage ../../pkgs/ropecli.nix { };

  # OpenCode — nixpkgs only packages the V1 CLI, so Darwin hosts build the V2
  # binary from opencode.ai (see ../../pkgs/opencode.nix); Linux hosts keep pkgs.opencode.
  opencode =
    if pkgs.stdenv.hostPlatform.isDarwin then
      pkgs.callPackage ../../pkgs/opencode.nix { }
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
    elmPackages.elm-format
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
    # Keep the package here and let programs.codex manage its settings.
    codex
  ];

  programs.codex = {
    enable = true;
    package = null;
    settings = {
      sandbox_mode = "workspace-write";
      approval_policy = "on-request";
      approvals_reviewer = "auto_review";
      tui = {
        notifications = [
          "agent-turn-complete"
          "approval-requested"
        ];
        notification_method = "bel";
        notification_condition = "always";
      };
    };
  };
}
