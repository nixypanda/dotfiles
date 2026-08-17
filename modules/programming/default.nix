{
  config,
  nixpkgs-unstable,
  pkgs,
  ...
}:

let
  inherit (config) xdg;

  # hledger-lsp is not packaged in nixpkgs yet.
  hledger-lsp = pkgs.callPackage ./hledger-lsp.nix { };

  # Personal CLI wrapper over Rope for Python refactors.
  ropify = pkgs.callPackage ./ropecli.nix { };

  # The upstream GitHub bundle avoids a long source build on Intel Darwin.
  unstable_codex = pkgs.callPackage ./codex-bin.nix { };

  # Unstable no longer evaluates its package set for Intel Darwin. Reuse the
  # current Claude recipe with the supported 26.05 Darwin package set.
  unstable_claude_code =
    (pkgs.callPackage (nixpkgs-unstable + "/pkgs/by-name/cl/claude-code/package.nix") { }).overrideAttrs
      (old: {
        # The unstable manifest still publishes a darwin-x64 binary, even
        # though x86_64-darwin was removed from the package metadata.
        meta = old.meta // {
          platforms = old.meta.platforms ++ [ "x86_64-darwin" ];
        };
      });

  # OpenCode — pre-built binary for Intel Mac (nixpkgs doesn't support x86_64-darwin).
  opencode =
    if pkgs.stdenv.hostPlatform.system == "x86_64-darwin" then
      pkgs.callPackage ./opencode.nix { }
    else
      pkgs.opencode;

  # Build one immutable Vale styles tree from packaged style sets. Vale expects
  # vocabulary files to exist, so create empty Base vocab files instead of
  # relying on mutable setup under $HOME.
  vale_styles =
    pkgs.runCommand "vale-styles"
      {
        buildInputs = with pkgs.valeStyles; [
          alex
          proselint
          write-good
        ];
      }
      ''
        mkdir -p $out/config/vocabularies/Base
        touch $out/config/vocabularies/Base/accept.txt
        touch $out/config/vocabularies/Base/reject.txt
        for pkg in $buildInputs; do
          cp -rs "$pkg/share/vale/styles/"* "$out/"
        done
      '';
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

    # Hledger
    hledger-fmt
    hledger-lsp

    # Python
    (python3.withPackages (
      ps: with ps; [
        setuptools
        pip
      ]
    ))
    pyright
    ruff
    # ty

    # Shell
    shellcheck
    shfmt
    bash-language-server

    # Docker (language server)
    dockerfile-language-server

    # HTML/CSS/JS
    vscode-langservers-extracted
    typescript-language-server

    # Lua
    lua-language-server
    stylua

    # Make
    cmake-language-server

    # Nix
    nixd
    deadnix
    statix
    nixfmt

    # Terraform
    terraform-lsp

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

    # SQL
    postgresql

    # Git / Build tools
    gitlint
    just

    # AI coding assistant (pre-built binary from GitHub releases)
    opencode

    # Prose / Markdown
    vale
    markdownlint-cli
    # General purpose / multiple langs
    prettier
  ];

  home.file = {
    ".config/vale/config.ini".source = ./vale.ini;
    ".local/share/vale/styles".source = vale_styles;
    ".config/markdownlint/config.json".source = ./markdown_lint.json;
  };

  # Codex — HM module auto-manages CODEX_HOME via preferXdgDirectories
  programs.codex = {
    enable = true;
    package = unstable_codex;
  };

  # Claude Code — HM module auto-manages CLAUDE_CONFIG_DIR when configDir ≠ ~/.claude
  programs.claude-code = {
    enable = true;
    configDir = "${xdg.configHome}/claude";
    package = unstable_claude_code;
  };
}
