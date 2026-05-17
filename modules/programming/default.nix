{ config, pkgs, ... }:

let
  inherit (config) xdg;

  ropify = pkgs.callPackage ./ropecli.nix { };

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

    # Python
    (python3.withPackages (
      ps: with ps; [
        setuptools
        pip
      ]
    ))
    pyright
    ruff
    ty

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
  programs.codex.enable = true;

  # Claude Code — HM module auto-manages CLAUDE_CONFIG_DIR when configDir ≠ ~/.claude
  programs.claude-code = {
    enable = true;
    configDir = "${xdg.configHome}/claude";
  };
}
