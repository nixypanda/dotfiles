{
  agent-skills,
  config,
  pkgs,
  ...
}:

let
  inherit (config) xdg;

  elmcraft = agent-skills + "/elmcraft";
  grill-me = agent-skills + "/grill-me";
  cut-the-crap = agent-skills + "/cut-the-crap";

  # hledger-lsp is not packaged in nixpkgs yet.
  hledger-lsp = pkgs.callPackage ./hledger-lsp.nix { };

  # Personal CLI wrapper over Rope for Python refactors.
  ropify = pkgs.callPackage ./ropecli.nix { };

  # OpenCode — nixpkgs only packages the V1 CLI, so Darwin hosts build the V2
  # binary from opencode.ai (see opencode.nix); Linux hosts keep pkgs.opencode.
  opencode =
    if pkgs.stdenv.hostPlatform.isDarwin then pkgs.callPackage ./opencode.nix { } else pkgs.opencode;

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

    # AI coding assistants
    opencode
    codex

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
    ".config/opencode/skills/elmcraft".source = elmcraft;
    ".config/opencode/commands/elmcraft.md".source = elmcraft + "/shims/opencode-command.md";
    ".config/opencode/skills/grill-me".source = grill-me;
    ".config/opencode/commands/grill-me.md".source = grill-me + "/shims/opencode-command.md";

    # Codex resolves this from $HOME, not $CODEX_HOME, and treats
    # $CODEX_HOME/skills as deprecated (codex-rs/ext/skills/src/host_roots.rs).
    # programs.codex.skills writes the deprecated path, so bypass the module.
    ".agents/skills/elmcraft".source = elmcraft;
    ".agents/skills/grill-me".source = grill-me;
  };

  # Codex deliberately keeps its default ~/.codex. Its auth is a file
  # ($CODEX_HOME/auth.json, unlike Claude Code which uses the macOS Keychain)
  # and it holds seven sqlite databases, so pointing CODEX_HOME elsewhere logs
  # you out and hides all local state. Upstream has no XDG support to migrate
  # toward, so programs.codex is skipped entirely and the package is installed
  # plainly above: the module's only remaining effect here would be exporting
  # that CODEX_HOME.

  programs.claude-code = {
    enable = true;
    configDir = "${xdg.configHome}/claude";

    skills = {
      inherit elmcraft grill-me cut-the-crap;
    };

    context = ./claude/CLAUDE.md;

    # Written into a synthetic plugin dir passed as --plugin-dir, not into
    # .claude.json, so this coexists with the state file Claude Code mutates.
    mcpServers = {
      sentry = {
        type = "http";
        url = "https://mcp.sentry.dev/mcp";
      };
      atlassian = {
        type = "sse";
        url = "https://mcp.atlassian.com/v1/sse";
      };
    };

    # Whole-file store symlink, so Claude Code can no longer persist anything
    # here: /model, /fast and /config stop sticking. Change them by editing
    # this and rebuilding.
    settings = {
      includeCoAuthoredBy = false;
      permissions.allow = [ "Bash(*)" ];
      model = "opus[1m]";
      enabledPlugins = {
        "typescript-lsp@claude-plugins-official" = true;
      };
      effortLevel = "medium";
      tui = "fullscreen";
    };
  };
}
