{ pkgs, colorscheme, codelldb_fixed_pkgs, ... }:
let
  # Using a fixed version of codelldb which works on Mac.
  # We get this from an alternate nixpkgs repo.
  # Caveat: This requires Xcode.app installed on the system
  # NOTE: https://github.com/NixOS/nixpkgs/pull/211321
  code_lldb = codelldb_fixed_pkgs.vscode-extensions.vadimcn.vscode-lldb;
  tree-sitter-nu = pkgs.callPackage ./nvim-treesitter-nu.nix {
    inherit (pkgs.tree-sitter) buildGrammar;
  };
in {
  programs.neovim = {
    enable = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      # Appearance
      bufferline-nvim
      indent-blankline-nvim
      lualine-nvim
      alpha-nvim
      nvim-colorizer-lua
      headlines-nvim
      nvim-web-devicons
      noice-nvim
      statuscol-nvim
      nvim-ufo

      # Appearance: Themes
      dracula-vim
      one-nvim
      tokyonight-nvim
      catppuccin-nvim

      # DAP
      nvim-dap
      nvim-dap-python
      nvim-dap-ui
      nvim-dap-go

      # Fuzzy Finder
      telescope-fzf-native-nvim
      telescope-nvim
      telescope-ui-select-nvim

      # Git
      gitsigns-nvim
      vim-fugitive

      # Navigation
      nvim-tree-lua
      vim-tmux-navigator

      # Programming: LSP
      lspkind-nvim
      null-ls-nvim
      nvim-lspconfig
      lspsaga-nvim
      nvim-sqls
      nvim-conform

      # Progrmming: Treesitter
      (nvim-treesitter.withPlugins (plugins:
        with plugins; [
          bash
          c
          css
          dhall
          dockerfile
          elixir
          elm
          go
          haskell
          hcl
          html
          java
          javascript
          json
          latex
          lua
          markdown
          markdown-inline
          nix
          python
          regex
          regex
          ruby
          rust
          scss
          sql
          terraform
          toml
          tsx
          typescript
          vim
          yaml

          tree-sitter-nu.grammar
        ]))
      nvim-treesitter-refactor
      nvim-treesitter-textobjects
      which-key-nvim

      # Programming: Language support
      crates-nvim
      yuck-vim
      rust-tools-nvim
      haskell-tools-nvim

      # Programming: Autocompletion setup
      nvim-cmp
      cmp-buffer
      cmp-calc
      cmp-cmdline
      cmp-nvim-lsp
      cmp-nvim-lua
      cmp-path
      cmp-treesitter
      cmp-vsnip
      vim-vsnip
      vim-vsnip-integ

      # Programming: AI shit
      codeium-vim # AI completion prediction
      ChatGPT-nvim

      # Programming: Code Evaluation
      conjure

      # Programming: Database support
      vim-dadbod
      vim-dadbod-ui

      # Programming: Testing
      FixCursorHold-nvim
      neotest
      neotest-python
      neotest-rust
      neotest-go

      ## Project management
      direnv-vim
      project-nvim

      # Text Helpers
      nvim-regexplainer
      todo-comments-nvim
      venn-nvim
      vim-haskellConcealPlus
      vim-table-mode

      # Text objects
      nvim-autopairs
      nvim-comment
      nvim-surround
    ];

    extraPackages = with pkgs;
      [
        # Bash
        nodePackages.bash-language-server
        shellcheck
        shfmt

        # Clojure
        clojure-lsp

        # dhall (broken on nix unstable)
        # dhall-lsp-server

        # Docker
        nodePackages.dockerfile-language-server-nodejs
        # hadolint # (broken)

        # elm
        elmPackages.elm-language-server
        elmPackages.elm
        elmPackages.elm-test
        elmPackages.elm-format

        # grammer
        vale

        # Git
        gitlint

        # Go
        gopls
        delve

        # HTML/CSS/JS
        nodePackages.vscode-langservers-extracted

        # Java
        java-language-server

        # JavaScript
        nodePackages.typescript-language-server

        # lua
        luaformatter
        lua-language-server

        # Make
        # cmake-language-server

        # Markdown
        nodePackages.markdownlint-cli
        # This is a cli utility as we can't display all this in cli
        pandoc

        # Nix
        deadnix
        statix
        nil
        nixfmt

        # rust
        code_lldb

        # SQL
        sqls
        postgresql

        # terraform
        terraform-lsp

        # TOML
        taplo-cli

        # Vimscript
        nodePackages.vim-language-server

        # YAML
        nodePackages.yaml-language-server
        yamllint

        # general purpose / multiple langs
        efm-langserver
        nodePackages.prettier

        # utilities used by various programs
        # telescope
        ripgrep
        fd

      # general purpose / multiple langs
      efm-langserver
      nodePackages.prettier

        ]);

    extraConfig = ''
      colorscheme catppuccin-macchiato
      luafile ${builtins.toString ./init_lua.lua}
    '';
  };

  home.packages = with pkgs; [
    nodePackages.livedown
    # Rust
    rust-analyzer
    rustfmt
    clippy
    evcxr

    # Haskell
    haskellPackages.haskell-language-server
    haskellPackages.hoogle
    haskellPackages.fast-tags
    haskellPackages.haskell-debug-adapter
    haskellPackages.ghci-dap

    # python
    python3Packages.isort
    nodePackages.pyright
    black
    python3Packages.flake8
    mypy
  ];

  xdg.configFile = {
    "nvim/lua" = {
      source = ./lua;
      recursive = true;
    };
    "nvim/queries/nu/highlights.scm".text = tree-sitter-nu.highlights;
    "nvim/queries/nu/injections.scm".text = tree-sitter-nu.injections;

    "nvim/lua/injected.lua".text = ''
      local extension_path = '${code_lldb}/share/vscode/extensions/vadimcn.vscode-lldb/'
      return {
          codelldb_path = extension_path .. 'adapter/codelldb',
          liblldb_path = extension_path .. 'lldb/lib/liblldb.dylib'
      }
    '';
    "nvim/init_lua.lua".source = ./init_lua.lua;
  };

  home.file.".vale.ini".source = ./vale.ini;
  home.file.".markdownlintrc".source = ./markdown_lint.json;
}
