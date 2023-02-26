{ pkgs, colorscheme, ... }:
{
  programs.neovim = {
    enable = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      nvim-codeium
      # Appearance
      bufferline-nvim
      indent-blankline-nvim
      lualine-nvim
      alpha-nvim
      nvim-colorizer-lua
      nvim-headlines
      nvim-web-devicons

      # Appearance: Themes
      dracula-vim
      one-nvim
      tokyonight-nvim

      # DAP
      nvim-dap
      nvim-dap-python
      nvim-dap-ui

      # File Tree
      nvim-tree-lua

      # Fuzzy Finder
      cheatsheet-nvim
      nvim-better-digraphs
      telescope-fzf-native-nvim
      telescope-nvim

      # General Deps
      nui-nvim
      plenary-nvim
      popup-nvim

      # Git
      gitsigns-nvim
      vim-fugitive

      # Programming: LSP
      fidget-nvim
      lsp_signature-nvim
      lspkind-nvim
      null-ls-nvim
      nvim-lspconfig
      nvim-lspsaga
      nvim-sqls

      # Progrmming: Treesitter
      (nvim-treesitter.withPlugins (plugins: with plugins; [
        tree-sitter-bash
        tree-sitter-c
        tree-sitter-css
        tree-sitter-dockerfile
        tree-sitter-elm
        tree-sitter-go
        tree-sitter-haskell
        tree-sitter-hcl
        tree-sitter-html
        tree-sitter-java
        tree-sitter-javascript
        tree-sitter-json
        tree-sitter-latex
        tree-sitter-lua
        tree-sitter-markdown
        tree-sitter-markdown-inline
        tree-sitter-nix
        tree-sitter-python
        tree-sitter-regex
        tree-sitter-ruby
        tree-sitter-rust
        tree-sitter-scss
        tree-sitter-sql
        tree-sitter-toml
        tree-sitter-tsx
        tree-sitter-typescript
        tree-sitter-yaml
      ]))
      nvim-nu
      nvim-treesitter-refactor
      nvim-treesitter-textobjects
      which-key-nvim

      # Programming: Language support
      crates-nvim
      yuck-vim

      # Programming: Autocompletion setup
      nvim-cmp
      cmp-buffer
      cmp-calc
      cmp-nvim-lsp
      cmp-nvim-lua
      cmp-path
      cmp-treesitter
      cmp-vsnip
      vim-vsnip
      vim-vsnip-integ

      # Programming: Database support
      vim-dadbod
      vim-dadbod-ui

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

        # Docker
        nodePackages.dockerfile-language-server-nodejs
        hadolint

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

        # HTML/CSS/JS
        nodePackages.vscode-langservers-extracted

        # JavaScript
        nodePackages.typescript-language-server

        # lua
        luaformatter
        lua-language-server

        # Make
        /* cmake-language-server */

        # Markdown
        nodePackages.markdownlint-cli
        # This is a cli utility as we can't display all this in cli
        pandoc

        # Nix
        deadnix
        statix
        nil

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
      ] ++ (if pkgs.stdenv.isLinux then [
        # Grammer
        # Not available on mac using brew to install it
        ltex-ls
      ] else [

      ]);


    extraConfig = ''
      colorscheme ${colorscheme.vim-name}
      luafile ${builtins.toString ./init_lua.lua}
    '';
  };

  home.packages = with pkgs; [
    nodePackages.livedown

    # Haskell
    haskellPackages.haskell-language-server


    # Rust
    rust-analyzer
    rustfmt
    clippy

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
    "nvim/init_lua.lua".source = ./init_lua.lua;
  };

  home.file.".vale.ini".source = ./vale.ini;
  home.file.".markdownlintrc".source = ./markdown_lint.json;
}
