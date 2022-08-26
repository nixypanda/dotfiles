{ config, pkgs, lib, colorscheme, ... }:
{
  home.packages = with pkgs;
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

      # Haskell
      haskellPackages.haskell-language-server

      # HTML/CSS/JS
      nodePackages.vscode-langservers-extracted

      # JavaScript
      nodePackages.typescript-language-server

      # lua
      luaformatter
      sumneko-lua-language-server

      # Make
      cmake-language-server

      # Markdown
      nodePackages.markdownlint-cli
      # This is a cli utility as we can't display all this in cli
      nodePackages.livedown
      pandoc

      # Nix
      rnix-lsp
      deadnix
      statix

      # python
      python3Packages.isort
      nodePackages.pyright
      black
      python3Packages.flake8
      mypy

      # Rust
      rust-analyzer
      rustfmt
      clippy
      # lldb # debugging setup

      # SQL
      sqls

      # terraform
      terraform-ls

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

  programs.neovim = {
    enable = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      # Appearance
      bufferline-nvim
      dracula-vim
      indent-blankline-nvim
      lualine-nvim
      nvim-alpha
      nvim-colorizer-lua
      nvim-headlines
      nvim-tree-lua
      nvim-web-devicons
      one-nvim
      tokyonight-nvim

      # DAP
      nvim-dap
      nvim-dap-python
      nvim-dap-ui

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
      vim-fugitive # vimscript

      # Programming
      crates-nvim
      fidget-nvim
      lsp_signature-nvim
      lspkind-nvim
      null-ls-nvim
      nvim-lspconfig
      nvim-lspsaga
      nvim-nu
      nvim-regexplainer
      nvim-sqls
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
        # tree-sitter-markdown_inline
        tree-sitter-nix
        tree-sitter-python
        tree-sitter-regex
        tree-sitter-ruby
        tree-sitter-rust
        tree-sitter-scss
        tree-sitter-toml
        tree-sitter-tsx
        tree-sitter-typescript
        tree-sitter-yaml
      ]))
      nvim-treesitter-refactor
      nvim-treesitter-textobjects
      nvim-yuck # vimscript
      rust-tools-nvim
      vim-haskellConcealPlus # vimscript
      which-key-nvim
      ## Autocompletion setup
      cmp-buffer
      cmp-calc
      cmp-nvim-lsp
      cmp-nvim-lua
      cmp-path
      cmp-treesitter
      cmp-vsnip
      nvim-cmp
      vim-vsnip
      vim-vsnip-integ

      ## Project management
      direnv-vim
      project-nvim

      # Text Helpers
      todo-comments-nvim
      venn-nvim
      vim-table-mode # vimscript

      # Text objects
      nvim-autopairs
      tcomment_vim # vimscript
      vim-repeat # vimscript
      vim-surround # vimscript
    ];

    extraConfig = ''
      colorscheme ${colorscheme.vim-name}
      luafile ${builtins.toString ./init_lua.lua}
    '';
  };

  xdg.configFile = {
    "nvim/lua" = {
      source = ./lua;
      recursive = true;
    };
  };

  home.file.".vale.ini".source = ./vale.ini;
  home.file.".markdownlintrc".source = ./markdown_lint.json;
}