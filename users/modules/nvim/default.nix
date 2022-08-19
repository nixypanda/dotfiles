{ config, pkgs, lib, colorscheme, ... }:
{
  home.packages = with pkgs;
    [
      # elm
      elmPackages.elm-language-server
      elmPackages.elm
      elmPackages.elm-test
      elmPackages.elm-format

      # grammer
      vale

      # Go
      gopls

      # Haskell
      haskellPackages.haskell-language-server

      # JavaScript
      nodePackages.typescript-language-server

      # lua
      luaformatter
      sumneko-lua-language-server

      # Markdown
      nodePackages.markdownlint-cli

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

      # general purpose / multiple langs
      efm-langserver
      nodePackages.prettier

      # shit you need to deal with
      nodePackages.bash-language-server
      shellcheck
      nodePackages.dockerfile-language-server-nodejs
      hadolint
      nodePackages.vscode-langservers-extracted
      nodePackages.vim-language-server
      nodePackages.yaml-language-server
      yamllint
      gitlint
      taplo-cli
      # This is a cli utility as we can't display all this in cli
      nodePackages.livedown
      pandoc

    ] ++ (if pkgs.stdenv.isLinux then [
      # Depends on pygls which does not build on darwin
      cmake-language-server
      # Not available on mac using brew to install it
      ltex-ls
    ] else [

    ]);

  programs.neovim = {
    enable = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      # Appearance
      indent-blankline-nvim
      bufferline-nvim
      nvim-tree-lua
      nvim-web-devicons
      lualine-nvim
      one-nvim
      dracula-vim
      nvim-alpha
      nvim-colorizer-lua
      tokyonight-nvim
      nvim-headlines

      # Programming
      which-key-nvim
      vim-haskellConcealPlus # vimscript
      nvim-yuck # vimscript
      nvim-nu
      lspkind-nvim
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
      nvim-lspconfig
      nvim-regexplainer
      nvim-lspsaga
      lsp_signature-nvim
      rust-tools-nvim
      fidget-nvim
      nvim-sqls
      crates-nvim
      null-ls-nvim
      ## Autocompletion setup
      nvim-cmp
      cmp-path
      cmp-vsnip
      cmp-buffer
      cmp-nvim-lsp
      cmp-nvim-lua
      cmp-treesitter
      cmp-calc
      vim-vsnip
      vim-vsnip-integ
      ## Project management
      project-nvim
      direnv-vim

      # Text objects
      tcomment_vim # vimscript
      vim-surround # vimscript
      vim-repeat # vimscript
      nvim-autopairs

      # Git
      vim-fugitive # vimscript
      gitsigns-nvim

      # DAP
      nvim-dap
      nvim-dap-ui
      nvim-dap-python

      # Fuzzy Finder
      telescope-nvim
      telescope-fzf-native-nvim
      nvim-better-digraphs
      cheatsheet-nvim

      # Text Helpers
      vim-table-mode # vimscript
      venn-nvim
      todo-comments-nvim

      # General Deps
      popup-nvim
      plenary-nvim
      nui-nvim
    ];

    extraConfig = ''
      ${builtins.readFile ./base-sane.vim}

      lua << EOF
        ${builtins.readFile ./base-sane.lua}
        ${builtins.readFile ./look-theme.lua}

        -- We do theme specific stuff in look-theme. This needs to come after that
        local statusline_theme = '${colorscheme.vim-statusline}'
        vim.cmd[[ colorscheme ${colorscheme.vim-name}]]

        ${builtins.readFile ./look-dashboard.lua}
        ${builtins.readFile ./look-colorizer.lua}
        ${builtins.readFile ./look-statusline.lua}
        ${builtins.readFile ./look-topline.lua}

        ${builtins.readFile ./ide-regexplainer.lua}
        ${builtins.readFile ./ide-treesitter.lua}
        ${builtins.readFile ./ide-completion.lua}
        ${builtins.readFile ./ide-lsp.lua}
        ${builtins.readFile ./ide-dap.lua}

        ${builtins.readFile ./git.lua}
        ${builtins.readFile ./todo.lua}
        ${builtins.readFile ./edit.lua}

        ${builtins.readFile ./nav-project.lua}
        ${builtins.readFile ./nav-nvim-tree.lua}
        ${builtins.readFile ./nav-telescope.lua}
        ${builtins.readFile ./nav-which_key.lua}
      EOF
    '';
  };
  home.file.".vale.ini".source = ./vale.ini;
  home.file.".markdownlintrc".source = ./markdown_lint.json;
}
