{ config, pkgs, lib, colorscheme, ... }:
{
  home.packages = with pkgs; [
    # elm
    elmPackages.elm-language-server
    elmPackages.elm
    elmPackages.elm-test
    elmPackages.elm-format

    # Go
    gopls

    # Haskell
    haskellPackages.haskell-language-server

    # JavaScript
    nodePackages.typescript-language-server

    # lua
    luaformatter

    # Nix
    rnix-lsp

    # python
    python3Packages.isort
    black
    python3Packages.black
    nodePackages.pyright

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
    nur.repos.crazazy.efm-langserver
    nodePackages.prettier

    # shit you need to deal with
    cmake-language-server
    nodePackages.bash-language-server
    nodePackages.dockerfile-language-server-nodejs
    nodePackages.vscode-langservers-extracted
    nodePackages.vim-language-server
    nodePackages.yaml-language-server
    # makrdown
    # This is a cli utility as we can't display all this in cli
    nodePackages.livedown
    pandoc
  ] ++ (lib.optional pkgs.stdenv.isLinux sumneko-lua-language-server);

  programs.neovim = {
    enable = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      # Appearance
      indent-blankline-nvim
      barbar-nvim
      nvim-tree-lua
      nvim-web-devicons
      lualine-nvim
      one-nvim
      dracula-vim
      # nvim-alpha
      nvim-colorizer-lua
      tokyonight-nvim

      # Programming
      which-key-nvim
      vim-haskellConcealPlus # vimscript
      nvim-yuck # vimscript
      lspkind-nvim
      nvim-treesitter
      nvim-treesitter-refactor
      nvim-treesitter-textobjects
      nvim-lspconfig
      nvim-lsp-saga
      lsp_signature-nvim
      rust-tools-nvim
      symbols-outline-nvim
      nvim-fidget
      nvim-sqls
      ## Autocompletion setup
      nvim-cmp
      cmp-path
      cmp-vsnip
      cmp-buffer
      cmp-nvim-lsp
      cmp-nvim-lua
      cmp-treesitter
      cmp-calc
      # copilot.vim
      # nvim-cmp-copilot
      vim-vsnip
      vim-vsnip-integ
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
      # nvim-dap-ui
      nvim-dap-python

      # Fuzzy Finder
      telescope-nvim
      telescope-fzf-native-nvim

      # Text Helpers
      vim-table-mode # vimscript
      venn-nvim
      todo-comments-nvim

      # General Deps
      popup-nvim
      plenary-nvim
    ];

    extraConfig = ''
      colorscheme ${colorscheme.vim-name}
      ${builtins.readFile ./base-sane.vim}
      ${builtins.readFile ./look-theme.vim}

      lua << EOF
        ${builtins.readFile ./base-sane.lua}

        local statusline_theme = '${colorscheme.vim-statusline}'

        ${builtins.readFile ./look-dashboard.lua}
        ${builtins.readFile ./look-colorizer.lua}
        ${builtins.readFile ./look-statusline.lua}

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
}
