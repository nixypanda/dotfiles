{ config, pkgs, lib, colorscheme, ... }:
let
  vimPlugsFromSource = (import ./plugins.nix) pkgs;
in
{
  programs.neovim = {
    enable = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      # Appearance
      indentLine     # vimscript
      indent-blankline-nvim
      barbar-nvim
      nvim-tree-lua
      nvim-web-devicons
      lualine-nvim
      one-nvim
      dracula-vim
      dashboard-nvim #vimscript

      # Programming
      which-key-nvim
      vim-haskellConcealPlus # vimscript
      vim-nix                # vimscript
      lspkind-nvim
      nvim-treesitter
      nvim-treesitter-refactor
      nvim-treesitter-textobjects
      nvim-lspconfig
      lspsaga-nvim
      lsp_signature-nvim
      nvim-compe
      vim-vsnip
      vim-vsnip-integ
      rust-tools-nvim
      vimPlugsFromSource.nvim-lsp-symbols-outline

      # Text objects
      tcomment_vim    # vimscript
      vim-surround    # vimscript
      vim-repeat      # vimscript
      nvim-autopairs

      # Git
      vim-fugitive  # vimscript
      gitsigns-nvim

      # DAP
      nvim-dap
      nvim-dap-ui
      vimPlugsFromSource.nvim-dap-python

      # Fuzzy Finder
      telescope-nvim

      # Text Helpers
      vim-table-mode # vimscript
      todo-comments-nvim

      # General Deps
      popup-nvim
      plenary-nvim
    ];

    extraConfig = ''
      ${builtins.readFile ./sane_defaults.vim}
      ${builtins.readFile ./dashboard.vim}

      colorscheme ${colorscheme.vim-name}

      lua << EOF
        local statusline_theme = '${colorscheme.vim-statusline}'

        ${builtins.readFile ./sane_defaults.lua}
        ${builtins.readFile ./treesitter.lua}
        ${builtins.readFile ./telescope.lua}
        ${builtins.readFile ./lsp.lua}
        ${builtins.readFile ./dap.lua}
        ${builtins.readFile ./statusline.lua}
        ${builtins.readFile ./git.lua}
        ${builtins.readFile ./todo.lua}
        ${builtins.readFile ./which_key.lua}
      EOF

      ${builtins.readFile ./theme.vim}
      ${builtins.readFile ./indentline.vim}
    '';

    package = pkgs.neovim-nightly;
  };
}
