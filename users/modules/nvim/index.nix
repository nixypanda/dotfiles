{ config, pkgs, lib, ... }:
let
  colorscheme = (import ../../colorschemes/onedark.nix) { lib = lib; };
  vimPlugsFromSource = (import ./plugins.nix) pkgs;
in
{
  programs.neovim = {
    enable = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      # Appearance
      vim-table-mode # vimscript
      indentLine  # vimscript
      indent-blankline-nvim
      barbar-nvim
      nvim-tree-lua
      nvim-web-devicons
      lualine-nvim
      one-nvim

      # Programming
      vim-which-key          # vimscript
      vim-haskellConcealPlus # vimscript
      vim-nix                # vimscript
      lspkind-nvim
      nvim-treesitter
      nvim-treesitter-refactor
      nvim-treesitter-textobjects
      nvim-lspconfig
      vimPlugsFromSource.nvim-lsp-saga
      nvim-compe
      vim-vsnip
      vim-vsnip-integ
      vimPlugsFromSource.nvim-rust-tools

      # Text objects
      tcomment_vim    # vimscript
      vim-surround    # vimscript
      vim-repeat      # vimscript
      nvim-autopairs

      # Git
      vim-fugitive  # vimscript
      vim-gitgutter # vimscript

      # DAP
      vimspector # vimscript

      # Fuzzy Finder
      telescope-nvim

      # General Deps
      # popup-nvim
      # Note: Current version on nixpkgs seems broken
      vimPlugsFromSource.nvim-popup
      plenary-nvim
    ];

    extraConfig = ''
      " NOTE: For some reason these settings don't have any affect if configured
      " in lua

      ${builtins.readFile ./sane_defaults.vim}
      lua << EOF
        ${builtins.readFile ./sane_defaults.lua}
        ${builtins.readFile ./treesitter.lua}
        ${builtins.readFile ./telescope.lua}
        ${builtins.readFile ./lsp.lua}
        ${builtins.readFile ./statusline.lua}
      EOF

      " Set indentLine char
      let g:indentLine_char='▏'

      " Vim theme info
      colorscheme one-nvim
      ${builtins.readFile ./theme.vim}

      "" lsp shit that can't be done in lua atm
      autocmd BufWritePre *.rs lua vim.lsp.buf.formatting_sync(nil, 1000)
      autocmd BufWritePre *.hs lua vim.lsp.buf.formatting_sync(nil, 1000)

      let g:vsnip_snippet_dir = expand('~/.dotfiles/users/modules/nvim/vsnip')

      ${builtins.readFile ./which_key.vim}
    '';

    package = pkgs.neovim-nightly;
  };
}
