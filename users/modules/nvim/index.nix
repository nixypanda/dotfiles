{ config, pkgs, libs, ... }:
let
  colorscheme = (import ../../colorschemes/onedark.nix);
  vimPlugsFromSource = (import ./plugins.nix) pkgs;
in
{
  programs.neovim = {
    enable = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      # Appearance
      vim-table-mode # vimscript
      barbar-nvim
      nvim-tree-lua
      nvim-web-devicons
      galaxyline-nvim
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
      vimPlugsFromSource.nvim-rust-tools

      # Text objects
      tcomment_vim # vimscript
      vim-surround # vimscript
      vim-repeat   # vimscript
      delimitMate  # vimscript

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
      set number relativenumber
      set colorcolumn=100

      lua << EOF
        ${builtins.readFile ./sane_defaults.lua}
        ${builtins.readFile ./treesitter.lua}
        ${builtins.readFile ./telescope.lua}
        ${builtins.readFile ./lsp.lua}
        ${builtins.readFile ./statusline.lua}
      EOF

      " Vim theme info
      colorscheme one-nvim
      ${builtins.readFile ./theme.vim}

      "" lsp shit that can't be done in lua atm
      autocmd BufWritePre *.rs lua vim.lsp.buf.formatting_sync(nil, 1000)
      autocmd BufWritePre *.hs lua vim.lsp.buf.formatting_sync(nil, 1000)

      ${builtins.readFile ./which_key.vim}
    '';

    package = pkgs.neovim-nightly;
  };
}
