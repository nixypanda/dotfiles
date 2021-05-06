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
      indentLine     # vimscript
      indent-blankline-nvim
      barbar-nvim
      nvim-tree-lua
      nvim-web-devicons
      lualine-nvim
      one-nvim
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
      gitsigns-nvim

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
      ${builtins.readFile ./sane_defaults.vim}
      ${builtins.readFile ./dashboard.vim}

      lua << EOF
        ${builtins.readFile ./sane_defaults.lua}
        ${builtins.readFile ./treesitter.lua}
        ${builtins.readFile ./telescope.lua}
        ${builtins.readFile ./lsp.lua}
        ${builtins.readFile ./statusline.lua}
        ${builtins.readFile ./git.lua}
        ${builtins.readFile ./which_key.lua}
      EOF

      " Vim theme info
      colorscheme one-nvim
      ${builtins.readFile ./theme.vim}

      "" lsp shit that can't be done in lua atm
      autocmd BufWritePre *.rs lua vim.lsp.buf.formatting_sync(nil, 1000)
      autocmd BufWritePre *.hs lua vim.lsp.buf.formatting_sync(nil, 1000)

      let g:vsnip_snippet_dir = expand('~/.dotfiles/users/modules/nvim/vsnip')
      ${builtins.readFile ./indentline.vim}
    '';

    package = pkgs.neovim-nightly;
  };
}
