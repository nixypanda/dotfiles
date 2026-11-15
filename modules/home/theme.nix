_: {
  # Single place the colorscheme is chosen. Home modules read
  # `colorscheme` from `_module.args`; hosts override here if needed.
  _module.args.colorscheme = import ../../colorschemes/tokyonight.nix // {
    vim-plugin = "tokyonight-merino-nvim";
    vim-name = "tokyonight-merino";
  };
}
