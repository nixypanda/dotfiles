{ ... }:
{
  _module.args = {
    colorscheme = import ../../colorschemes/tokyonight.nix;
  };

  home = {
    homeDirectory = "/home/nixypanda";
    username = "nixypanda";
    stateVersion = "25.11";
  };

  programs.git = {
    enable = true;
    settings = {
      user.email = "sherub.thakur@gmail.com";
      user.name = "nixypanda";
      pull.ff = "only";
      init.defaultBranch = "main";
      merge.conflictstyle = "diff3";
      core.editor = "vi";
    };
  };

  imports = [
    ../../modules/cli.nix
    ../../modules/env.nix
    ../../modules/nvim/minimal.nix
  ];
}
