{ pkgs, ... }:
{

  nvim-dap-python = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-dap-python";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "mfussenegger";
      repo = "nvim-dap-python";
      rev = "master";
      sha256 = "sha256-ZPJuv+XsizTZmYC4CZkzV8NGwt+Mlq+KmddQsLApEYQ=";
    };
  };

  nvim-alpha = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-alpha";
    version = "main";
    src = pkgs.fetchFromGitHub {
      owner = "goolord";
      repo = "alpha-nvim";
      rev = "main";
      sha256 = "sha256-/FW29rwyYLQgpItomriu98MF2nog6lVzz75wX4Tz3WE=";
    };
  };

}
