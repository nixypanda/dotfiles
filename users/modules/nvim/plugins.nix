{ pkgs, ... }:
{

  nvim-dap-python = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-dap-python";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "mfussenegger";
      repo = "nvim-dap-python";
      rev = "master";
      sha256 = "sha256-yRFoqQrG1H+ADJGt1x6+9em221B32C6ia0VEi8PRRIc=";
    };
  };

  nvim-alpha = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-alpha";
    version = "main";
    src = pkgs.fetchFromGitHub {
      owner = "goolord";
      repo = "alpha-nvim";
      rev = "main";
      sha256 = "sha256-RdvlKhrqjbYvAw+X0tkzZea4GwgJv0YlMgQY/SEtH8c=";
    };
  };

}
