{pkgs, ...}:
{
  nvim-popup = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "popup.nvim";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-lua";
      repo = "popup.nvim";
      rev = "master";
      sha256 = "sha256-1s/x4KMzDIZat4AHVEqe/IcGsD7xEClEWrt8o5aaL0g=";
    };
  };
  nvim-treesitter = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-treesitter";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-treesitter";
      repo = "nvim-treesitter";
      rev = "master";
      sha256 = "sha256-Q5AtqMcnCvEo/yPhh+84jfJQNMmp0ZDaMKi/x1CDZ+A=";
    };
  };
}
