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
  nvim-lsp-saga = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-lsp-saga";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "ckipp01";
      repo = "lspsaga.nvim";
      rev = "185526658e6e8b11c2b1a268d98dbd28f46dad77";
      sha256 = "sha256-rbW9HlntH7GVgUUnPL7JZSdyRoqZdEQRZ8s1dkR7lkM=";
    };
  };
}
