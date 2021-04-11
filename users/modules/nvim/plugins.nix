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
  nvim-rust-tools = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-rust-tools";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "simrat39";
      repo = "rust-tools.nvim";
      rev = "master";
      sha256 = "sha256-PELLnW9/b7/vDuws0+6wKyhxZjExyCSIe5dzEUKLz5M=";
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
