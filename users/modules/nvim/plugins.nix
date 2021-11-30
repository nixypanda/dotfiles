{ pkgs, ... }:
{

  nvim-dap-python = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-dap-python";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "mfussenegger";
      repo = "nvim-dap-python";
      rev = "master";
      sha256 = "sha256-lN9+0kijNRocas/5PVXe+jhvhIkHKFO8n6q4y3Hn0vI=";
    };
  };

  nvim-alpha = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-alpha";
    version = "main";
    src = pkgs.fetchFromGitHub {
      owner = "goolord";
      repo = "alpha-nvim";
      rev = "main";
      sha256 = "sha256-+00DTu9D2vbxJulKC9TYEZL85MA2hlln/908okKShjY=";
    };
  };

  nvim-copilot = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-copilot";
    version = "release";
    src = pkgs.fetchFromGitHub {
      owner = "github";
      repo = "copilot.vim";
      rev = "release";
      sha256 = "sha256-hKRkn/+6S2JfAlgN13X2HNl/1vIjeMM5YnSTEwVQDTg=";
    };
  };

  nvim-lsp-saga = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-lsp-saga";
    version = "main";
    src = pkgs.fetchFromGitHub {
      owner = "tami5";
      repo = "lspsaga.nvim";
      rev = "main";
      sha256 = "sha256-m+CcPoKhW30Qu7nK655+xwg8B/rZoFWN5gnz1o2gnsY=";
    };
  };

  nvim-cmp-copilot = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-cmp-copilot";
    version = "main";
    src = pkgs.fetchFromGitHub {
      owner = "hrsh7th";
      repo = "cmp-copilot";
      rev = "main";
      sha256 = "sha256-07cdIAJbGI8nbxvqO3I2ebP9mW1kzbraPW44WOpQRjk=";
    };
  };
}
