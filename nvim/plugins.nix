{pkgs, ...}:
{

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

  nvim-treesitter-refactor = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-treesitter-refactor";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-treesitter";
      repo = "nvim-treesitter-refactor";
      rev = "master";
      sha256 = "sha256-t4L9avmEKcONPB+D7UG69Cs88bzKJpgXI0bwO7e56Z8=";
    };
  };

  nvim-treesitter-textobjects = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-treesitter-textobjects";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-treesitter";
      repo = "nvim-treesitter-textobjects";
      rev = "master";
      sha256 = "sha256-lTgY677ZNxOcvafaB0l2qVVkn9zYFi5DZzLytE0gAy8=";
    };
  };

  nvim-bubbly = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "bubbly.nvim";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "datwaft";
      repo = "bubbly.nvim";
      rev = "master";
      sha256 = "sha256-VmHjxeDc4ZVIygKGOVWU8ILkInmV6o792+gi4VOVxKA=";
    };
  };

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

  nvim-plenary = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "plenary.nvim";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-lua";
      repo = "plenary.nvim";
      rev = "master";
      sha256 = "sha256-EE/rzidmZKNSisuRhicirI68XCmoCZs+v/ofEtpeIBE=";
    };
  };

  nvim-telescope = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "telescope.nvim";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-telescope";
      repo = "telescope.nvim";
      rev = "master";
      sha256 = "sha256-27Fe4ahSXa4c4f9jfwOvPpgCkiQWEN+IZwLUnrp72WU=";
    };
  };

  nvim-lspconfig = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-lspconfig";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "neovim";
      repo = "nvim-lspconfig";
      rev = "master";
      sha256 = "sha256-ioWoYt3bp+jm+RuXUi0nkwFhhNFxPPuY3mFEj9TOSkg=";
    };
  };

  nvim-lspsaga = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-lspsaga";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "glepnir";
      repo = "lspsaga.nvim";
      rev = "main";
      sha256 = "sha256-dsKXlDk5cHRIKnH0awZu3iROWjL2ivLloZMhSmVyTbE=";
    };
  };

  nvim-compe = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-compe";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "hrsh7th";
      repo = "nvim-compe";
      rev = "master";
      sha256 = "sha256-xBA7u8lusLW5hbZkkVrrH5M1g30wh+J7mIBRF8krXG0=";
    };
  };


  nvim-web-devicons = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-web-devicons";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "kyazdani42";
      repo = "nvim-web-devicons";
      rev = "master";
      sha256 = "sha256-Veoi0h9jPOEP1mCNuyW0782lHLMUf7/Pncy8jCXjGuc=";
    };
  };

  nvim-tree = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-tree";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "kyazdani42";
      repo = "nvim-tree.lua";
      rev = "master";
      sha256 = "sha256-F1e4T4N5cbAkshay2kbE61pAzNx+pHd1eg60s8rDfAc=";
    };
  };


}
