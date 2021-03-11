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
      sha256 = "sha256-4hzwxKSBnG1HRlaGMcixJdWEf4Fy+HHUE0JEDm/X6kk=";
    };
  };

  nvim-treesitter-textobjects = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-treesitter-textobjects";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-treesitter";
      repo = "nvim-treesitter-textobjects";
      rev = "master";
      sha256 = "sha256-RzfqnBqnNuqkBntKOvGxzie1f8rAoDOM4FaHYGMs1B8=";
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

  # nvim-lspconfig = pkgs.vimutils.buildvimpluginfrom2nix {
  #   pname = "nvim-lspconfig";
  #   version = "master";
  #   src = pkgs.fetchfromgithub {
  #     owner = "neovim";
  #     repo = "nvim-lspconfig";
  #     rev = "master";
  #     sha256 = "1vsr13nmzfbwdd3gr5rajiq00qwib1j5ri74nbj5ifshhlm0aqkd";
  #   };
  # };
  #
  # nvim-completion = pkgs.vimutils.buildvimpluginfrom2nix {
  #   pname = "completion-nvim";
  #   version = "master";
  #   src = pkgs.fetchfromgithub {
  #     owner = "nvim-lua";
  #     repo = "completion-nvim";
  #     rev = "936bbd17577101a4ffb07ea7f860f77dd8007d43";
  #     sha256 = "1z399q3v36hx2ipj1fhxcc051pi4q0lifyglmclxi5zkbmm0z6a7";
  #   };
  # };
  #
  # nvim-completion-treesitter = pkgs.vimutils.buildvimpluginfrom2nix {
  #   pname = "completion-treesitter";
  #   version = "master";
  #   src = pkgs.fetchfromgithub {
  #     owner = "nvim-treesitter";
  #     repo = "master";
  #     rev = "45c9b2faff4785539a0d0c655440c2465fed985a";
  #     sha256 = "19pgdzzk7zq85b1grfjf0nncvs5vxrd4rj1p90iw2amq4mvqrx3l";
  #   };
  # };
  #
  # nvim-completion-tags = pkgs.vimutils.buildvimpluginfrom2nix {
  #   pname = "completion-tags";
  #   version = "master";
  #   src = pkgs.fetchfromgithub {
  #     owner = "kristijanhusak";
  #     repo = "completion-tags";
  #     rev = "master";
  #     sha256 = "1s51bk5qragcjjb57zr4s2lxm4z8d6gwj5ympai8jfgq4sxwzdfc";
  #   };
  # };
  #
  # nvim-completion-buffers = pkgs.vimUtils.buildVimPluginFrom2Nix {
  #   pname = "completion-buffers";
  #   version = "441a58b77c04409e8ccb35fd4970598ae551462f";
  #   src = pkgs.fetchFromGitHub {
  #     owner = "steelsojka";
  #     repo = "completion-buffers";
  #     rev = "441a58b77c04409e8ccb35fd4970599ae551462f";
  #     sha256 = "0000000000000000000000000000000000000000000000000000";
  #   };
  # };

}
