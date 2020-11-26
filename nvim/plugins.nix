{pkgs, ...}:
{

  nvim-treesitter = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-treesitter";
    version = "1bfffb11fdc73164caef9eb9f8c2a3a291c79b8c";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-treesitter";
      repo = "nvim-treesitter";
      rev = "1bfffb11fdc73164caef9eb9f8c2a3a291c79b8c";
      sha256 = "0gxv6rnj79kdaafs1fx8w6z2kyn1dlzyd58kgb1v6kil25pnqzsl";
    };
  };

  nvim-treesitter-refactor = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-treesitter-refactor";
    version = "9d4b9daf2f138a5de538ee094bd899591004f8e2";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-treesitter";
      repo = "nvim-treesitter-refactor";
      rev = "9d4b9daf2f138a5de538ee094bd899591004f8e2";
      sha256 = "0ma5zsl70mi92b9y8nhgkppdiqfjj0bl3gklhjv1c3lg7kny7511";
    };
  };

  nvim-treesitter-textobjects = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-treesitter-textobjects";
    version = "42bca9550fa6ad5389d6e95ba9876bf05a8d0406";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-treesitter";
      repo = "nvim-treesitter-textobjects";
      rev = "42bca9550fa6ad5389d6e95ba9876bf05a8d0406";
      sha256 = "0zvjjbffvpnmc9rgrkj8cx5jqbkw8yvjwnj0jk8ccxpl0s9v2yi6";
    };
  };

  nvim-lspconfig = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-lspconfig";
    version = "e6bc6b23bc162132b3040e0b0847ecfc04ff808c";
    src = pkgs.fetchFromGitHub {
      owner = "neovim";
      repo = "nvim-lspconfig";
      rev = "e6bc6b23bc162132b3040e0b0847ecfc04ff808c";
      sha256 = "1vsr13nmzfbwdd3gr5rajiq00qwib1j5ri74nbj5ifshhlm0aqkd";
    };
  };

  nvim-completion = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "completion-nvim";
    version = "936bbd17577101a4ffb07ea7f860f77dd8007d43";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-lua";
      repo = "completion-nvim";
      rev = "936bbd17577101a4ffb07ea7f860f77dd8007d43";
      sha256 = "1z399q3v36hx2ipj1fhxcc051pi4q0lifyglmclxi5zkbmm0z6a7";
    };
  };

  nvim-completion-treesitter = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "completion-treesitter";
    version = "45c9b2faff4785539a0d0c655440c2465fed985a";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-treesitter";
      repo = "completion-treesitter";
      rev = "45c9b2faff4785539a0d0c655440c2465fed985a";
      sha256 = "19pgdzzk7zq85b1grfjf0nncvs5vxrd4rj1p90iw2amq4mvqrx3l";
    };
  };

  nvim-completion-tags = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "completion-tags";
    version = "450d9572be48307c0f6264851d92f9f0d4b333a1";
    src = pkgs.fetchFromGitHub {
      owner = "kristijanhusak";
      repo = "completion-tags";
      rev = "450d9572be48307c0f6264851d92f9f0d4b333a1";
      sha256 = "1s51bk5qragcjjb57zr4s2lxm4z8d6gwj5ympai8jfgq4sxwzdfc";
    };
  };

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
