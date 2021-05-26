{pkgs, ...}:
{
  nvim-todo-comments = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-todo-comments";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "folke";
      repo = "todo-comments.nvim";
      rev = "main";
      sha256 = "sha256-xpN/g+GuuBZtiD/pSk3TYiN6xhYECJbA46aRlObVT2g=";
    };
  };
  nvim-octo = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-octo";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "pwntester";
      repo = "octo.nvim";
      rev = "master";
      sha256 = "sha256-6zL2diOsOGuThkIbok0JKbmrZi2KfyKsrm/00J5g4Wg=";
    };
  };
  nvim-lsp-signature = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-lsp-signature";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "ray-x";
      repo = "lsp_signature.nvim";
      rev = "master";
      sha256 = "sha256-/SnCofoQNKY6M9i5WA/hnZlrPvbbBQzQFwMBsmkRGC4=";
    };
  };
}
