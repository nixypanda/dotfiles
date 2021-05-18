{pkgs, ...}:
{
  nvim-todo-comments = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-todo-comments";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "folke";
      repo = "todo-comments.nvim";
      rev = "main";
      sha256 = "sha256-dyCmp/UnJ3zZr8ezMAhmvCNyLnahRg8/KUetxbGJC0E=";
    };
  };
  nvim-octo = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-octo";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "pwntester";
      repo = "octo.nvim";
      rev = "master";
      sha256 = "sha256-gpkaDAaRWyPQ911YlpBxFxEWEhJlYDSGT45fEQ4UAnE=";
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
