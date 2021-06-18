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
  nvim-dap-python = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-dap-python";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "mfussenegger";
      repo = "nvim-dap-python";
      rev = "master";
      sha256 = "sha256-yu/6nNtAlveQ4OTIBdmeVQkc8qFfxnDDeqM6R8HUibg=";
    };
  };
}
