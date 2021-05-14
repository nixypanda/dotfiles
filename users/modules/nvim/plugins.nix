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
}
