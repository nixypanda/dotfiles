{ pkgs, ... }:
{
  nvim-dap-python = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-dap-python";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "mfussenegger";
      repo = "nvim-dap-python";
      rev = "master";
      sha256 = "sha256-vQ7wWiSdCDd1hQfi9Mv67ZpmBvDcO/hy8IOgMzE2yUk=";
    };
  };
  nvim-alpha = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-alpha";
    version = "main";
    src = pkgs.fetchFromGitHub {
      owner = "goolord";
      repo = "alpha-nvim";
      rev = "main";
      sha256 = "sha256-MGfxAuyGOF7sMk+l9iBBelUlvg5OJEd0iOxf4Oger5s=";
    };
  };
}
