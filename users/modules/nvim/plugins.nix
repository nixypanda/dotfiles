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
}
