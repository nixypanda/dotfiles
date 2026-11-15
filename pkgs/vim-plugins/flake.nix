{
  inputs = {
    nvim-dadbod-ssh = {
      url = "github:pbogut/vim-dadbod-ssh";
      flake = false;
    };
    nvim-cronex-src = {
      url = "github:fabridamicelli/cronex.nvim";
      flake = false;
    };
    tokyonight-merino-src = {
      url = "github:LosFarmosCTL/tokyonight-merino.nvim";
      flake = false;
    };

  };
  outputs =
    inputs:
    let
      missingVimPluginsInNixpkgs = _: prev: {
        nvim-dadbod-ssh = prev.vimUtils.buildVimPlugin {
          pname = "nvim-dadbod-ssh";
          version = "custom";
          src = inputs.nvim-dadbod-ssh;
        };
        nvim-cronex = prev.vimUtils.buildVimPlugin {
          pname = "cronex.nvim";
          version = "custom";
          src = inputs.nvim-cronex-src;
        };
        tokyonight-merino-nvim = prev.vimUtils.buildVimPlugin {
          pname = "tokyonight-merino.nvim";
          version = "custom";
          src = inputs.tokyonight-merino-src;
        };
      };
    in
    {
      overlay = final: prev: {
        vimPlugins = prev.vimPlugins // (missingVimPluginsInNixpkgs final prev);
      };
    };
}
