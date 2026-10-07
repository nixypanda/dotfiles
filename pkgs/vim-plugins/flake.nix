{
  inputs = {
    nvim-sqls-src = {
      url = "github:nanotee/sqls.nvim";
      flake = false;
    };
    nvim-dadbod-ssh = {
      url = "github:pbogut/vim-dadbod-ssh";
      flake = false;
    };
    nvim-cronex-src = {
      url = "github:fabridamicelli/cronex.nvim";
      flake = false;
    };

  };
  outputs =
    inputs:
    let
      missingVimPluginsInNixpkgs = final: prev: {
        nvim-sqls = prev.vimUtils.buildVimPlugin {
          pname = "nvim-sqls";
          version = "custom";
          src = inputs.nvim-sqls-src;
        };
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
      };
    in
    {
      overlay = final: prev: {
        vimPlugins = prev.vimPlugins // (missingVimPluginsInNixpkgs final prev);
      };
    };
}
