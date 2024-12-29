{
  inputs = {
    nvim-sqls-src = {
      url = "github:nanotee/sqls.nvim";
      flake = false;
    };
    nvim-blame-src = {
      url = "github:FabijanZulj/blame.nvim";
      flake = false;
    };
    nvim-dadbod-ssh = {
      url = "github:pbogut/vim-dadbod-ssh";
      flake = false;
    };
  };
  outputs =
    inputs:
    let
      missingVimPluginsInNixpkgs = pkgs: {
        nvim-sqls = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-sqls";
          src = inputs.nvim-sqls-src;
        };
        nvim-blame = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-blame";
          src = inputs.nvim-blame-src;
        };
        nvim-dadbod-ssh = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-dadbod-ssh";
          src = inputs.nvim-dadbod-ssh;
        };
      };
    in
    {
      overlay = _final: prev: { vimPlugins = prev.vimPlugins // (missingVimPluginsInNixpkgs prev.pkgs); };
    };
}
