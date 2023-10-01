{
  inputs = {
    nvim-regexplainer-src = {
      url = "github:bennypowers/nvim-regexplainer";
      flake = false;
    };
    nvim-sqls-src = {
      url = "github:nanotee/sqls.nvim";
      flake = false;
    };
    nvim-conform-src = {
      url = "github:stevearc/conform.nvim";
      flake = false;
    };
  };
  outputs = inputs:
    let
      missingVimPluginsInNixpkgs = pkgs: {
        nvim-regexplainer = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-regexplainer";
          src = inputs.nvim-regexplainer-src;
        };
        nvim-sqls = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-sqls";
          src = inputs.nvim-sqls-src;
        };
        nvim-conform = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-conform";
          src = inputs.nvim-conform-src;
        };
      };
    in {
      overlay = _final: prev: {
        vimPlugins = prev.vimPlugins // (missingVimPluginsInNixpkgs prev.pkgs);
      };
    };
}
