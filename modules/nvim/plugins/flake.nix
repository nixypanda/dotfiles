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
    nvim-haskell-tools-src = {
      url =
        "github:mrcjkb/haskell-tools.nvim/fd7c33cc3e893a12c1d90aca9ff7ede7d01f003d";
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
        nvim-haskell-tools = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-haskell-tools";
          src = inputs.nvim-haskell-tools-src;
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
