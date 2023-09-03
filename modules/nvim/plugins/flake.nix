{
  inputs = {
    nvim-better-digraphs-src = {
      url = "github:protex/better-digraphs.nvim";
      flake = false;
    };
    nvim-regexplainer-src = {
      url = "github:bennypowers/nvim-regexplainer";
      flake = false;
    };
    nvim-sqls-src = {
      url = "github:nanotee/sqls.nvim";
      flake = false;
    };
  };
  outputs = inputs:
    let
      missingVimPluginsInNixpkgs = pkgs: {
        nvim-better-digraphs = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-better-digraphs";
          src = inputs.nvim-better-digraphs-src;
        };
        nvim-regexplainer = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-regexplainer";
          src = inputs.nvim-regexplainer-src;
        };
        nvim-sqls = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-sqls";
          src = inputs.nvim-sqls-src;
        };
      };
    in {
      overlay = _final: prev: {
        vimPlugins = prev.vimPlugins // (missingVimPluginsInNixpkgs prev.pkgs);
      };
    };
}
