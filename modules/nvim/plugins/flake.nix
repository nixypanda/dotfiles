{
  inputs = {
    nvim-better-digraphs-src = {
      url = "github:protex/better-digraphs.nvim";
      flake = false;
    };
    nvim-headlines-src = {
      url = "github:lukas-reineke/headlines.nvim";
      flake = false;
    };
    nvim-lspsaga-src = {
      url = "github:glepnir/lspsaga.nvim";
      flake = false;
    };
    nvim-nu-src = {
      url = "github:LhKipp/nvim-nu";
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
    nvim-codeium-src = {
      url = "github:Exafunction/codeium.vim";
      flake = false;
    };
  };
  outputs = inputs:
    let
      missingVimPluginsInNixpkgs = pkgs: {
        nvim-codeium = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-codeium";
          src = inputs.nvim-codeium-src;
        };
        nvim-better-digraphs = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-better-digraphs";
          src = inputs.nvim-better-digraphs-src;
        };
        nvim-headlines = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-headlines";
          src = inputs.nvim-headlines-src;
        };
        nvim-lspsaga = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-lspsaga";
          src = inputs.nvim-lspsaga-src;
        };
        nvim-nu = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-nu";
          src = inputs.nvim-nu-src;
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
    in
    {
      overlay = _final: prev: {
        vimPlugins = prev.vimPlugins // (missingVimPluginsInNixpkgs prev.pkgs);
      };
    };
}
