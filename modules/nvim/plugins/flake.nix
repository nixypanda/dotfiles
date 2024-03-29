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
    nvim-lsp-file-operations-src = {
      url = "github:antosha417/nvim-lsp-file-operations";
      flake = false;
    };
    nvim-rustaceanvim-src = {
      url = "github:mrcjkb/rustaceanvim";
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
        nvim-lsp-file-operations = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-lsp-file-operations";
          src = inputs.nvim-lsp-file-operations-src;
        };
        nvim-rustaceanvim = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-rustaceanvim";
          src = inputs.nvim-rustaceanvim-src;
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
    in {
      overlay = _final: prev: {
        vimPlugins = prev.vimPlugins // (missingVimPluginsInNixpkgs prev.pkgs);
      };
    };
}
