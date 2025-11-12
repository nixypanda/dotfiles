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
    nvim-treesitter-kulala-http-src = {
      url = "github:mistweaverco/kulala.nvim";
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
      missingVimPluginsInNixpkgs = pkgs: {
        nvim-sqls = pkgs.vimUtils.buildVimPlugin {
          pname = "nvim-sqls";
          version = "custom";
          src = inputs.nvim-sqls-src;
        };
        nvim-blame = pkgs.vimUtils.buildVimPlugin {
          pname = "blame.nvim";
          version = "custom";
          src = inputs.nvim-blame-src;
        };
        nvim-dadbod-ssh = pkgs.vimUtils.buildVimPlugin {
          pname = "nvim-dadbod-ssh";
          version = "custom";
          src = inputs.nvim-dadbod-ssh;
        };
        nvim-cronex = pkgs.vimUtils.buildVimPlugin {
          pname = "cronex.nvim";
          version = "custom";
          src = inputs.nvim-cronex-src;
        };
        nvim-treesitter-kulala-http = pkgs.tree-sitter.buildGrammar {
          language = "kulala_http";
          version = "5.3.1"; # <- this can be anything I ususally do like, inputs.kulala-grammar.shortRev
          src = inputs.nvim-treesitter-kulala-http-src;
          location = "lua/tree-sitter";
        };
      };
    in
    {
      overlay = _final: prev: { vimPlugins = prev.vimPlugins // (missingVimPluginsInNixpkgs prev.pkgs); };
    };
}
