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
    nvim-neotest-src = {
      url = "github:appaquet/neotest/fix/add-subprocess-default-init-back";
      flake = false;
    };
    nvim-neotest-python-src = {
      url = "github:nvim-neotest/neotest-python";
      flake = false;
    };
    nvim-neotest-haskell-src = {
      url = "github:mrcjkb/neotest-haskell";
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
        nvim-blame = prev.vimUtils.buildVimPlugin {
          pname = "blame.nvim";
          version = "custom";
          src = inputs.nvim-blame-src;
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
        nvim-treesitter-kulala-http = prev.tree-sitter.buildGrammar {
          language = "kulala_http";
          version = "5.3.1"; # <- this can be anything I ususally do like, inputs.kulala-grammar.shortRev
          src = inputs.nvim-treesitter-kulala-http-src;
          location = "lua/tree-sitter";
        };
        # WARN: Remove this once https://github.com/nvim-neotest/neotest/issues/531 is fixed
        neotest = prev.vimUtils.buildVimPlugin {
          pname = "neotest";
          version = "5.13.1-patched2";
          src = inputs.nvim-neotest-src;
          propagatedBuildInputs = with prev.vimPlugins; [
            nvim-nio
            plenary-nvim
          ];
          doCheck = false;
        };
        neotest-python = prev.vimUtils.buildVimPlugin {
          pname = "neotest-python";
          version = "2025-11-04";
          src = inputs.nvim-neotest-python-src;
          propagatedBuildInputs = [
            final.vimPlugins.neotest
          ];
          doCheck = false;
        };
        neotest-haskell = prev.vimUtils.buildVimPlugin {
          pname = "neotest-haskell";
          version = "2025-11-04";
          src = inputs.nvim-neotest-haskell-src;
          propagatedBuildInputs = [
            final.vimPlugins.neotest
          ];
          doCheck = false;
        };
      };
    in
    {
      overlay = final: prev: {
        vimPlugins = prev.vimPlugins // (missingVimPluginsInNixpkgs final prev);
      };
    };
}
