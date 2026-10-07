{ pkgs, colorscheme, ... }:
{
  programs.neovim = {
    enable = true;
    withRuby = true;
    withPython3 = true;
    plugins = with pkgs.vimPlugins; [
      nvim-nio
      neotest-python
      plenary-nvim
      pkgs.vimPlugins.${colorscheme.vim-plugin}

      {
        plugin = nvim-treesitter.withPlugins (
          plugins: with plugins; [
            python
          ]
        );
        type = "lua";
        config = "require'nvim-treesitter.configs'.setup {}";
      }
      {
        plugin = neotest;
        config = # lua
          ''
            require("neotest").setup({
                adapters = {
                    require("neotest-python")({ dap = { justMyCode = true } }),
                },
            })
          '';
        type = "lua";
      }
    ];
    extraConfig = "colorscheme ${colorscheme.vim-name}";
  };
}
