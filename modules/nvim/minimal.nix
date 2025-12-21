{ pkgs, ... }:
{
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      nvim-nio
      neotest-python
      plenary-nvim
      tokyonight-nvim

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
    # extraConfig = "colorscheme tokyonight";
  };
}
