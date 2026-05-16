{
  pkgs,
  colorscheme,
  ...
}:
let
  python_with_debugpy = pkgs.python3.withPackages (ps: with ps; [ debugpy ]);

  cron_describe =
    pkgs.writeScriptBin "cron-describe" # python
      ''
        #!${pkgs.python3.withPackages (p: [ p.cron-descriptor ])}/bin/python3
        from cron_descriptor import get_description
        import sys
        if len(sys.argv) != 2:
            print("Usage: cron-describe '<cron_expression>'")
            sys.exit(1)
        print(get_description(sys.argv[1]))
      '';

in
{
  xdg.configFile = {
    "nvim/lua/core".source = ./lua/core;
    "nvim/lua/common.lua".source = ./lua/common.lua;
    "nvim/lua/nix_injected.lua".text = # lua
      ''
        return {
             dap_python_with_debugpy  = "${python_with_debugpy}",
             cronex_explainer = "${cron_describe}/bin/cron-describe",
        }
      '';
    "nvim/ftplugin/nix.lua".text = # lua
      ''
        vim.opt_local.tabstop = 2
        vim.opt_local.shiftwidth = 2
        vim.opt_local.expandtab = true
      '';
  };
  programs.neovim = {
    enable = true;
    viAlias = true;
    withRuby = true;
    withPython3 = true;

    plugins =
      let
        plug = name: config: {
          plugin = name;
          type = "lua";
          config =
            if builtins.typeOf config == "string" then
              config
            else if builtins.typeOf config == "path" then
              builtins.readFile config
            else
              builtins.throw "invalid config type";
        };
        plug_dep = name: {
          plugin = name;
          optional = true;
        };
        lazy_plug = name: config: (plug name config) // { optional = true; };
      in
      with pkgs.vimPlugins;
      [
        # Setup the plugin that can lazy load others
        lz-n
        nvim-nio
        plenary-nvim

        # Appearance
        (plug indent-blankline-nvim ./lua/indent-blankline.lua)
        (plug lualine-nvim ./lua/lualine.lua)
        nvim-web-devicons
        (plug statuscol-nvim ./lua/statuscol.lua)
        (lazy_plug nvim-tree-lua ./lua/nvim-tree.lua)

        # themes
        catppuccin-nvim
        tokyonight-nvim

        # DAP
        (lazy_plug nvim-dap ./lua/dap.lua)
        (plug_dep nvim-dap-view)
        (plug_dep nvim-dap-python)

        # Fuzzy Finder
        (lazy_plug telescope-nvim ./lua/telescope.lua)
        (plug_dep telescope-fzf-native-nvim)
        (plug_dep telescope-ui-select-nvim)

        # Git
        (plug gitsigns-nvim ./lua/gitsigns.lua)
        (lazy_plug blame-nvim ./lua/blame.lua)
        (lazy_plug diffview-nvim ./lua/diffview.lua)

        # Keymaps
        (plug which-key-nvim ./lua/which-key.lua)

        # navigation
        (plug nvim-bqf ''require("bqf").setup()'')
        (plug nvim-pqf ''require("pqf").setup()'')

        # Programming: LSP
        (lazy_plug nvim-lspconfig ./lua/lspconfig.lua)
        (plug_dep SchemaStore-nvim)
        (plug_dep lspsaga-nvim)
        (lazy_plug nvim-lint ./lua/lint.lua)
        (lazy_plug conform-nvim ./lua/conform.lua)
        (lazy_plug lsp_lines-nvim ./lua/lsp_lines.lua)

        # Programming: Treesitter
        {
          plugin = nvim-treesitter.withPlugins (
            plugins: with plugins; [
              bash
              dockerfile
              haskell
              json
              kdl
              ledger
              lua
              markdown
              markdown-inline
              nix
              nu
              python
              regex
              rust
              sql
              toml
              vimdoc
              yaml
              nvim-treesitter-kulala-http
            ]
          );
          type = "lua";
          config = builtins.readFile ./lua/treesitter.lua;
        }
        (lazy_plug treesj ./lua/treesj.lua)

        # Programming: Language support
        (plug rustaceanvim ./lua/rustaceanvim.lua)
        (plug haskell-tools-nvim ./lua/haskell-tools.lua)

        # Programming: Autocompletion setup
        (lazy_plug blink-cmp ./lua/blink.lua)
        (plug_dep blink-compat)
        (plug_dep friendly-snippets)

        # Programming: Testing
        (lazy_plug neotest ./lua/neotest.lua)
        (plug_dep neotest-python)
        (plug_dep neotest-haskell)
        FixCursorHold-nvim
        # sessions
        (plug auto-session ./lua/auto-session.lua)

        # Text Helpers
        (lazy_plug todo-comments-nvim ./lua/todo-comments.lua)
        (plug venn-nvim ./lua/venn.lua)
        (lazy_plug vim-table-mode ./lua/table-mode.lua)

        # Text objects
        (plug nvim-autopairs "require('nvim-autopairs').setup {}")
        (plug nvim-surround "require('nvim-surround').setup {}")

        # Webdev: Database
        (plug_dep vim-dotenv)
        (plug_dep vim-dadbod)
        (lazy_plug vim-dadbod-ui ./lua/dadbod.lua)
        (plug_dep vim-dadbod-completion)
        (plug_dep nvim-dadbod-ssh)

        # Webdev: API
        (lazy_plug kulala-nvim ./lua/kulala.lua)

        # File specific plugins
        (lazy_plug render-markdown-nvim ./lua/render-markdown.lua)
        (lazy_plug crates-nvim ./lua/crates.lua)
        (lazy_plug nvim-cronex ./lua/cronex.lua)
      ];

    initLua = # lua
      ''
        ${builtins.readFile ./lua/base.lua}
        vim.cmd("colorscheme ${colorscheme.vim-name}")

      '';
  };

  home = {
    sessionVariables.NIXD_FLAGS = "-log=error";
    packages = with pkgs; [
      vim-startuptime
    ];

  };
}
