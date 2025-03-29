{ pkgs, colorscheme, ... }:
let
  # Caveat: This requires Xcode.app installed on the system
  code_lldb = pkgs.vscode-extensions.vadimcn.vscode-lldb;

  python_with_debugpy = pkgs.python3.withPackages (ps: with ps; [ debugpy ]);

  # This sucks
  # The codeium-nvim plugin works with specific version of the language server
  # now anytime I update I will need to check if the lanague-server with what it works with
  # and then update this accordingly.
  codeium-server = pkgs.codeium.overrideAttrs (o: rec {
    version = "1.20.9";
    src = builtins.fetchurl {
      url = "https://github.com/Exafunction/codeium/releases/download/language-server-v${version}/language_server_macos_x64.gz";
      sha256 = "sha256:0c8gjx47ddi29lgzrziafx68q2y962lyy8agnaylnlic8jhaaqmg";
    };

  });
in
{
  programs.neovim = {
    enable = true;
    viAlias = true;

    plugins = with pkgs.vimPlugins; [
      # Setup the plugin that can lazy load others
      lz-n

      # Appearance
      {
        plugin = bufferline-nvim;
        type = "lua";
        config = builtins.readFile ./lua/bufferline.lua;
      }
      {
        plugin = indent-blankline-nvim;
        type = "lua";
        config = builtins.readFile ./lua/indent-blankline.lua;
      }
      {
        plugin = lualine-nvim;
        type = "lua";
        config = builtins.readFile ./lua/lualine.lua;
      }
      {
        plugin = alpha-nvim;
        type = "lua";
        config = builtins.readFile ./lua/alpha.lua;
      }
      nvim-web-devicons
      {
        plugin = noice-nvim;
        type = "lua";
        config = builtins.readFile ./lua/noice.lua;
      }
      {
        plugin = statuscol-nvim;
        type = "lua";
        config = builtins.readFile ./lua/statuscol.lua;
      }
      {
        plugin = nvim-ufo;
        type = "lua";
        config = builtins.readFile ./lua/ufo.lua;
      }
      {
        plugin = nvim-tree-lua;
        type = "lua";
        config = builtins.readFile ./lua/nvim-tree.lua;
        optional = true;
      }
      {
        plugin = marks-nvim;
        type = "lua";
        config = "require('marks').setup({})";
      }

      # Appearance: Themes
      catppuccin-nvim

      # DAP
      {
        plugin = nvim-dap;
        optional = true;
        type = "lua";
        config = ''
          local python_with_debugpy = "${python_with_debugpy}"
          ${builtins.readFile ./lua/dap.lua}
        '';
      }
      {
        plugin = nvim-dap-ui;
        optional = true;
      }
      {
        plugin = nvim-dap-virtual-text;
        optional = true;
      }
      {
        plugin = nvim-dap-python;
        optional = true;
      }

      # Fuzzy Finder
      {
        plugin = telescope-nvim;
        type = "lua";
        config = builtins.readFile ./lua/telescope.lua;
        optional = true;
      }
      {
        plugin = telescope-fzf-native-nvim;
        optional = true;
      }
      {
        plugin = telescope-ui-select-nvim;
        optional = true;
      }

      # Git
      {
        plugin = gitsigns-nvim;
        type = "lua";
        config = builtins.readFile ./lua/gitsigns.lua;
      }
      {
        plugin = git-conflict-nvim;
        type = "lua";
        config = builtins.readFile ./lua/git_conflict.lua;
      }
      {
        plugin = nvim-blame;
        type = "lua";
        config = # lua
          ''
            require('blame').setup()
            vim.keymap.set("n", "<leader>gB", "<cmd>BlameToggle window<cr>", {desc = "Git blame"})
          '';
      }

      # Keymaps
      {
        plugin = which-key-nvim;
        type = "lua";
        config = builtins.readFile ./lua/which-key.lua;
      }

      # navigation
      {
        plugin = nvim-bqf;
        type = "lua";
        config = ''require("bqf").setup()'';
      }
      {
        plugin = nvim-pqf;
        type = "lua";
        config = ''require("pqf").setup()'';
      }

      # Programming: LSP
      {
        plugin = nvim-lspconfig;
        type = "lua";
        config = # lua
          ''
            local extension_path = '${code_lldb}/share/vscode/extensions/vadimcn.vscode-lldb/'
            local codelldb_path = extension_path .. 'adapter/codelldb'
            local liblldb_path = extension_path .. 'lldb/lib/liblldb.dylib'
            ${builtins.readFile ./lua/lspconfig.lua}
          '';
        optional = true;
      }
      {
        plugin = SchemaStore-nvim;
        optional = true;
      }
      {
        plugin = lspsaga-nvim;
        optional = true;
      }
      {
        plugin = nvim-lint;
        type = "lua";
        config = builtins.readFile ./lua/lint.lua;
        optional = true;
      }
      {
        plugin = conform-nvim;
        type = "lua";
        config = builtins.readFile ./lua/conform.lua;
        optional = true;
      }
      {
        plugin = lsp_lines-nvim;
        type = "lua";
        config = builtins.readFile ./lua/lsp_lines.lua;
        optional = true;
      }
      # {
      #   plugin = nvim-lsp-file-operations;
      #   type = "lua";
      #   optional = true;
      #   config = ''require("lsp-file-operations").setup()'';
      # }

      # Progrmming: Treesitter
      {
        plugin = nvim-treesitter.withPlugins (
          plugins: with plugins; [
            bash
            css
            dockerfile
            haskell
            hcl
            html
            javascript
            json
            kdl
            lua
            markdown
            markdown-inline
            nix
            nu
            python
            regex
            rust
            scss
            sql
            terraform
            toml
            tsx
            typescript
            vimdoc
            yaml
          ]
        );
        type = "lua";
        config = builtins.readFile ./lua/treesitter.lua;
      }
      {
        plugin = treesj;
        type = "lua";
        config = builtins.readFile ./lua/treesj.lua;
        optional = true;
      }
      nvim-treesitter-textobjects

      # Programming: Language support
      rustaceanvim
      haskell-tools-nvim

      # Programming: Autocompletion setup
      {
        plugin = blink-cmp;
        type = "lua";
        config = ''
          local codeium_language_server_bin = "${codeium-server}/bin/codeium_language_server"
           ${builtins.readFile ./lua/blink.lua};
        '';
        optional = true;
      }
      {
        plugin = blink-compat;
        type = "lua";
        optional = true;
      }
      {
        plugin = friendly-snippets;
        optional = true;
      }

      # Programming: AI shit
      {
        plugin = avante-nvim;
        type = "lua";
        optional = true;
        config = builtins.readFile ./lua/avante.lua;
      }
      {
        plugin = img-clip-nvim;
        type = "lua";
        config = ''
          require("img-clip").setup()
        '';
      }
      # config in blink-cmp
      {
        plugin = codeium-nvim;
        optional = true;
      }

      # Programming: Database support
      vim-dotenv
      vim-dadbod
      {
        plugin = vim-dadbod-ui;
        type = "lua";
        config = # lua
          ''
            vim.g.db_ui_use_nerd_fonts = 1
            vim.g.db_ui_win_position = "right"
            vim.keymap.set("n", "<leader>Da",  "<cmd>DBUIAddConnection<cr>",{ desc = "Add new connection" })
            vim.keymap.set("n", "<leader>Do",  "<cmd>:tab DBUI<cr>",{ desc = "Open DBUI" })
          '';
      }
      vim-dadbod-completion
      nvim-dadbod-ssh

      # Programming: Testing
      {
        plugin = neotest;
        type = "lua";
        config = builtins.readFile ./lua/neotest.lua;
        optional = true;
      }
      neotest-python
      neotest-haskell
      FixCursorHold-nvim
      {
        plugin = nvim-coverage;
        type = "lua";
        config = builtins.readFile ./lua/coverage.lua;
      }

      # Text Helpers
      {
        plugin = todo-comments-nvim;
        type = "lua";
        config = # lua
          ''
            require 'todo-comments'.setup()
            vim.keymap.set("n", "<leader>sT", "<cmd>TodoTelescope<cr>", { desc = "Todo comments" })
          '';
      }
      {
        plugin = venn-nvim;
        type = "lua";
        config = builtins.readFile ./lua/venn.lua;
      }
      {
        plugin = vim-table-mode;
        type = "lua";
        config = # lua
          ''
            vim.g.table_mode_disable_mappings = 1
            vim.g.table_mode_disable_tableize_mappings = 1
            require("lz.n").load({
              "vim-table-mode",
              keys = { { "<leader>ut", "<cmd>TableModeToggle<cr>", desc = "Toggle Table Mode" } }
            })
          '';
      }

      # Text objects
      {
        plugin = nvim-autopairs;
        type = "lua";
        config = "require('nvim-autopairs').setup {}";
      }
      {
        plugin = nvim-comment;
        type = "lua";
        config = "require('nvim_comment').setup {}";
      }
      {
        plugin = nvim-surround;
        type = "lua";
        config = "require('nvim-surround').setup {}";
      }

      # File specific plugins
      {
        plugin = render-markdown-nvim;
        type = "lua";
        config = # lua
          ''
            require("lz.n").load({
              "render-markdown.nvim",
              ft = {"markdown", "Avante"},
              after = function() require('render-markdown').setup({file_types = { 'markdown', 'Avante' }}) end,
            })
          '';
        optional = true;
      }
      {
        plugin = crates-nvim;
        type = "lua";
        config = # lua
          ''
            require("lz.n").load({
              "crates.nvim",
              ft = "toml",
              after = function() require("crates").setup() end,
            })
          '';
        optional = true;
      }
    ];

    extraPackages = with pkgs; [
      # Bash
      nodePackages.bash-language-server
      shellcheck
      shfmt

      # Docker
      nodePackages.dockerfile-language-server-nodejs
      hadolint

      # grammer
      vale
      harper

      # Git
      gitlint

      # HTML/CSS/JS
      nodePackages.vscode-langservers-extracted

      # JavaScript
      nodePackages.typescript-language-server

      # lua
      stylua
      lua-language-server

      # Make
      cmake-language-server

      # Markdown
      nodePackages.markdownlint-cli
      # This is a cli utility as we can't display all this in cli
      pandoc

      # Nix
      deadnix
      statix
      nixd
      nixfmt-rfc-style

      # Python
      (python3.withPackages (
        ps: with ps; [
          python-lsp-server
          pylsp-rope
        ]
      ))

      # SQL
      postgresql

      # terraform
      terraform-lsp

      # TOML
      taplo

      # YAML
      nodePackages.yaml-language-server
      yamllint

      # general purpose / multiple langs
      nodePackages.prettier

      # utilities used by various programs
      # telescope
      ripgrep
      fd
    ];

    extraConfig = # vim
      ''
        colorscheme ${colorscheme.vim-name}
        luafile ${builtins.toString ./lua/base.lua}
      '';
  };

  home = {
    sessionVariables.NIXD_FLAGS = "-log=error";
    packages = with pkgs; [
      vim-startuptime

      nodePackages.livedown
      # Rust
      rust-analyzer
      rustfmt
      clippy
      evcxr

      # Haskell
      haskellPackages.haskell-language-server
      haskellPackages.hoogle
      haskellPackages.fast-tags
      haskellPackages.haskell-debug-adapter
      haskellPackages.ghci-dap

      # python
      pyright
      basedpyright
    ];

    file.".vale.ini".source = ./vale.ini;
    file.".markdownlintrc".source = ./markdown_lint.json;
  };
}
