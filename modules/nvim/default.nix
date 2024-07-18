{
  pkgs,
  colorscheme,
  codelldb_fixed_pkgs,
  ...
}:
let
  # Using a fixed version of codelldb which works on Mac.
  # We get this from an alternate nixpkgs repo.
  # Caveat: This requires Xcode.app installed on the system
  # NOTE: https://github.com/NixOS/nixpkgs/pull/211321
  code_lldb = codelldb_fixed_pkgs.vscode-extensions.vadimcn.vscode-lldb;

  python_with_debugpy = pkgs.python3.withPackages (ps: with ps; [ debugpy ]);

  tree-sitter-nu = pkgs.callPackage ./plugins/nvim-treesitter-nu.nix {
    inherit (pkgs.tree-sitter) buildGrammar;
  };

  # This sucks
  # The codeium-nvim plugin works with specific version of the language server
  # now anytime I update I will need to check if the lanague-server with what it works with
  # and then update this accordingly.
  codeium-1-8-25 = pkgs.codeium.overrideAttrs (o: {
    src = builtins.fetchurl {
      url = "https://github.com/Exafunction/codeium/releases/download/language-server-v1.8.80/language_server_macos_x64.gz";
      sha256 = "sha256:0j0qgjj267fxhj6dwl15hshav8n6n87kiva6dfrjfnklk9hxxzyh";
    };

  });
in
{
  programs.neovim = {
    enable = true;
    vimAlias = true;

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
      {
        plugin = nvim-dap-go;
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
            vim.keymap.set("n", "<leader>gb", "<cmd>BlameToggle window<cr>", {desc = "Git blame"})
          '';
      }

      # Keymaps
      {
        plugin = which-key-nvim;
        type = "lua";
        config = builtins.readFile ./lua/which-key.lua;
      }

      # Programming: LSP
      {
        plugin = nvim-lspconfig;
        type = "lua";
        config = builtins.readFile ./lua/lspconfig.lua;
        optional = true;
      }
      {
        plugin = nvim-lsp-file-operations;
        type = "lua";
        config = ''require("lsp-file-operations").setup()'';
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

      # Progrmming: Treesitter
      {
        plugin = nvim-treesitter.withPlugins (
          plugins: with plugins; [
            bash
            c
            css
            dhall
            dockerfile
            elixir
            elm
            go
            haskell
            hcl
            html
            java
            javascript
            json
            latex
            kdl
            lua
            markdown
            markdown-inline
            nix
            python
            regex
            ruby
            rust
            scss
            sql
            terraform
            toml
            tsx
            typescript
            vim
            vimdoc
            yaml

            tree-sitter-nu.grammar
          ]
        );
        type = "lua";
        config = builtins.readFile ./lua/treesitter.lua;
      }
      nvim-treesitter-textobjects

      # Programming: Language support
      {
        plugin = rustaceanvim;
        type = "lua";
        config = # lua
          ''
            local extension_path = '${code_lldb}/share/vscode/extensions/vadimcn.vscode-lldb/'
            local codelldb_path = extension_path .. 'adapter/codelldb'
            local liblldb_path = extension_path .. 'lldb/lib/liblldb.dylib'
            ${builtins.readFile ./lua/rustaceanvim.lua}
          '';
      }
      {
        plugin = haskell-tools-nvim;
        type = "lua";
        config = builtins.readFile ./lua/haskell-tools.lua;
      }

      # Programming: Autocompletion setup
      {
        plugin = nvim-cmp;
        type = "lua";
        config = builtins.readFile ./lua/cmp.lua;
      }
      cmp-buffer
      cmp-calc
      cmp-nvim-lsp
      cmp-nvim-lua
      cmp-path
      luasnip
      cmp_luasnip
      friendly-snippets
      lspkind-nvim

      # Programming: AI shit
      {
        plugin = codeium-nvim;
        type = "lua";
        config = # lua
          ''
            require('lz.n').load{
              "codeium.nvim",
              event = "InsertEnter",
              after = function()
                require('codeium').setup({
                  tools = {
                      language_server = "${codeium-1-8-25}/bin/codeium_language_server"
                  }
                })
                end
              }
          '';
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
            vim.keymap.set("n", "<leader>Da",  "<cmd>DBUIAddConnection<cr>",{ desc = "[A]dd new connection" })
            vim.keymap.set("n", "<leader>Dt",  "<cmd>DBUIToggle<cr>",{ desc = "Toggle DBUI" })
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
      neotest-go
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
        plugin = headlines-nvim;
        type = "lua";
        config = # lua
          ''
            require("lz.n").load({
              "headlines.nvim",
              ft = "markdown",
              after = function() require("headlines").setup() end,
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

      # Git
      gitlint

      # Go
      gopls
      delve

      # HTML/CSS/JS
      nodePackages.vscode-langservers-extracted

      # Java
      java-language-server

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

      # Vimscript
      nodePackages.vim-language-server

      # YAML
      nodePackages.yaml-language-server
      yamllint

      # general purpose / multiple langs
      efm-langserver
      nodePackages.prettier

      # utilities used by various programs
      # telescope
      ripgrep
      fd

      ltex-ls
    ];

    extraConfig = # vim
      ''
        colorscheme ${colorscheme.vim-name}
        luafile ${builtins.toString ./lua/base.lua}
      '';
  };

  xdg.configFile = {
    "nvim/queries/nu/highlights.scm".text = tree-sitter-nu.highlights;
    "nvim/queries/nu/injections.scm".text = tree-sitter-nu.injections;
  };

  home = {

    packages = with pkgs; [
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
