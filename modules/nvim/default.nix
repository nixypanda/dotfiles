{ pkgs, colorscheme, codelldb_fixed_pkgs, ... }:
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
  venv-mypy = pkgs.writeScriptBin "venv-mypy" ''
    #!/bin/sh
    set -e
    exec poetry run pylint "$@"
  '';
in {
  programs.neovim = {
    enable = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
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
      {
        plugin = nvim-colorizer-lua;
        type = "lua";
        config = ''require "colorizer".setup { css = { rgb_fn = true, } }'';
      }
      {
        plugin = headlines-nvim;
        type = "lua";
        config = ''require("headlines").setup()'';
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

      # Appearance: Themes
      dracula-vim
      one-nvim
      tokyonight-nvim
      catppuccin-nvim

      # DAP
      nvim-dap
      {
        plugin = nvim-dap-ui;
        type = "lua";
        config = builtins.readFile ./lua/dapui.lua;
      }
      {
        plugin = nvim-dap-python;
        type = "lua";
        config = ''
          local dap_python = require("dap-python")

          dap_python.setup("${python_with_debugpy}/bin/python")
          dap_python.test_runner = "pytest"
        '';
      }
      {
        plugin = nvim-dap-go;
        type = "lua";
        config = ''require("dap-go").setup()'';
      }

      # Fuzzy Finder
      {
        plugin = telescope-nvim;
        type = "lua";
        config = builtins.readFile ./lua/telescope.lua;
      }
      telescope-fzf-native-nvim
      telescope-ui-select-nvim

      # Git
      {
        plugin = gitsigns-nvim;
        type = "lua";
        config = builtins.readFile ./lua/gitsigns.lua;
      }
      vim-fugitive

      # Keymaps
      {
        plugin = which-key-nvim;
        type = "lua";
        config = builtins.readFile ./lua/which-key.lua;
      }

      # Navigation
      {
        plugin = nvim-tree-lua;
        type = "lua";
        config = builtins.readFile ./lua/nvim-tree.lua;
      }
      vim-tmux-navigator

      # Programming: LSP
      {
        plugin = lspkind-nvim;
        type = "lua";
        config = "require('lspkind').init({})";
      }
      {
        plugin = nvim-lint;
        type = "lua";
        config = builtins.readFile ./lua/lint.lua;
      }
      {
        plugin = nvim-lspconfig;
        type = "lua";
        config = builtins.readFile ./lua/lspconfig.lua;
      }
      {
        plugin = lspsaga-nvim;
        type = "lua";
        config = builtins.readFile ./lua/lspsaga.lua;
      }
      nvim-sqls
      {
        plugin = conform-nvim;
        type = "lua";
        config = builtins.readFile ./lua/conform.lua;
      }
      {
        plugin = nvim-lsp-file-operations;
        type = "lua";
        config = ''require("lsp-file-operations").setup()'';
      }

      # Progrmming: Treesitter
      {
        plugin = (nvim-treesitter.withPlugins (plugins:
          with plugins; [
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
            lua
            markdown
            markdown-inline
            nix
            python
            regex
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
          ]));
        type = "lua";
        config = builtins.readFile ./lua/treesitter.lua;
      }
      nvim-treesitter-refactor
      nvim-treesitter-textobjects

      # Programming: Language support
      {
        plugin = crates-nvim;
        type = "lua";
        config = ''require("crates").setup()'';
      }
      yuck-vim
      {
        plugin = nvim-rustaceanvim;
        type = "lua";
        config = ''
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
      cmp-cmdline
      cmp-nvim-lsp
      cmp-nvim-lua
      cmp-path
      cmp-treesitter
      luasnip
      cmp_luasnip
      friendly-snippets

      # Programming: AI shit
      {
        plugin = codeium-vim;
        type = "lua";
        config = builtins.readFile ./lua/codeium.lua;
      }
      {
        plugin = ChatGPT-nvim;
        type = "lua";
        config =
          ''require("chatgpt").setup({ keymaps = { submit = "<C-l>" } })'';
      }

      # Programming: Code Evaluation
      conjure

      # Programming: Database support
      vim-dadbod
      vim-dadbod-ui

      # Programming: Testing
      FixCursorHold-nvim
      {
        plugin = neotest;
        type = "lua";
        config = builtins.readFile ./lua/neotest.lua;
      }
      neotest-python
      neotest-go

      # Text Helpers
      {
        plugin = nvim-regexplainer;
        type = "lua";
        config = builtins.readFile ./lua/regexplainer.lua;
      }
      {
        plugin = todo-comments-nvim;
        type = "lua";
        config = "require 'todo-comments'.setup();";
      }
      {
        plugin = venn-nvim;
        type = "lua";
        config = builtins.readFile ./lua/venn.lua;
      }
      vim-haskellConcealPlus
      vim-table-mode

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

      # Utilities
      {
        plugin = bigfile-nvim;
        type = "lua";
        config = builtins.readFile ./lua/bigfile.lua;
      }
    ];

    extraPackages = with pkgs; [
      # Bash
      nodePackages.bash-language-server
      shellcheck
      shfmt

      # Clojure
      clojure-lsp

      # dhall (broken on nix unstable)
      # dhall-lsp-server

      # Docker
      nodePackages.dockerfile-language-server-nodejs
      hadolint

      # elm
      elmPackages.elm-language-server
      elmPackages.elm
      elmPackages.elm-test
      elmPackages.elm-format

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
      # cmake-language-server

      # Markdown
      nodePackages.markdownlint-cli
      # This is a cli utility as we can't display all this in cli
      pandoc

      # Nix
      deadnix
      statix
      nil
      nixfmt

      # Python
      venv-mypy
      python_with_debugpy

      # rust
      code_lldb

      # SQL
      sqls
      postgresql

      # terraform
      terraform-lsp

      # TOML
      taplo-cli

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

    extraConfig = ''
      colorscheme catppuccin-macchiato
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
      python3Packages.isort
      nodePackages.pyright
      black
      python3Packages.flake8
      mypy
    ];

    file.".vale.ini".source = ./vale.ini;
    file.".markdownlintrc".source = ./markdown_lint.json;
  };
}
