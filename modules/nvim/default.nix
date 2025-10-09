{ pkgs, colorscheme, ... }:
let
  # Caveat: This requires Xcode.app installed on the system
  code_lldb = pkgs.vscode-extensions.vadimcn.vscode-lldb;

  python_with_debugpy = pkgs.python3.withPackages (ps: with ps; [ debugpy ]);

  # This sucks
  # The windsurf-nvim plugin works with specific version of the language server
  # now anytime I update I will need to check if the lanague-server with what it works with
  # and then update this accordingly.
  codeium-server = pkgs.codeium.overrideAttrs (o: rec {
    version = "1.20.9";
    src = builtins.fetchurl {
      url = "https://github.com/Exafunction/windsurf/releases/download/language-server-v${version}/language_server_macos_x64.gz";
      sha256 = "sha256:0c8gjx47ddi29lgzrziafx68q2y962lyy8agnaylnlic8jhaaqmg";
    };

  });
in
{
  xdg.configFile."nvim/lua/common.lua".source = ./lua/common.lua;
  xdg.configFile."nvim/lua/nix_injected.lua".text = ''
    local extension_path = '${code_lldb}/share/vscode/extensions/vadimcn.vscode-lldb/'
    return {
         dap_python_with_debugpy  = "${python_with_debugpy}",
         rustaceanvim_codelldb_path  = extension_path .. 'adapter/codelldb',
         rustaceanvim_liblldb_path  = extension_path .. 'lldb/lib/liblldb.dylib',
         treesitter_kulala_grammer_location  = "${pkgs.vimPlugins.nvim-treesitter-kulala-http}",
         blink_codeium_language_server_bin = "${codeium-server}/bin/codeium_language_server",
    }
  '';
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

      # Appearance: Themes
      catppuccin-nvim

      # DAP
      {
        plugin = nvim-dap;
        optional = true;
        type = "lua";
        config = builtins.readFile ./lua/dap.lua;
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
        plugin = nvim-blame;
        type = "lua";
        config = builtins.readFile ./lua/blame.lua;
        optional = true;
      }
      {
        plugin = diffview-nvim;
        type = "lua";
        config = builtins.readFile ./lua/diffview.lua;
        optional = true;
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
        config = builtins.readFile ./lua/lspconfig.lua;
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
            nvim-treesitter-kulala-http
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
      {
        plugin = rustaceanvim;
        type = "lua";
        config = builtins.readFile ./lua/rustaceanvim.lua;
      }
      {
        plugin = haskell-tools-nvim;
        type = "lua";
        config = builtins.readFile ./lua/haskell-tools.lua;
      }

      # Programming: Autocompletion setup
      {
        plugin = blink-cmp;
        type = "lua";
        config = builtins.readFile ./lua/blink.lua;
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
        plugin = windsurf-nvim;
        optional = true;
      }

      # Programming: Database support
      vim-dotenv
      vim-dadbod
      {
        plugin = vim-dadbod-ui;
        type = "lua";
        config = builtins.readFile ./lua/dadbod.lua;
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
      {
        plugin = neotest-python;
        type = "lua";
        optional = true;
      }
      {
        plugin = neotest-haskell;
        type = "lua";
        optional = true;
      }
      FixCursorHold-nvim
      {
        plugin = nvim-coverage;
        type = "lua";
        config = builtins.readFile ./lua/coverage.lua;
      }

      {
        plugin = kulala-nvim;
        type = "lua";
        config = builtins.readFile ./lua/kulala.lua;
        optional = true;
      }

      # Text Helpers
      {
        plugin = todo-comments-nvim;
        type = "lua";
        config = builtins.readFile ./lua/todo_comments.lua;
      }
      {
        plugin = venn-nvim;
        type = "lua";
        config = builtins.readFile ./lua/venn.lua;
      }
      {
        plugin = vim-table-mode;
        type = "lua";
        config = builtins.readFile ./lua/table_mode.lua;
        optional = true;
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
        config = builtins.readFile ./lua/render_markdown.lua;
        optional = true;
      }
      {
        plugin = crates-nvim;
        type = "lua";
        config = builtins.readFile ./lua/crates.lua;
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
      python3

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
