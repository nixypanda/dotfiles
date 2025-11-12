{
  pkgs,
  nixpkgs-pinned,
  colorscheme,
  ...
}:
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
  # WARN: Remove this once https://github.com/nvim-neotest/neotest/issues/531 is fixed
  nvim-neotest = pkgs.lua51Packages.buildLuarocksPackage {
    pname = "neotest";
    version = "5.13.0-1";
    knownRockspec =
      (pkgs.fetchurl {
        url = "mirror://luarocks/neotest-5.13.0-1.rockspec";
        sha256 = "05jajrfvlh6phn8dblwymb17j6zm3g8cmibdzyqgxfivqm748q03";
      }).outPath;
    src = pkgs.fetchzip {
      url = "https://github.com/nvim-neotest/neotest/archive/7166dc36af2760a76479e021e0521e23f62165f1.zip";
      sha256 = "1mfdlns4y4dxf1im7rwcq264jqwm3814dlnf37c41yq13pc7fd13";
    };
    propagatedBuildInputs = [
      pkgs.vimPlugins.nvim-nio
      pkgs.vimPlugins.plenary-nvim
    ];
  };
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

        # Appearance
        (plug bufferline-nvim ./lua/bufferline.lua)
        (plug indent-blankline-nvim ./lua/indent-blankline.lua)
        (plug lualine-nvim ./lua/lualine.lua)
        (plug alpha-nvim ./lua/alpha.lua)
        nvim-web-devicons
        (plug noice-nvim ./lua/noice.lua)
        (plug statuscol-nvim ./lua/statuscol.lua)
        (plug nvim-ufo ./lua/ufo.lua)
        (lazy_plug nvim-tree-lua ./lua/nvim-tree.lua)
        catppuccin-nvim # theme

        # DAP
        (lazy_plug nvim-dap ./lua/dap.lua)
        (plug_dep nvim-dap-ui)
        (plug_dep nvim-dap-virtual-text)
        (plug_dep nvim-dap-python)

        # Fuzzy Finder
        (lazy_plug telescope-nvim ./lua/telescope.lua)
        (plug_dep telescope-fzf-native-nvim)
        (plug_dep telescope-ui-select-nvim)

        # Git
        (plug gitsigns-nvim ./lua/gitsigns.lua)
        (lazy_plug nvim-blame ./lua/blame.lua)
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
        # (plug nvim-lsp-file-operations ''require("lsp-file-operations").setup()'')

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
        (lazy_plug treesj ./lua/treesj.lua)
        nvim-treesitter-textobjects

        # Programming: Language support
        (plug rustaceanvim ./lua/rustaceanvim.lua)
        (plug haskell-tools-nvim ./lua/haskell-tools.lua)

        # Programming: Autocompletion setup
        (lazy_plug blink-cmp ./lua/blink.lua)
        (plug_dep blink-compat)
        (plug_dep friendly-snippets)

        # Programming: AI shit
        (lazy_plug avante-nvim ./lua/avante.lua)
        (plug img-clip-nvim ''require("img-clip").setup()'')
        (plug_dep windsurf-nvim) # config in blink-cmp

        # Programming: Testing
        (lazy_plug nvim-neotest ./lua/neotest.lua)
        (plug_dep neotest-python)
        (plug_dep neotest-haskell)
        FixCursorHold-nvim
        (plug nvim-coverage ./lua/coverage.lua)

        # Text Helpers
        (plug todo-comments-nvim ./lua/todo-comments.lua)
        (plug venn-nvim ./lua/venn.lua)
        (lazy_plug vim-table-mode ./lua/table-mode.lua)

        # Text objects
        (plug nvim-autopairs "require('nvim-autopairs').setup {}")
        (plug nvim-surround "require('nvim-surround').setup {}")

        # Webdev: Database
        vim-dotenv
        vim-dadbod
        (plug vim-dadbod-ui ./lua/dadbod.lua)
        vim-dadbod-completion
        nvim-dadbod-ssh

        # Webdev: API
        (lazy_plug kulala-nvim ./lua/kulala.lua)

        # File specific plugins
        (lazy_plug render-markdown-nvim ./lua/render-markdown.lua)
        (lazy_plug crates-nvim ./lua/crates.lua)
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
      # evcxr

      # Haskell
      haskellPackages.haskell-language-server
      haskellPackages.hoogle
      haskellPackages.fast-tags
      # nixpkgs-pinned.haskellPackages.haskell-debug-adapter
      # nixpkgs-pinned.haskellPackages.ghci-dap

      # python
      pyright
      basedpyright
    ];

    file.".vale.ini".source = ./vale.ini;
    file.".markdownlintrc".source = ./markdown_lint.json;
  };
}
