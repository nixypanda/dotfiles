{ config, pkgs, lib, colorscheme, ... }:
let
  vimPlugsFromSource = (import ./plugins.nix) pkgs;
in
{
  home.packages = with pkgs; [
    # C
    gcc
    # Clojure
    clojure
    # go
    go
    # Haskell
    ghc
    haskellPackages.cabal-install
    haskellPackages.stack
    # JavaScript
    nodejs
    yarn
    # lua
    lua
    # makrdown
    nodePackages.livedown
    pandoc
    # python
    (python3.withPackages (ps: with ps; [ setuptools pip debugpy ]))
    poetry
    autoflake
    python3Packages.pip
    python3Packages.ipython
    python3Packages.parso
    python3Packages.twine
    # rust
    rustc
    cargo
    rustfmt
    cargo-tarpaulin
    perl # perl (this is required by rust)
    lldb # debugging setup
  ] ++ (lib.optional pkgs.stdenv.isLinux [ sumneko-lua-language-server ]);

  programs.neovim = {
    enable = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      # Appearance
      indent-blankline-nvim
      barbar-nvim
      nvim-tree-lua
      nvim-web-devicons
      lualine-nvim
      one-nvim
      dracula-vim
      vimPlugsFromSource.nvim-alpha

      # Programming
      which-key-nvim
      vim-haskellConcealPlus # vimscript
      vim-nix # vimscript
      lspkind-nvim
      nvim-treesitter
      nvim-treesitter-refactor
      nvim-treesitter-textobjects
      nvim-lspconfig
      lspsaga-nvim
      lsp_signature-nvim
      ## Autocompletion setup
      nvim-cmp
      cmp-path
      cmp-vsnip
      cmp-buffer
      cmp-nvim-lsp
      cmp-nvim-lua
      cmp-treesitter
      cmp-calc

      vim-vsnip
      vim-vsnip-integ
      rust-tools-nvim
      symbols-outline-nvim

      # Text objects
      tcomment_vim # vimscript
      vim-surround # vimscript
      vim-repeat # vimscript
      nvim-autopairs

      # Git
      vim-fugitive # vimscript
      gitsigns-nvim

      # DAP
      nvim-dap
      # nvim-dap-ui
      vimPlugsFromSource.nvim-dap-python

      # Fuzzy Finder
      telescope-nvim

      # Text Helpers
      vim-table-mode # vimscript
      todo-comments-nvim

      # General Deps
      popup-nvim
      plenary-nvim
    ];

    # cmake = {"${pkgs.cmake-language-server}/bin/cmake-language-server"},
    extraConfig = ''
      ${builtins.readFile ./sane_defaults.vim}

      colorscheme ${colorscheme.vim-name}

      lua << EOF
      ${builtins.readFile ./dashboard.lua}
        local statusline_theme = '${colorscheme.vim-statusline}'

        local lang_servers_cmd = {
          bashls = {"${pkgs.nodePackages.bash-language-server}/bin/bash-language-server", "start"},
          cssls = {"${pkgs.nodePackages.vscode-css-languageserver-bin}/bin/css-languageserver", "--stdio"},
          dockerls = {"${pkgs.nodePackages.dockerfile-language-server-nodejs}/bin/docker-langserver", "--stdio"},
          elmls = {"${pkgs.elmPackages.elm-language-server}/bin/elm-language-server"},
          gopls = {"${pkgs.gopls}/bin/gopls"},
          hls = {"${pkgs.haskellPackages.haskell-language-server}/bin/haskell-language-server", "--lsp"},
          html = {"${pkgs.nodePackages.vscode-html-languageserver-bin}/bin/html-languageserver", "--stdio"},
          jsonls = {"${pkgs.nodePackages.vscode-json-languageserver-bin}/bin/json-languageserver", "--stdio"},
          pyright = {"${pkgs.nodePackages.pyright}/bin/pyright-langserver", "--stdio"},
          rnix = {"${pkgs.rnix-lsp}/bin/rnix-lsp"},
          rust_analyzer = {"${pkgs.rust-analyzer}/bin/rust-analyzer"},
          tsserver = {"${pkgs.nodePackages.typescript-language-server}/bin/typescript-language-server", "--stdio"},
          vimls = {"${pkgs.nodePackages.vim-language-server}/bin/vim-language-server", "--stdio"},
          yamlls = {"${pkgs.nodePackages.yaml-language-server}/bin/yaml-language-server", "--stdio"},
          efmls = {"${pkgs.nur.repos.crazazy.efm-langserver}/bin/efm-langserver"},

          prettier = "${pkgs.nodePackages.prettier}/bin/prettier",
          isort = "${pkgs.python3Packages.isort}/bin/isort",
          black = "${pkgs.python3Packages.black}/bin/black",
          lua_format = "${pkgs.luaformatter}/bin/lua-format",
          clippy = "${pkgs.clippy}/bin/cargo-clippy",
          rustfmt = "${pkgs.rustfmt}/bin/cargo-fmt",

          elm = "${pkgs.elmPackages.elm}/bin/elm",
          elm_test = "${pkgs.elmPackages.elm-test}/bin/elm-test",
          elm_format = "${pkgs.elmPackages.elm-format}/bin/elm-format",
        }

        ${builtins.readFile ./nvim-tree.lua}
        ${builtins.readFile ./sane_defaults.lua}
        ${builtins.readFile ./treesitter.lua}
        ${builtins.readFile ./telescope.lua}

        ${builtins.readFile ./lsp.lua}
        ${builtins.readFile ./dap.lua}
        ${builtins.readFile ./statusline.lua}
        ${builtins.readFile ./git.lua}
        ${builtins.readFile ./todo.lua}
        ${builtins.readFile ./which_key.lua}

        ${builtins.readFile ./dummy.lua}
      EOF

      ${builtins.readFile ./theme.vim}
      ${builtins.readFile ./indentline.vim}
    '';
  };
}
