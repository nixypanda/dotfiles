{ config, pkgs, lib, colorscheme, ... }:
let
  vimPlugsFromSource = (import ./plugins.nix) pkgs;
in
{

  home.packages = with pkgs; [
    # bash
    nodePackages.bash-language-server
    # C
    gcc
    # css
    nodePackages.vscode-css-languageserver-bin
    # cmake
    cmake-language-server
    # Clojure
    clojure
    # docker
    nodePackages.dockerfile-language-server-nodejs
    # Elm
    elmPackages.elm-language-server
    # go
    go
    gopls
    # Haskell
    ghc
    haskellPackages.cabal-install
    haskellPackages.stack
    haskellPackages.haskell-language-server
    # HTML
    nodePackages.vscode-html-languageserver-bin
    # JavaScript
    nodejs
    yarn
    # json
    nodePackages.vscode-json-languageserver-bin
    # lua
    lua
    luaformatter
    # makrdown
    nodePackages.livedown
    pandoc
    # Nix
    rnix-lsp
    # python
    (python3.withPackages (ps: with ps; [ setuptools pip debugpy ]))
    poetry
    autoflake
    python3Packages.pip
    python3Packages.black
    python3Packages.ipython
    python3Packages.isort
    python3Packages.parso
    python3Packages.twine
    nodePackages.pyright
    # rust
    rustc
    rust-analyzer
    clippy
    cargo
    rustfmt
    perl # perl (this is required by rust)
    lldb # debugging setup
    #Vim
    nodePackages.vim-language-server

    # General purpose programming language server
    nur.repos.crazazy.efm-langserver
    nodePackages.prettier
  ] ++ (
    lib.optionals (stdenv.isDarwin == false) [
      # Note: What possible reason would this have to not build on mac
      sumneko-lua-language-server
    ]
  );

  programs.neovim = {
    enable = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      # Appearance
      indentLine # vimscript
      indent-blankline-nvim
      barbar-nvim
      nvim-tree-lua
      nvim-web-devicons
      lualine-nvim
      one-nvim
      dracula-vim
      dashboard-nvim #vimscript

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
      nvim-compe
      vim-vsnip
      vim-vsnip-integ
      rust-tools-nvim
      vimPlugsFromSource.nvim-lsp-symbols-outline

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
      nvim-dap-ui
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

    extraConfig = ''
      ${builtins.readFile ./sane_defaults.vim}
      ${builtins.readFile ./dashboard.vim}

      colorscheme ${colorscheme.vim-name}

      lua << EOF
        local statusline_theme = '${colorscheme.vim-statusline}'

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
      EOF

      ${builtins.readFile ./theme.vim}
      ${builtins.readFile ./indentline.vim}
    '';

    package = pkgs.neovim-nightly;
  };
}
