{ config, lib, pkgs, ... }:

let
  # A colorfull ls package
  ls-colors = pkgs.runCommand "ls-colors" {} ''
    mkdir -p $out/bin
    ln -s ${pkgs.coreutils}/bin/ls $out/bin/ls
    ln -s ${pkgs.coreutils}/bin/dircolors $out/bin/dircolors
  '';

  colorscheme = (import ./colorschemes/onedark.nix);

  custom-i3-polybar-launch = pkgs.writeScriptBin "custom-i3-polybar-launch" ''
    #!/${pkgs.stdenv.shell}

    killall -q polybar
    echo "launching polybar" | systemd-cat

    polybar top &
  '';

  custom-script-sysmenu = pkgs.writeScriptBin "custom-script-sysmenu" ''
    #!/${pkgs.stdenv.shell}
    ${builtins.readFile ./polybar/scripts/sysmenu.sh}
  '';
in
{
  home.packages = with pkgs; [
    # GUI Apps
    google-chrome
    # Just to look at how stuff looks like
    lxappearance
    i3lock-fancy

    # CLI tools
    awscli
    git
    gitAndTools.gh

    # Make that terminal pretty
    bat
    ls-colors # NOTE: custom
    direnv
    starship
    zsh-syntax-highlighting

    # Searching helpers
    fzf
    jump
    ripgrep
    universal-ctags
    xcwd

    # system info
    ytop
    neofetch

    # uncatagorised
    ngrok

    # file browsers
    ranger

    # Docker
    docker-compose

    # custom scripts
    custom-script-sysmenu
    custom-i3-polybar-launch

    # Programming

    # Clojure
    clojure
    clojure-lsp

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

    # JavaScript
    nodejs
    nodePackages.livedown

    # Nix
    rnix-lsp

    # python
    python3
    pipenv
    poetry
    python3Packages.pip
    python3Packages.black
    python3Packages.ipython
    python3Packages.pynvim
    python3Packages.isort
    python3Packages.jedi
    python3Packages.parso
    python3Packages.rope
    python3Packages.mypy
    python3Packages.flake8

    # Ruby
    bundler
    solargraph

    # rust
    rustc
    rls
    cargo
    rustfmt
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "sherub";
  home.homeDirectory = "/home/sherub";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.09";

  gtk = {
    enable = true;
    font = {
      name = "Sans 10";
    };
    iconTheme = {
      name = "Arc";
      package = pkgs.arc-icon-theme;
    };
    theme = {
      name = "Arc-Dark";
      package = pkgs.arc-theme;
    };
  };

  programs.alacritty = {
    enable = true;
    settings = (import ./alacrity-config.nix) { colors = colorscheme; };
  };

  programs.bat = {
    enable = true;
    config.theme = "OneHalfDark";
  };

  programs.git = {
    enable = true;
    userName = "Sherub Thakur";
    userEmail = "sherub.thakur@gmail.com";
  };

  programs.rofi = {
    enable = true;
    package = pkgs.rofi.override {
      plugins = [ pkgs.rofi-emoji pkgs.rofi-calc pkgs.rofi-file-browser ];
    };
    lines = 7;
    width = 40;
    font = "hack 10";
    theme = ./rofi/grid.rasi;
  };

  programs.neovim = {
    enable = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      # Appearance
      vim-airline
      vim-devicons
      awesome-vim-colorschemes
      vim-table-mode

      # Navigation
      vim-sneak
      fzf-vim

      # English
      vim-grammarous

      # Programming
      # markdown-preview
      vim-which-key
      vim-haskellConcealPlus
      vim-polyglot

      coc-nvim
      # coc-actions
      coc-eslint
      coc-explorer
      coc-go
      coc-json
      coc-pairs
      coc-prettier
      coc-python
      coc-rls
      coc-snippets
      coc-solargraph
      coc-tsserver
      coc-yaml
      coc-go

      # Text objects
      tcomment_vim
      vim-surround
      vim-repeat
      vim-indent-object

      vim-fugitive
      vim-gitgutter
    ];

    extraConfig = ''
      ${builtins.readFile ./nvim/sane_defaults.vim}
      ${builtins.readFile ./nvim/airline.vim}
      ${builtins.readFile ./nvim/navigation.vim}
      ${builtins.readFile ./nvim/coc.vim}
      ${builtins.readFile ./nvim/terminal.vim}
      ${builtins.readFile ./nvim/theme.vim}

      " Vim theme info
      colorscheme ${colorscheme.vim-name}

      " Coc highlights
      " Makes the floating window more readable
      " NOTE: Really wish the theme could overwrite this
      highlight CocErrorSign ctermfg=204 guifg=${colorscheme.alert}
      highlight CocWarningSign ctermfg=173 guifg=${colorscheme.warning}
      highlight Pmenu ctermbg=237 guibg=${colorscheme.fg-secondary}

      ${builtins.readFile ./nvim/which_key.vim}
    '';

  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    defaultKeymap = "viins";
    enableAutosuggestions = true;
    shellAliases = (import ./zsh/aliases.nix);
    history.extended = true;
    plugins = [
      {
        name = "zsh-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-syntax-highlighting";
          rev = "be3882aeb054d01f6667facc31522e82f00b5e94";
          sha256 = "0w8x5ilpwx90s2s2y56vbzq92ircmrf0l5x8hz4g1nx3qzawv6af";
        };
      }
    ];
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "vi-mode" ];
    };
    initExtraBeforeCompInit = ''
      ${builtins.readFile ./zsh/session_variables.zsh}
      ${builtins.readFile ./zsh/functions.zsh}
      ${builtins.readFile ./zsh/secrets.zsh}

      bindkey -M vicmd 'k' history-beginning-search-backward
      bindkey -M vicmd 'j' history-beginning-search-forward

      eval "$(jump shell zsh)"
      alias ls="ls --color=auto -F"

      eval "$(starship init zsh)"
    '';
  };

  services.random-background = {
    enable = true;
    imageDirectory = "%h/Pictures/backgrounds";
  };

  services.polybar = {
    enable = true;
    config = (import ./polybar/accented-pills.nix) { colors = colorscheme; };
    package = pkgs.polybar.override { i3GapsSupport = true; };
    script = "polybar top &";
  };

  services.picom = {
    enable = true;
    blur = false;
    fade = true;
    fadeDelta = 5;
    # inactiveDim = "0.1";
    inactiveOpacity = "0.8";
    shadow = true;
    experimentalBackends = true;
    extraOptions = ''
      focus-exclude = [ "class_g ?= 'rofi'" ];
    '';
  };

  xsession = {
    enable = true;
    scriptPath = ".hm-xsession";
    windowManager.i3 = rec {
      enable = true;
      package = pkgs.i3-gaps;
      config = (import ./i3/config.nix) { colorscheme = colorscheme; };
    };
  };
}
