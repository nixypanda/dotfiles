{ config, lib, pkgs, ... }:

let
  colorscheme = (import ./colorschemes/onedark.nix);

  custom-panel-launch = pkgs.writeScriptBin "custom-panel-launch" ''
    #!/${pkgs.stdenv.shell}

    killall -q polybar
    killall -q volumeicon

    polybar main &
    polybar powermenu &
    nm-applet &
    volumeicon &
    solaar -w hide -b symbolic &
    caffeine &
  '';

  custom-script-sysmenu = pkgs.writeScriptBin "custom-script-sysmenu" ''
    #!/${pkgs.stdenv.shell}
    ${builtins.readFile ./polybar/scripts/sysmenu.sh}
  '';

  custom-browsermediacontrol = pkgs.stdenv.mkDerivation {
    name = "custom-browsermediacontrol";
    buildInputs = with pkgs; [
      pkg-config
      cairo
      gobject-introspection
      (python3.withPackages (python3Packages: with python3Packages; [
        pydbus
        pygobject3
      ]))
    ];
    unpackPhase = ":";
    installPhase = ''
      mkdir -p $out/bin
      cp ${./bmc/bmc.py} $out/bin/custom-browsermediacontrol
      chmod +x $out/bin/custom-browsermediacontrol
    '';
  };

  color-dim = color: color;

in
{
  home.packages = with pkgs; [
    # GUI Apps
    google-chrome
    # Just to look at how stuff looks like
    lxappearance
    i3lock-fancy

    # system tray (Kind of a hack atm)
    # Need polybar to support this as a first class module
    gnome3.networkmanagerapplet
    volumeicon
    solaar
    caffeine-ng
    psensor

    # CLI tools / Terminal facification
    awscli
    git
    gitAndTools.gh
    ngrok
    # Moar colors
    gitAndTools.delta
    less
    bat
    direnv
    starship
    zsh-syntax-highlighting
    # Searching/Movement helpers
    fzf
    jump
    ripgrep
    universal-ctags
    xcwd
    # system info
    ytop
    neofetch
    # file browser
    ranger
    # screenshot utility
    scrot
    # custom scripts
    custom-script-sysmenu
    custom-panel-launch
    # Music shit
    # Note: Turn this into a singular package
    plasma-browser-integration
    custom-browsermediacontrol

    # Fonts
    (nerdfonts.override { fonts = [ "Hack" ]; })

    # Themes (GTK)
    arc-icon-theme
    arc-theme

    # Docker
    docker-compose

    # Programming

    # C
    gcc

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

    # perl
    perl

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

  home.file.".config/google-chrome/NativeMessagingHosts".source = pkgs.symlinkJoin {
    name = "native-messaging-hosts";
    paths = [
      "${pkgs.plasma-browser-integration}/etc/var/empty/chrome/native-messaging-hosts"
    ];
  };

  gtk = {
    enable = true;
    font = { name = "TeX Gyre Heros 10"; };
    iconTheme = { name = colorscheme.gtk-icon-name; };
    theme = { name = colorscheme.gtk-name; };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.alacritty = {
    enable = true;
    settings = (import ./alacritty/config.nix) { colors = colorscheme; };
  };

  programs.bat = {
    enable = true;
    config.theme = colorscheme.bat-theme-name;
  };

  programs.git = {
    enable = true;
    userName = "Sherub Thakur";
    userEmail = "sherub.thakur@gmail.com";
    extraConfig = {
      core = {
        pager = "delta";
      };
      delta = {
        features = "side-by-side line-numbers decorations";
      };
      "delta \"decorations\"" = {
        commit-decoration-style = "bold yellow box ul";
        file-style = "bold yellow";
        file-decoration-style = "none";
      };
    };
  };

  programs.rofi = {
    enable = true;
    package = pkgs.rofi.override {
      plugins = [ pkgs.rofi-emoji pkgs.rofi-calc pkgs.rofi-file-browser ];
    };
    lines = 7;
    width = 40;
    font = "hack 10";
  };
  home.file.".config/rofi/colors.rasi".text = ''
    * {
      accent: ${colorscheme.accent-primary};
      background: ${colorscheme.bg-primary};
      foreground: ${colorscheme.fg-primary};
    }
  '';
  home.file.".config/rofi/grid.rasi".source = ./rofi/grid.rasi;
  home.file.".config/rofi/sysmenu.rasi".source = ./rofi/sysmenu.rasi;

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

  # coc-config for vim
  home.file.".config/nvim/coc-settings.json".source = ./nvim/coc-settings.json;

  programs.nushell = {
    enable = true;
    settings = {
      edit_mode = "vi";
      prompt = "echo $(starship prompt)";
    };
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
    script = "polybar main &";
  };

  services.picom = {
    enable = true;
    blur = true;
    fade = true;
    fadeDelta = 5;
    inactiveOpacity = "0.8";
    shadow = true;
    experimentalBackends = true;
    extraOptions = ''
      focus-exclude = [ "class_g ?= 'rofi'" ];
      blur-strength = 20;
    '';
  };

  xsession = {
    enable = true;
    scriptPath = ".hm-xsession";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = pkgs.writeText "xmonad.hs" ''
        ${builtins.readFile ./xmonad.hs}

        myFocusedBorderColor = "${colorscheme.accent-primary}"
      '';
    };
  };

  xresources = {
    properties = {
      "*.foreground" = colorscheme.fg-primary;
      "*.background" = colorscheme.bg-primary;

      "*.color0"  = color-dim(colorscheme.black);
      "*.color1"  = color-dim(colorscheme.red);
      "*.color2"  = color-dim(colorscheme.green);
      "*.color3"  = color-dim(colorscheme.yellow);
      "*.color4"  = color-dim(colorscheme.blue);
      "*.color5"  = color-dim(colorscheme.magenta);
      "*.color6"  = color-dim(colorscheme.cyan);
      "*.color7"  = color-dim(colorscheme.white);

      "*.color8"  = colorscheme.black;
      "*.color9"  = colorscheme.red;
      "*.color10" = colorscheme.green;
      "*.color11" = colorscheme.yellow;
      "*.color12" = colorscheme.blue;
      "*.color13" = colorscheme.magenta;
      "*.color14" = colorscheme.cyan;
      "*.color15" = colorscheme.white;

      "XTerm*font" = "xft:Hack Nerd Font Mono:pixelsize=12";

      "Xft.dpi" = 96;
      "Xft.antialias" = true;
      "Xft.hinting" = true;
      "Xft.rgba" = "rgb";
      "Xft.autohint" = false;
      "Xft.hintstyle" = "hintslight";
      "Xft.lcdfilter" = "lcddefault";
    };
  };
}
