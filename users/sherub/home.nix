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
    solaar -w hide &
  '';

  custom-script-sysmenu = pkgs.writeScriptBin "custom-script-sysmenu" ''
    #!/${pkgs.stdenv.shell}
    ${builtins.readFile ./polybar/scripts/sysmenu.sh}
  '';

  custom-browsermediacontrol =
    (import ./browser-media-control/default.nix) { pkgs = pkgs; };
  
  vimPlugsFromSource = (import ./nvim/plugins.nix) pkgs;

in
{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
    }))
  ];

  home.packages = with pkgs; [
    # GUI Apps
    google-chrome
    slack
    discord
    sxiv
    exiv2
    imagemagick
    # Screen Locker
    i3lock-fancy
    # Theming (GTK)
    lxappearance
    arc-icon-theme
    arc-theme
    dracula-theme
    # system tray (Kind of a hack atm)
    # Need polybar to support this as a first class module
    gnome3.networkmanagerapplet
    volumeicon
    solaar
    psensor
    gnome3.nautilus
    p3x-onenote

    # CLI tools / Terminal facification
    awscli
    git
    gitAndTools.gh
    ngrok
    gnumake
    # Moar colors
    gitAndTools.delta
    starship
    zsh-syntax-highlighting
    # Searching/Movement helpers
    fzf
    zoxide
    ripgrep
    universal-ctags
    xcwd
    # system info
    bottom
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
    # Busybox replacements
    usbutils
    pciutils
    less
    tokei

    # Fonts
    (nerdfonts.override { fonts = [ "Hack" ]; })

    # Docker
    docker-compose

    openvpn
    gnome3.networkmanager-openvpn

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
    yarn

    # lua
    lua
    sumneko-lua-language-server

    # Nix
    rnix-lsp

    # perl
    perl

    # python
    (python3.withPackages (ps: with ps; [ setuptools ]))
    pipenv
    poetry
    autoflake
    python3Packages.pip
    python3Packages.black
    python3Packages.ipython
    python3Packages.pynvim
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

  programs.direnv = {
    enable = true;
    enableNixDirenvIntegration = true;
  };

  programs.git = {
    enable = true;
    userName = "Sherub Thakur";
    userEmail = "sherub.thakur@gmail.com";
    extraConfig = {
      core = {
        pager = "delta";
      };
      pull.ff = "only";
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

  programs.kitty = {
    enable = true;
    settings = {
      font_size = 10;
      shell = "zsh";
      scrollback_lines = 10000;
      input_delay = 1;

      foreground = "${colorscheme.fg-primary}";
      background = "${colorscheme.bg-primary}";

      color0  = "${colorscheme.black}";
      color1  = "${colorscheme.red}";
      color2  = "${colorscheme.green}";
      color3  = "${colorscheme.yellow}";
      color4  = "${colorscheme.blue}";
      color5  = "${colorscheme.magenta}";
      color6  = "${colorscheme.cyan}";
      color7  = "${colorscheme.white}";
      color8  = "${colorscheme.bright-black}";
      color9  = "${colorscheme.bright-red}";
      color10 = "${colorscheme.bright-green}";
      color11 = "${colorscheme.bright-yellow}";
      color12 = "${colorscheme.bright-blue}";
      color13 = "${colorscheme.bright-magenta}";
      color14 = "${colorscheme.bright-cyan}";
      color15 = "${colorscheme.bright-white}";
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

  home.file.".config/sxiv/exec/image-info".text = ''
    ${builtins.readFile ./sxiv/image_info.sh}
  '';

  # systray stuff
  home.file.".config/volumeicon/volumeicon".source = ./systray/volumeicon.cfg;

  programs.neovim = {
    enable = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      # Appearance
      vim-table-mode # vimscript
      barbar-nvim
      nvim-tree-lua
      nvim-web-devicons
      galaxyline-nvim
      one-nvim

      # Programming
      vim-which-key          # vimscript
      vim-haskellConcealPlus # vimscript
      vim-polyglot           # vimscript
      lspkind-nvim
      nvim-treesitter
      nvim-treesitter-refactor
      nvim-treesitter-textobjects
      nvim-lspconfig
      lspsaga-nvim
      nvim-compe

      # Text objects
      tcomment_vim # vimscript
      vim-surround # vimscript
      vim-repeat   # vimscript
      delimitMate  # vimscript

      # Git
      vim-fugitive  # vimscript
      vim-gitgutter # vimscript

      # DAP
      vimspector # vimscript

      # Fuzzy Finder
      telescope-nvim

      # General Deps
      # popup-nvim
      # Note: Current version on nixpkgs seems broken
      vimPlugsFromSource.nvim-popup
      plenary-nvim
    ];

    extraConfig = ''
      " NOTE: For some reason these settings don't have any affect if configured
      " in lua
      set number relativenumber
      set colorcolumn=100

      lua << EOF
        ${builtins.readFile ./nvim/sane_defaults.lua}
        ${builtins.readFile ./nvim/treesitter.lua}
        ${builtins.readFile ./nvim/telescope.lua}
        ${builtins.readFile ./nvim/lsp.lua}
        ${builtins.readFile ./nvim/statusline.lua}
      EOF

      " Vim theme info
      colorscheme one-nvim
      ${builtins.readFile ./nvim/theme.vim}

      "" lsp shit that can't be done in lua atm
      autocmd BufWritePre *.rs lua vim.lsp.buf.formatting_sync(nil, 1000)
      autocmd BufWritePre *.hs lua vim.lsp.buf.formatting_sync(nil, 1000)

      ${builtins.readFile ./nvim/which_key.vim}
    '';

    package = pkgs.neovim-nightly;
  };

  programs.nushell = {
    enable = true;
    settings = {
      edit_mode = "vi";
      # figure out how to use z add here
      prompt = "za $(pwd) | echo $(starship prompt)";
      startup = [
        "def z [a] {if $(echo $a | empty?) == $true { cd ~ } {echo $a | each { if $it == '-' { cd - } {cd $(zoxide query -- $it | str trim)}}}}"
        "def zi [a] {echo $(do -i {zoxide query -i $a} | str trim) | each { cd $it }}"
        "def za [a:block] {zoxide add $a}"
        "def zq [a] {zoxide query -- $a}"
        "def zl [] {zoxide query --list}"
        "def zqi [a] {echo $(do -i {zoxide query -i $a | str trim }) | each { cd $it }}"
        "def zr [a] {zoxide remove $a}"
        "def zri [a] {zoxide remove -i $a}"
        "def dce [container command] { docker-compose exec $container /bin/bash -c '$command' }"
        "def dcpytest [container] { docker-compose exec $container /bin/bash -c 'pytest' }"
        "def dcpytestlf [container] { docker-compose exec $container /bin/bash -c 'pytest --lf' }"
        "def dcpytestni [container] { docker-compose exec $container /bin/bash -c 'pytest -m \"not integration\"' }"
      ] ++ (lib.mapAttrsToList (k: v: "alias ${k} = ${v}") (import ./zsh/aliases.nix));
      color_config = {
        # green|g, red|r, blue|u, black|b, yellow|y, purple|p, cyan|c, white|w
        primitive_int = "wb";
        primitive_decimal = "wb";
        primitive_boolean = "cyan";
        primitive_binary = "cyan";
        primitive_date = "wd";
        primitive_filesize = "rb";
        primitive_string = "gb";
        primitive_line = "yellow";
        primitive_columnpath = "cyan";
        primitive_pattern = "white";
        primitive_duration = "blue";
        primitive_range = "purple";
        primitive_path = "ub";
        separator_color = "grey";
        header_align = "l"; # left|l, right|r, center|c
        header_color = "blue";
        header_bold = true;
        index_color = "wd";
        # leading_trailing_space_bg = "white";
      };
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

      eval "$(direnv hook zsh)"

      bindkey -M vicmd 'k' history-beginning-search-backward
      bindkey -M vicmd 'j' history-beginning-search-forward

      alias ls="ls --color=auto -F"
      eval "$(zoxide init zsh)"

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
    # inactiveOpacity = "0.55";
    # activeOpacity = "0.85";
    blur = true;
    experimentalBackends = true;
    opacityRule = [
      "100:class_g   *?= 'Google-chrome'"
    ];
    extraOptions = ''
      # blur-method = "dual_kawase";
      # blur-strength = 8;
      # corner-radius = 8;
      # round-borders = 1;
      #
      # rounded-corners-exclude = [
      #   "class_g = 'Polybar'",
      #   "class_g = 'Google-chrome'"
      # ];
    '';
    fade = true;
    fadeDelta = 5;
  };

  xsession = {
    enable = true;
    scriptPath = ".hm-xsession";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = pkgs.writeText "xmonad.hs" ''
        ${builtins.readFile ./xmonad/config.hs}

        myFocusedBorderColor = "${colorscheme.accent-primary}"
        myNormalBorderColor = "${colorscheme.bg-primary-bright}"
      '';
    };
  };

  xresources = {
    properties = {
      "*.foreground" = colorscheme.fg-primary;
      "*.background" = colorscheme.bg-primary;

      "*.color0"  = colorscheme.black;
      "*.color1"  = colorscheme.red;
      "*.color2"  = colorscheme.green;
      "*.color3"  = colorscheme.yellow;
      "*.color4"  = colorscheme.blue;
      "*.color5"  = colorscheme.magenta;
      "*.color6"  = colorscheme.cyan;
      "*.color7"  = colorscheme.white;

      "*.color8"  = colorscheme.bright-black;
      "*.color9"  = colorscheme.bright-red;
      "*.color10" = colorscheme.bright-green;
      "*.color11" = colorscheme.bright-yellow;
      "*.color12" = colorscheme.bright-blue;
      "*.color13" = colorscheme.bright-magenta;
      "*.color14" = colorscheme.bright-cyan;
      "*.color15" = colorscheme.bright-white;

      "XTerm*font" = "xft:Hack Nerd Font Mono:pixelsize=12";
      "*.internalBorder" = 4;

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
