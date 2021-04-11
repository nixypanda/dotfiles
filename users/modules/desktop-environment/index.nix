{ config, pkgs, libs, ... }:
let
  colorscheme = (import ../../colorschemes/onedark.nix);
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
in
  {
    home.packages = with pkgs; [
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

      # custom scripts
      custom-script-sysmenu
      custom-panel-launch
      custom-browsermediacontrol

      # Required so that BMC can work with chrome
      plasma-browser-integration

      # file browser
      ranger
      # screenshot utility
      scrot
      # image viewer
      feh
    ];

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

    # systray stuff
    home.file.".config/volumeicon/volumeicon".source = ./systray/volumeicon.cfg;

    gtk = {
      enable = true;
      font = { name = "TeX Gyre Heros 10"; };
      iconTheme = { name = colorscheme.gtk-icon-name; };
      theme = { name = colorscheme.gtk-name; };
    };

    services.polybar = {
      enable = true;
      config = (import ./polybar/accented-pills.nix) { colors = colorscheme; };
      script = "polybar main &";
    };

    services.random-background = {
      enable = true;
      imageDirectory = "%h/Pictures/backgrounds";
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
  }
