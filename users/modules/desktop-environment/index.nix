{ config, pkgs, lib, colorscheme, ... }:
let
  custom-panel-launch = pkgs.writeScriptBin "custom-panel-launch" ''
    #!/${pkgs.stdenv.shell}

    killall -q polybar
    killall -q volumeicon

    polybar main &
    polybar powermenu &
    nm-applet &
    volumeicon &
    deadd-notification-center &
    solaar -w hide &
    blueman-applet &
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
    dracula-theme
    moka-icon-theme
    numix-icon-theme-square

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
    # Utility to open present directory (Only use it with xmonad to open
    # terminal in same directory)
    xcwd

    psmisc

    nur.repos.fortuneteller2k.impure.eww

    # Notifications
    deadd-notification-center
    notify-desktop
  ];

  home.file.".config/google-chrome/NativeMessagingHosts".source = pkgs.symlinkJoin {
    name = "native-messaging-hosts";
    paths = [
      "${pkgs.plasma-browser-integration}/etc/var/empty/chrome/native-messaging-hosts"
    ];
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
  home.file.".config/deadd/deadd.css".source = ./deadd/dracula.css;

  services.picom = {
    enable = true;
    inactiveOpacity = "0.50";
    activeOpacity = "0.90";
    blur = true;
    experimentalBackends = true;
    opacityRule = [
      "100:class_g   *?= 'Google-chrome'"
      "95:class_g   *?= 'Deadd-notification-center'"
      "75:class_g   *?= 'Rofi'"
    ];
    extraOptions = ''
      blur-method = "dual_kawase";
      blur-strength = 8;
      corner-radius = 8;
      round-borders = 1;
        
      rounded-corners-exclude = [
        "class_g = 'Polybar'",
      ];
    '';
    fade = true;
    fadeDelta = 5;
    package = pkgs.picom.overrideAttrs (
      o: {
        src = pkgs.fetchFromGitHub {
          repo = "picom";
          owner = "ibhagwan";
          rev = "44b4970f70d6b23759a61a2b94d9bfb4351b41b1";
          sha256 = "0iff4bwpc00xbjad0m000midslgx12aihs33mdvfckr75r114ylh";
        };
      }
    );
  };

  services.xidlehook = {
    enable = true;
    # NOTE: The delays add onto the previous value (and the value is in seconds)
    timers = [
      {
        delay = 60;
        command = "${pkgs.notify-desktop}/bin/notify-desktop --app-name=i3lock-fancy --urgency=critical \"Locking screen in 1 min\"";
        canceller = "${pkgs.notify-desktop}/bin/notify-desktop --app-name=i3lock-fancy \"Locking screen cancelled\"";
      }
      {
        delay = 60;
        command = "${pkgs.i3lock-fancy}/bin/i3lock-fancy";
      }
      {
        delay = 420;
        command = "${pkgs.notify-desktop}/bin/notify-desktop --app-name=systemctl --urgency=critical \"Sleeping in 1 min\"";
        canceller = "${pkgs.notify-desktop}/bin/notify-desktop --app-name=systemctl \"Sleep cancelled\"";
      }
      {
        delay = 60;
        command = "systemctl -i suspend";
      }
    ];
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
