{ config, pkgs, lib, colorscheme, ... }:
let
  custom-panel-launch = pkgs.writeScriptBin "custom-panel-launch" ''
    #!/${pkgs.stdenv.shell}

    killall custom-taffybar
    kill $(pidof pasystray)
    killall nm-applet

    eww daemon &
    deadd-notification-center &

    nm-applet &
    nm-tray &
    solaar -w hide &
    blueman-applet &
    pasystray &
    status-notifier-watcher &

    custom-taffybar &
    eww close topbar-btw && eww open topbar-btw
  '';

  custom-script-eww-sysinfo = pkgs.writeScriptBin "custom-script-eww-sysinfo" ''
    #!/${pkgs.stdenv.shell}
    ${builtins.readFile ./eww/scripts/custom-eww-sysinfo.sh}
  '';

  custom-browsermediacontrol =
    (import ./browser-media-control/default.nix) { pkgs = pkgs; };
  custom-weather-cli =
    (import ./weather-cli/default.nix) { pkgs = pkgs; };
  custom-taffybar =
    (import ./taffybar/default.nix) { pkgs = pkgs; };
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
    whitesur-icon-theme
    palenight-theme

    # system tray (Kind of a hack atm)
    # Need polybar to support this as a first class module
    networkmanagerapplet
    psensor
    gnome3.nautilus
    pasystray
    nm-tray

    # custom scripts
    custom-panel-launch
    custom-browsermediacontrol
    custom-script-eww-sysinfo
    custom-weather-cli
    custom-taffybar
    haskellPackages.status-notifier-item

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
    # Sound control panel
    pavucontrol
    # openvpn interop
    gnome3.networkmanager-openvpn

    # info
    glxinfo
    radeontop

    # Widgets
    eww

    # Notifications
    deadd-notification-center
    notify-desktop

    # Busybox replacements: As the default ones give out very
    # limited info which is extremely unhelpful when debugging
    # something
    pciutils
    usbutils
    less
    stress
    procps
    psmisc
  ];

  home.file.".config/google-chrome/NativeMessagingHosts".source = pkgs.symlinkJoin {
    name = "native-messaging-hosts";
    paths = [
      "${pkgs.plasma-browser-integration}/etc/var/empty/chrome/native-messaging-hosts"
    ];
  };
  home.file.".config/chromium/NativeMessagingHosts".source = pkgs.symlinkJoin {
    name = "native-messaging-hosts";
    paths = [
      "${pkgs.plasma-browser-integration}/etc/chromium/native-messaging-hosts"
    ];
  };

  home.file.".config/taffybar/taffybar.css".source = ./taffybar/taffybar.css;
  home.file.".config/taffybar/colors.css".text = ''
    @define-color font-color ${colorscheme.fg-primary};
    @define-color accent ${colorscheme.accent-primary};
    @define-color bg ${colorscheme.bg-primary};
    @define-color bg-alt ${colorscheme.bright-black};
    @define-color red ${colorscheme.red};
    @define-color menu-background-color @bg;
    @define-color menu-background-color-selected @bg-alt;
    @define-color menu-font-color @font-color;
  '';

  programs.rofi = {
    enable = true;
    package = pkgs.rofi.override {
      plugins = [ pkgs.rofi-emoji pkgs.rofi-calc pkgs.rofi-file-browser ];
    };
    font = "hack 10";
  };
  home.file.".config/rofi/colors.rasi".text = ''
    * {
      accent: ${colorscheme.accent-primary};
      accent-secondary: ${colorscheme.accent-secondary};
      background: ${colorscheme.bg-primary};
      foreground: ${colorscheme.fg-primary};
    }
  '';
  home.file.".config/rofi/grid.rasi".source = ./rofi/grid.rasi;
  home.file.".config/rofi/launcher.rasi".source = ./rofi/launcher.rasi;

  home.file.".config/eww/eww.scss".source = ./eww/eww.scss;
  home.file.".config/eww/eww.yuck".source = ./eww/eww.yuck;

  # systray stuff
  home.file.".config/volumeicon/volumeicon".source = ./systray/volumeicon.cfg;

  gtk = {
    enable = true;
    font = { name = "TeX Gyre Heros 10"; };
    iconTheme = { name = colorscheme.gtk-icon-name; };
    theme = { name = colorscheme.gtk-name; };
  };

  services.random-background = {
    enable = true;
    imageDirectory = "%h/Pictures/backgrounds/${colorscheme.name}";
  };
  home.file.".config/deadd/deadd.css".source = colorscheme.deadd-css-file;

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
        delay = 240;
        command = "${pkgs.notify-desktop}/bin/notify-desktop --app-name=i3lock-fancy --urgency=critical \"Locking screen in 1 min\"";
        canceller = "${pkgs.notify-desktop}/bin/notify-desktop --app-name=i3lock-fancy \"Locking screen cancelled\"";
      }
      {
        delay = 60;
        command = "${pkgs.i3lock-fancy}/bin/i3lock-fancy";
      }
      {
        delay = 240 + 1200;
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
      extraPackages = haskellPackages: with haskellPackages; [
        taffybar
      ];
      config = pkgs.writeText "xmonad.hs" ''
        ${builtins.readFile ./xmonad/config.hs}

        myFocusedBorderColor = "${colorscheme.accent-primary}"
        myNormalBorderColor = "${colorscheme.bg-primary-bright}"
      '';
    };

    # So eww daemon starts in the parent shell, It executes the scripts
    # provided in that shell environment only. That environment does not have
    # access to the session variables that I declare in zsh.
    # TODO: Figure out the exact sequence of evnets that happen here
    # To provide the required session variables to eww daemon, importing the
    # variables here.
    profileExtra = ''
      export DOTFILES=~/.dotfiles
      ${builtins.readFile ../../../.secrets/env-vars.sh}
    '';
  };
}

