{ config, pkgs, lib, colorscheme, ... }:
let
  custom-browsermediacontrol =
    (import ../custom-programs/browser-media-control/default.nix) { inherit pkgs; };
  custom-weather-cli =
    (import ../custom-programs/weather-cli/default.nix) { inherit pkgs; };
in
{
  home.packages = with pkgs; [
    # Busybox replacements: As the default ones give out very
    # limited info which is extremely unhelpful when debugging
    # something
    less
    pciutils
    procps
    psmisc
    stress
    usbutils

    # Custom scripts
    custom-browsermediacontrol
    custom-weather-cli

    # File browser
    ranger

    # Image viewer
    feh

    # Info
    glxinfo
    radeontop

    # Openvpn interop
    gnome3.networkmanager-openvpn

    # Screenshot utility
    scrot

    # Sound control panel
    pavucontrol

    # System tray (Kind of a hack atm)
    # Need polybar to support this as a first class module
    gnome3.nautilus
    networkmanagerapplet
    nm-tray
    pasystray
    psensor

    # Utility to open present directory (Only use it with xmonad to open
    # terminal in same directory)
    xcwd
  ];
}

