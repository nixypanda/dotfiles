{ pkgs, colorscheme, ... }:
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

    eww close topbar-btw &
    custom-taffybar &
    eww open topbar-btw
  '';
  custom-taffybar =
    (import ../../custom-programs/taffybar/default.nix) { inherit pkgs; };
in {
  home.packages = with pkgs; [
    custom-panel-launch
    custom-taffybar
    haskellPackages.status-notifier-item
  ];

  home.file.".config/taffybar/taffybar.css".source =
    ../../custom-programs/taffybar/taffybar.css;
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
}
