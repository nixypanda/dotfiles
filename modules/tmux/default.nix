{ pkgs, ... }:
{
  programs.tmux = {
    terminal = "tmux-256color";
    enable = true;
    baseIndex = 1;
    clock24 = true;
    keyMode = "vi";
    secureSocket = false;
    shortcut = "a";
    customPaneNavigationAndResize = true;
    escapeTime = 0;
    historyLimit = 30000;
    shell = "${pkgs.nushell}/bin/nu";
    extraConfig = ''
      # Default termtype. If the rcfile sets $TERM, that overrides this value.
      set -g terminal-overrides ',xterm-256color:Tc'

      # Move status bar to top
      set -g status-position top

      # Create splits and vertical splits
      bind-key v split-window -h -p 50 -c "#{pane_current_path}"
      bind-key s split-window -p 50 -c "#{pane_current_path}"

      # popups
      bind p display-popup -d "#{pane_current_path}" -w 80% -h 80% -E "nu"

      # Also use mouse
      set -g mouse on
    '';

    plugins = with pkgs.tmuxPlugins; [
      vim-tmux-navigator
      # Not really maintained so tokyo-night-tmux is the only one
      # onedark-theme
      {
        plugin = tokyo-night-tmux;
        extraConfig = ''
          # auto theming
          set-hook -g client-dark-theme '
            set -g @tokyo-night-tmux_theme night;
            run-shell ${pkgs.tmuxPlugins.tokyo-night-tmux}/share/tmux-plugins/tokyo-night-tmux/tokyo-night.tmux
          '
          set-hook -g client-light-theme '
            set -g @tokyo-night-tmux_theme day;
            run-shell ${pkgs.tmuxPlugins.tokyo-night-tmux}/share/tmux-plugins/tokyo-night-tmux/tokyo-night.tmux
          '
        '';
      }
      {
        plugin = tmux-which-key;
        extraConfig = ''
          set -g @which_key_delay 300
          set -g @which_key_max_depth 2
          bind ? run-shell "tmux-which-key show"
        '';
      }
    ];
  };
}
