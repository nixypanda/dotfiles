{ pkgs, ... }: {
  programs.tmux = {
    terminal = "screen-256color";
    enable = true;
    baseIndex = 1;
    clock24 = true;
    keyMode = "vi";
    secureSocket = false;
    shortcut = "a";
    customPaneNavigationAndResize = true;
    escapeTime = 0;
    historyLimit = 30000;
    extraConfig = ''
      # Default termtype. If the rcfile sets $TERM, that overrides this value.
      set -g terminal-overrides ',xterm-256color:Tc'
      # Create splits and vertical splits
      bind-key v split-window -h -p 50 -c "#{pane_current_path}"
      bind-key s split-window -p 50 -c "#{pane_current_path}"
      # Also use mouse
      setw -g mouse on
    '';

    plugins = with pkgs.tmuxPlugins; [
      vim-tmux-navigator
      # onedark-theme
      catppuccin
    ];
  };
}
