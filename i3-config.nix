rec {
  fonts = ["Hack Nerd Font 12"];
  modifier = "Mod1";
  startup = [
    {
      command = "systemctl --user restart polybar";
      always = true;
      notification = false;
    }
  ];
  gaps = {
    outer = 5;
    inner = 5;

    smartGaps = true;
    smartBorders = "no_gaps";
  };
  bars = [ ];

  keybindings = let mod = modifier; in {
    # Programs executions
    "${mod}+Return" = "exec alacritty";
    "${mod}+space" = "exec rofi -show drun";
    "${mod}+Shift+c" = "exec i3lock-fancy -p -t \"\"";
    "${mod}+w" = "exec google-chrome-stable";

    # i3 level ops
    "${mod}+Shift+e" = "exec i3-msg exit";
    "${mod}+Shift+r" = "restart";

    # window operations
    "${mod}+q" = "kill";
    "${mod}+m" = "fullscreen";

    # Vim like navigation
    "${mod}+h" = "focus left";
    "${mod}+j" = "focus down";
    "${mod}+k" = "focus up";
    "${mod}+l" = "focus right";

    # Vim like window movement
    "${mod}+Shift+h" = "move left";
    "${mod}+Shift+j" = "move down";
    "${mod}+Shift+k" = "move up";
    "${mod}+Shift+l" = "move right";

    # Workspaces
    "${mod}+a" = "workspace 1";
    "${mod}+s" = "workspace 2";
    "${mod}+d" = "workspace 3";
    "${mod}+f" = "workspace 4";

    # Move stuff between workspaces
    "${mod}+Shift+a" = "move container to workspace 1";
    "${mod}+Shift+s" = "move container to workspace 2";
    "${mod}+Shift+d" = "move container to workspace 3";
    "${mod}+Shift+f" = "move container to workspace 4";

    # Spliting
    "${mod}+c" = "split h";
    "${mod}+v" = "split v";
  };
}
