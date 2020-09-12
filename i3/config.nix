{ colorscheme }:
  rec {
    fonts = ["Hack Nerd Font 12"];
    modifier = "Mod1";
    startup = [
      {
        command = "custom-i3-polybar-launch";
        always = true;
        notification = false;
      }
    ];
    gaps = {
      outer = 10;
      inner = 10;

      smartGaps = false;
      smartBorders = "no_gaps";
    };
    bars = [ ];

    keybindings = let mod = modifier; in {
        # Programs executions
        "${mod}+Return" = "exec alacritty --working-directory=`xcwd`";
        "${mod}+space" = "exec rofi -show drun";
        "${mod}+w" = "exec google-chrome-stable";

        # Keyboard Volument controls
        "XF86AudioRaiseVolume" = "exec amixer sset Master 5%+";
        "XF86AudioLowerVolume" = "exec amixer sset Master 5%-";
        "XF86AudioMute" = "exec amixer sset Master toggle";

        # i3 level ops
        "${mod}+Shift+e" = "exec i3-msg exit";
        "${mod}+Shift+r" = "restart";
        "${mod}+Shift+q" = "exec i3lock-fancy -p -t \"\"";
        "${mod}+r" = "mode \"resize\"";

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

    modes = {
      resize = {
        "h" = "resize shrink width 10 px or 10 ppt";
        "j" = "resize grow height 10 px or 10 ppt";
        "k" = "resize shrink height 10 px or 10 ppt";
        "l" = "resize grow width 10 px or 10 ppt";
        "Escape" = "mode default";
        "Return" = "mode default";
      };
    };

    window = {
      border = 4;
    };
    
    colors = {
      background = colorscheme.bg-primary;
      focused = {
        background = colorscheme.accent-primary;
        text = colorscheme.fg-primary;
        # looks like this is brighter accent
        indicator = colorscheme.accent-primary-bright;
        # looks like this is dimmer accent. Maybe I can right a function that
        # computes these
        border = "#4c7899";
        childBorder = colorscheme.accent-primary;
      };
    };
  }
