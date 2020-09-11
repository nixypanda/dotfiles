{colors}:
  rec {
    "bar/top" = {
      monitor = "HDMI-1";
      width = "100%";
      height = "1.5%";
      radius = 0;

      wm-restack = "i3";

      background = colors.bg-primary;
      foreground = colors.fg-secondary;

      overline-size = 2;
      overline-color = colors.accent-primary;

      border-size = 4;
      border-color = colors.bg-primary;

      padding = 1;
      modules-left = "i3 separator";
      modules-center = "title";
      modules-right = "separator alsa_i alsa separator network_i network separator date_i date separator powermenu";

      font-0 = "Hack Nerd Font:size=9;2";
    };

    "module/i3" = {
      type = "internal/i3";


      pin-workspaces = true;
      strip-wsnumbers = true;
      index-sort = true;

      enable-click = true;
      enable-scroll = true;

      wrapping-scroll = false;
      reverse-scroll = false;

      format = "<label-state> <label-mode>";
      label-focused = "%icon%";
      label-focused-foreground = colors.fg-secondary;
      label-focused-background = colors.accent-primary;
      label-focused-underline = colors.accent-primary;
      label-focused-padding = 1;

      label-unfocused = "%icon%";
      label-unfocused-foreground = colors.fg-secondary;
      label-unfocused-background = colors.bg-secondary;
      label-unfocused-underline = colors.bg-secondary;
      label-unfocused-padding = 1;

      label-visible = "%index%";
      label-visible-underline = "#555555";
      label-visible-padding = 1;

      label-urgent = "%index%";
      label-urgent-foreground = colors.fg-secondary;
      label-urgent-background = colors.alert;
      label-urgent-padding = 1;

      ws-icon-default = "";
      ws-icon-0 = "1;";
      ws-icon-1 = "2;";
      ws-icon-2 = "3;";
      ws-icon-3 = "4;";
    };

    "module/title" = {
      type = "internal/xwindow";

      format = "<label>";
      format-foreground = colors.fg-primary;

      label = "%title%";
      label-maxlen = 100;
    };

    "module/alsa" = {
      type = "internal/alsa";

      format-volume = "<label-volume>";
      format-volume-background = colors.bg-secondary;
      format-volume-foreground = colors.fg-secondary;
      format-volume-padding = 1;

      label-volume = "%percentage%%";

      format-muted-background = colors.bg-secondary;
      format-muted-foreground = colors.alert;
      format-muted-padding = 1;

      label-muted = "Muted";
      label-muted-foreground = colors.alert;
    };

    "module/alsa_i" = {
      type = "internal/alsa";

      format-volume = "<ramp-volume>";
      format-volume-background = colors.accent-secondary;
      format-volume-foreground = colors.fg-secondary;
      format-volume-padding = 1;

      format-muted-background = colors.accent-secondary;
      format-muted-foreground = colors.alert;
      format-muted-padding = 1;

      label-muted = "";
      label-muted-foreground = colors.fg-secondary;

      ramp-volume-0 = "";
      ramp-volume-1 = "";

      ramp-headphones-0 = "";
      ramp-headphones-1 = "";
    };

    "module/date" = {
      type = "internal/date";

      interval = "1.0";

      time = "%I:%M %p";
      time-alt = "%Y-%m-%d%";

      format = "<label>";
      format-background = colors.bg-secondary;
      format-foreground = colors.fg-secondary;
      format-padding = 1;
       
      label = "%time%";
    };

    "module/date_i" = {
      type = "internal/date";

      interval = 1;

      time = "";
      time-alt = "";

      format = "<label>";
      format-background = colors.warning;
      format-foreground = colors.fg-secondary;
      format-padding = 1;
       
      label = "%time%";
    };

    "module/network" = {
      type = "internal/network";

      interface = "wlp4s0";
      interval = "1.0";

      accumulate-stats = true;
      unknown-as-up = true;

      format-connected = "<label-connected>";
      format-connected-background = colors.bg-secondary;
      format-connected-foreground = colors.fg-secondary;
      format-connected-padding = 1;
       
      format-disconnected = "<label-disconnected>";
      format-disconnected-background = colors.bg-secondary;
      format-disconnected-foreground = colors.fg-secondary;
      format-disconnected-padding = 1;

      label-connected = "%essid%";
      label-disconnected = "Disconnected";
    };

    "module/network_i" = {
      type = "internal/network";

      interface = "wlp4s0";
      interval = "1.0";

      accumulate-stats = true;
      unknown-as-up = true;

      format-connected = "<ramp-signal>";
      format-connected-background = colors.accent-tertiary;
      format-connected-foreground = colors.fg-secondary;
      format-connected-padding = 1;
       
      format-disconnected = "<label-disconnected>";
      format-disconnected-background = colors.accent-tertiary;
      format-disconnected-foreground = colors.fg-secondary;
      format-disconnected-padding = 1;

      label-disconnected = "";
      ramp-signal-0 = "";
      ramp-signal-1 = "";
    };

    "module/powermenu" = {
      type = "custom/menu";
      expand-right = true;
      format-spacing = 0;

      label-open = "";
      label-open-foreground = colors.fg-secondary;
      label-open-background = colors.alert;
      label-open-padding = 1;

      label-close = "x";
      label-close-background = colors.accent-primary;
      label-close-foreground = colors.fg-secondary;
      label-close-padding = 1;

      label-separator = "";
      label-separator-background = colors.bg-secondary;
      label-separator-foreground = colors.bg-secondary;

      menu-0-0 = "";
      menu-0-0-background = colors.bg-secondary;
      menu-0-0-padding = 1;
      menu-0-0-exec = "systemctl reboot";

      menu-0-1 = "";
      menu-0-1-background = colors.bg-secondary;
      menu-0-1-padding = 1;
      menu-0-1-exec = "systemctl poweroff";
    };

    "module/separator" = {
      type = "custom/text";

      content = "|";
      content-background = colors.bg-primary;
      content-foreground = colors.bg-primary;
      content-padding = "0.5";
    };
  }
