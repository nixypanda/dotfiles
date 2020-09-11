{colors}:
  rec {
    "bar/top" = {
      monitor = "HDMI-1";
      height = "1.5%";

      background = colors.bg-primary;
      foreground = colors.fg-secondary;

      padding = 1;
      border-size = 4;
      border-color = colors.bg-primary;

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
      type = "custom/text";
      content = "";
      content-padding = 2;
      content-background = colors.alert;
      content-foreground = colors.fg-secondary;
      click-left = "custom-script-sysmenu";
    };

    "module/separator" = {
      type = "custom/text";

      content = "|";
      content-background = colors.bg-primary;
      content-foreground = colors.bg-primary;
      content-padding = "0.5";
    };
  }
