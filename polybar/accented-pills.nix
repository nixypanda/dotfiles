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

      modules-left = "ewmh separator browsermediacontrol_play_i browsermediacontrol browsermediacontrol_next_i seprator";
      modules-center = "title";
      modules-right = "separator backlight_i backlight separator alsa_i alsa separator network_i network separator date_i date separator powermenu";

      font-0 = "Hack Nerd Font:size=9;2";
    };

    "module/ewmh" = {
      type = "internal/xworkspaces";

      pin-workspaces = false;
      enable-click = true;
      enable-scroll = false;

      icon-0 = "a;";
      icon-1 = "s;";
      icon-2 = "d;";
      icon-3 = "f;";
      icon-default = "";

      format = "<label-state>";

      label-active = "%icon%";
      label-active-foreground = colors.fg-secondary;
      label-active-underline =  colors.accent-primary;
      label-active-background = colors.accent-primary;
      label-active-padding = 1;

      label-occupied = "%icon%";
      label-occupied-foreground = colors.fg-secondary;
      label-occupied-background = colors.bg-secondary;
      label-occupied-underline = colors.bg-secondary;
      label-occupied-padding = 1;

      label-urgent = "%icon%";
      label-urgent-foreground = colors.fg-secondary;
      label-urgent-underline =  colors.accent-primary;
      label-urgent-background = colors.alert;
      label-urgent-padding = 1;

      label-empty = "%icon%";
      label-empty-foreground = colors.fg-primary;
      label-empty-background = colors.bg-primary-bright;
      label-empty-underline = colors.bg-secondary;
      label-empty-padding = 1;
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

    "module/backlight" = {
      type = "custom/script";
      exec = "custom-script-backlight --current";
      format = "<label>";

      format-padding = 1;
      format-background = colors.bg-secondary;
      format-foreground = colors.fg-secondary;

      scroll-up = "custom-script-backlight --increase";
      scroll-down = "custom-script-backlight --decrease";
    };

    "module/backlight_i" = {
      type = "custom/text";

      content = "";
      content-padding = 1;
      content-background = colors.accent-primary;
      content-foreground = colors.fg-secondary;
    };

    "module/browsermediacontrol_play_i" = {
      type = "custom/script";
      exec = "custom-browsermediacontrol --display=play/pause";

      format = "<label>";
      format-padding = 1;
      format-background = colors.accent-secondary;
      format-foreground = colors.fg-secondary;

      interval = "0.1";
    };


    "module/browsermediacontrol" = {
      type = "custom/script";
      exec = "custom-browsermediacontrol --display=title";

      format = "<label>";
      format-padding = 1;
      format-background = colors.bg-secondary;
      format-foreground = colors.fg-secondary;

      scroll-up = "custom-browsermediacontrol --volume 1";
      scroll-down = "custom-browsermediacontrol --volume -1";

      interval = "0.1";
    };

    "module/browsermediacontrol_next_i" = {
      type = "custom/script";
      exec = "custom-browsermediacontrol --display=next";

      format = "<label>";
      format-padding = 1;
      format-background = colors.accent-secondary;
      format-foreground = colors.fg-secondary;

      interval = "0.1";
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
