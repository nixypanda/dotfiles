{colors}:
  rec {
    "bar/main" = {
      monitor = "HDMI-1";
      height = 20;

      background = colors.bg-primary-transparent-argb;
      foreground = colors.fg-secondary;

      modules-left = "ewmh right separator browsermediacontrol";
      modules-center = "left title right";
      # NOTE: Need a system-tray here
      modules-right = "left network separator_fg volume separator_fg date right separator left-red powermenu right-red";

      border-size = 2;
      border-color = colors.bg-primary-transparent-argb;

      tray-position = "right";
      tray-padding = 2 ;
      tray-transparent = true;
      tray-background = "#0063ff";

      font-0 = "Hack Nerd Font:size=7;2";
      font-1 = "Hack Nerd Font:size=13;2";
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
      label-active-underline =  colors.bg-primary-transparent-argb;
      label-active-background = colors.accent-primary;
      label-active-padding = 2;

      label-occupied = "%icon%";
      label-occupied-foreground = colors.fg-secondary;
      label-occupied-background = colors.bg-secondary;
      label-occupied-underline = colors.bg-primary-transparent-argb;
      label-occupied-padding = 2;

      label-urgent = "%icon%";
      label-urgent-foreground = colors.fg-secondary;
      label-urgent-underline =  colors.bg-primary-transparent-argb;
      label-urgent-background = colors.alert;
      label-urgent-padding = 2;

      label-empty = "%icon%";
      label-empty-foreground = colors.fg-primary;
      label-empty-background = colors.bg-primary;
      label-empty-underline = colors.bg-primary-transparent-argb;
      label-empty-padding = 2;
    };

    "module/title" = {
      type = "internal/xwindow";

      format = "<label>";
      format-foreground = colors.fg-primary;
      format-background = colors.bg-primary;
      format-underline = colors.bg-primary-transparent-argb;
      # format-padding = 1;

      label = " %title% ";
      label-maxlen = 100;
    };

    "module/browsermediacontrol" = {
      type = "custom/script";
      exec = "custom-browsermediacontrol";

      format = "<label>";
      # format-padding = 1;
      format-background = colors.bg-primary-transparent-argb;
      format-foreground = colors.fg-primary;

      border-size = 2;
      border-color = colors.fg-primary;

      scroll-up = "custom-browsermediacontrol --volume 1 &";
      scroll-down = "custom-browsermediacontrol --volume -1 &";

      interval = "0.1";
    };

    "module/network" = {
      type = "internal/network";

      interface = "wlp4s0";
      interval = "1.0";

      accumulate-stats = true;
      unknown-as-up = true;

      format-connected = "<ramp-signal>";
      format-connected-background = colors.bg-primary;
      format-connected-foreground = colors.fg-primary;
      format-connected-padding = 1;

      format-disconnected = "<label-disconnected>";
      format-disconnected-background = colors.bg-primary;
      format-disconnected-foreground = colors.alert;
      format-disconnected-padding = 1;

      label-disconnected = "";
      ramp-signal-0 = "";
      ramp-signal-1 = "";
    };

    "module/volume" = {
      type = "internal/alsa";

      format-volume = "<ramp-volume>";
      format-volume-background = colors.bg-primary;
      format-volume-foreground = colors.fg-primary;
      format-volume-padding = 1;

      format-muted-background = colors.bg-primary;
      format-muted-foreground = colors.alert;
      format-muted-padding = 1;

      label-muted = "";
      label-muted-foreground = colors.alert;

      ramp-volume-0 = "";
      ramp-volume-1 = "";
    };

    "module/date" = {
      type = "internal/date";

      interval = "1.0";

      time = "%I:%M %p";

      format = " <label>";
      format-background = colors.bg-primary;
      format-foreground = colors.fg-primary;
      format-padding = 1;

      label = "%time%";
    };

    "module/powermenu" = {
      type = "custom/text";
      content = "";
      content-padding = 1;
      content-background = colors.alert;
      content-foreground = colors.fg-secondary;
      content-underline = colors.bg-primary-transparent-argb;
      click-left = "custom-script-sysmenu &";
    };

    "module/separator" = {
      type = "custom/text";

      content = "|";
      content-background = colors.bg-primary-transparent-argb;
      content-foreground = colors.bg-primary-transparent-argb;
    };

    "module/separator_fg" = {
      type = "custom/text";

      content = "|";
      content-background = colors.bg-primary;
      content-foreground = colors.bg-primary;
    };

    "module/left" ={
      type = "custom/text";

      content = "%{T2}%{T-}";
      content-foreground = colors.bg-primary;
    };
    "module/right" ={
      type = "custom/text";

      content = "%{T2}%{T-}";
      content-foreground = colors.bg-primary;
    };
    "module/left-red" ={
      type = "custom/text";

      content = "%{T2}%{T-}";
      content-foreground = colors.alert;
    };
    "module/right-red" ={
      type = "custom/text";

      content = "%{T2}%{T-}";
      content-foreground = colors.alert;
    };
  }
