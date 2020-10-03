{colors}:
  rec {
    "bar/main" = {
      monitor = "HDMI-1";
      height = 20;
      width = "98%";

      border-size = 2;
      border-color = colors.bg-primary;
      background = colors.bg-primary;
      foreground = colors.fg-secondary;

      modules-left = "ewmh separator separator separator browsermediacontrol";
      modules-center = "title";
      modules-right = "date";

      tray-position = "right";
      tray-padding = 2 ;
      tray-transparent = true;
      tray-background = colors.bg-primary;
      tray-maxsize = 10;

      font-0 = "Hack Nerd Font:size=7;2";
      font-1 = "Hack Nerd Font:size=13;2";
    };

    "bar/powermenu" = {
      monitor = "HDMI-1";
      height = 20;
      width = "2%";
      offset-x = "98%";

      border-size = 2;
      border-color = colors.alert;
      background = colors.alert;
      foreground = colors.fg-secondary;

      font-0 = "Hack Nerd Font:size=7;2";
      font-1 = "Hack Nerd Font:size=13;2";

      modules-center = "powermenu";
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

      label = " %title% ";
      label-maxlen = 100;
    };

    "module/browsermediacontrol" = {
      type = "custom/script";
      exec = "custom-browsermediacontrol";

      format = "<label>";
      format-background = colors.bg-primary;
      format-foreground = colors.fg-primary;

      border-size = 2;
      border-color = colors.fg-primary;

      scroll-up = "custom-browsermediacontrol --volume 1 &";
      scroll-down = "custom-browsermediacontrol --volume -1 &";

      interval = "0.1";
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
      click-left = "rofi -modi 'Powermenu:custom-script-sysmenu' -show Powermenu -theme sysmenu -location 3 -yoffset 25 &";
    };

    "module/separator" = {
      type = "custom/text";

      content = "|";
      content-background = colors.bg-primary;
      content-foreground = colors.bg-primary;
    };
  }
