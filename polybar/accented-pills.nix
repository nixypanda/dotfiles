{colors}:
let
  separator = color: {
      type = "custom/text";

      content = " ";
      content-background = color;
      content-foreground = color;
  };
  filled-half-circle = direction: color: {
    content = if direction == "left" then "%{T2}%{T-}" else "%{T2}%{T-}";
    type = "custom/text";	
    content-foreground = color;	
  };
  secondary-bar-width-pct = 7.6;
in
  rec {
    "bar/base" = {
      monitor = "HDMI-1";
      height = 20;

      border-size = 2;
      border-left-size = 0;
      border-right-size = 0;
      border-color = colors.bg-primary-transparent-argb;
      background = colors.bg-primary-transparent-argb;
      foreground = colors.fg-secondary;

      font-0 = "Hack Nerd Font:size=7;2";
      font-1 = "Hack Nerd Font:size=13;3";
      font-2 = "Hack Nerd Font:size=13;3";
    };

    "bar/main" = {
      "inherit" = "bar/base";

      width = "${builtins.toString(100 - secondary-bar-width-pct)}%";

      modules-left = "ewmh right separator separator separator saparator separator separator separator left-bmc browsermediacontrol right-bmc";
      modules-center = "left title right";
      modules-right = "left";

      tray-position = "right";
      tray-padding = 7 ;
      tray-transparent = true;
      tray-background = colors.bg-primary;
      tray-maxsize = 12;
    };

    "bar/powermenu" = {
      "inherit" = "bar/base";

      width = "${builtins.toString(secondary-bar-width-pct)}%";
      offset-x = "${builtins.toString(100 - secondary-bar-width-pct)}%";

      modules-left = "right";
      modules-right = "left date separator-bg separator-red powermenu separator-red right-red";
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
      icon-4 = "u;u";
      icon-5 = "i;i";
      icon-6 = "o;o";
      icon-7 = "p;p";
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
      format-background = colors.bg-secondary-transparent-argb;
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
      time-alt = "%d-%m-%y";

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

    "module/separator" = separator colors.bg-primary-transparent-argb;
    "module/separator-red" = separator colors.alert;
    "module/separator-bg" = separator colors.bg-primary;
    "module/left" = filled-half-circle "left" colors.bg-primary;	
    "module/right" = filled-half-circle "right" colors.bg-primary;	
    "module/left-red" = filled-half-circle "left" colors.alert;	
    "module/right-red" = filled-half-circle "right" colors.alert;	
    # NOTE: Not Working
    "module/left-bmc" = filled-half-circle "left" colors.bg-secondary-transparent-argb;	
    "module/right-bmc" = filled-half-circle "right" colors.bg-secondary-transparent-argb;	
  }
