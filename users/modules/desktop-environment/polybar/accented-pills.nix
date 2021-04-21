{colors}:
let
  separator = {color, size ? 1}: 
    {
      type = "custom/text";
      # NOTE: How to do? `take n . repeat`
      content = if size == 1 then " " else "       ";
      content-background = color;
      content-foreground = color;
  };
  filled-half-circle = { direction, color-fg, color-bg ? colors.bg-primary-transparent-argb }: {
    content = if direction == "left" then "%{T2}%{T-}" else "%{T2}%{T-}";
    type = "custom/text";	
    content-foreground = color-fg;	
    content-background = color-bg;
  };
  secondary-bar-width-pct = 7.6;
in
  rec {
    "bar/base" = {
      monitor = "HDMI-1";
      height = 27;

      border-size = 2;
      border-left-size = 0;
      border-right-size = 0;
      border-color = colors.bg-primary-transparent-argb;
      background = colors.bg-primary-transparent-argb;

      font-0 = "Hack Nerd Font:size=7;2";
      font-1 = "Hack Nerd Font:size=18;4";
    };

    "bar/main" = {
      "inherit" = "bar/base";

      width = "${builtins.toString(100 - secondary-bar-width-pct)}%";

      modules-left = "ewmh right-bg-bgb sep-bg-b layout-xmonad right-bg-b sep-huge left bmc right";
      modules-center = "left title right";
      modules-right = "left";

      tray-position = "right";
      tray-padding = 7 ;
      tray-background = colors.bg-primary;
      tray-maxsize = 12;
    };

    "bar/powermenu" = {
      "inherit" = "bar/base";

      width = "${builtins.toString(secondary-bar-width-pct)}%";
      offset-x = "${builtins.toString(100 - secondary-bar-width-pct)}%";

      modules-left = "right";
      modules-right = "left date sep-bg sep-red powermenu sep-red right-red";
    };

    "module/ewmh" = {
      type = "internal/xworkspaces";

      pin-workspaces = false;
      enable-click = true;
      enable-scroll = false;

      icon-0 = "a;";
      icon-1 = "s;";
      icon-2 = "d;ﭮ";
      icon-3 = "f;";
      icon-4 = "u;u";
      icon-5 = "i;i";
      icon-6 = "o;o";
      icon-7 = "p;p";
      icon-default = "";

      format = "<label-state>";

      label-active = "%icon%";
      label-active-foreground = colors.bg-primary;
      label-active-background = colors.accent-primary;
      label-active-padding = 2;

      label-occupied = "%icon%";
      label-occupied-foreground = colors.fg-primary;
      label-occupied-background = colors.bg-primary-bright;
      label-occupied-padding = 2;

      label-urgent = "%icon%";
      label-urgent-foreground = colors.alert;
      label-urgent-background = colors.bg-primary;
      label-urgent-padding = 2;

      label-empty = "%icon%";
      label-empty-foreground = colors.fg-primary;
      label-empty-background = colors.bg-primary;
      label-empty-padding = 2;
    };

    "module/layout-xmonad" = {
      type = "custom/script";
      exec = "tail -F \"$HOME/.xmonad/xmonad-layout\"";
      label = "%output%";
      tail = true;
      label-background = colors.bg-primary-bright;
      label-foreground = colors.fg-primary;
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

    "module/bmc" = {
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
      time-alt = "%d-%m-%y";

      format = "  <label>";
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
      content-foreground = colors.bg-primary;
      content-underline = colors.bg-primary-transparent-argb;
      click-left = "rofi -modi 'Powermenu:custom-script-sysmenu' -show Powermenu -theme sysmenu -location 3 -yoffset 25 &";
    };

    "module/sep"       =  separator { color = colors.bg-primary-transparent-argb; };           
    "module/sep-huge"  =  separator { color = colors.bg-primary-transparent-argb; size = 7; };
    "module/sep-red"   =  separator { color = colors.alert;                       };        
    "module/sep-bg"    =  separator { color = colors.bg-primary;                  };        
    "module/sep-bg-b"  =  separator { color = colors.bg-primary-bright;           };        
    "module/sep-ap"    =  separator { color = colors.accent-primary;              };        
    "module/sep-as"    =  separator { color = colors.accent-secondary;            };        
    "module/sep-at"    =  separator { color = colors.accent-tertiary;             };        

    "module/right-bg-bgb" = filled-half-circle { direction = "right"; color-fg = colors.bg-primary; color-bg = colors.bg-primary-bright; };
    "module/left-ap"      = filled-half-circle { direction = "left" ; color-fg = colors.accent-primary; };
    "module/right-ap"     = filled-half-circle { direction = "right"; color-fg = colors.accent-primary; };
    "module/left-as"      = filled-half-circle { direction = "left" ; color-fg = colors.accent-secondary; };
    "module/right-as"     = filled-half-circle { direction = "right"; color-fg = colors.accent-secondary; };
    "module/left-at"      = filled-half-circle { direction = "left" ; color-fg = colors.accent-tertiary; };
    "module/right-at"     = filled-half-circle { direction = "right"; color-fg = colors.accent-tertiary; };
    "module/left-bg-b"    = filled-half-circle { direction = "left" ; color-fg = colors.bg-primary-bright; };
    "module/right-bg-b"   = filled-half-circle { direction = "right"; color-fg = colors.bg-primary-bright; };
    "module/left"         = filled-half-circle { direction = "left" ; color-fg = colors.bg-primary; };
    "module/right"        = filled-half-circle { direction = "right"; color-fg = colors.bg-primary; };
    "module/left-red"     = filled-half-circle { direction = "left" ; color-fg = colors.alert; };
    "module/right-red"    = filled-half-circle { direction = "right"; color-fg = colors.alert; };
  }
