{ colors }:
let
  background = colors.bg-primary-bright-transparent-argb;

  module-background = colors.bg-primary;
  module-foreground = colors.fg-primary;
  module-background-alt = colors.bg-primary-bright;

  accent = colors.accent-primary;
  attention = colors.alert;

  separator = { color, size ? 1 }:
    {
      type = "custom/text";
      # NOTE: How to do? `take n . repeat`
      content = if size == 1 then " " else "       ";
      content-background = color;
      content-foreground = color;
    };

  filled-half-circle = { direction, color-fg ? module-background, color-bg ? background }: {
    content = if direction == "left" then "%{T2}%{T-}" else "%{T2}%{T-}";
    type = "custom/text";
    content-foreground = color-fg;
    content-background = color-bg;
  };

  secondary-bar-width-pct = 7.6;
in
rec {
  "bar/base" = {
    monitor = "DisplayPort-0";
    height = 27;

    border-size = 2;
    border-left-size = 0;
    border-right-size = 0;
    border-color = background;
    background = background;

    font-0 = "Hack Nerd Font:size=7;2";
    font-1 = "Hack Nerd Font:size=18;4";
  };

  "bar/main" = {
    "inherit" = "bar/base";

    width = "${builtins.toString (100 - secondary-bar-width-pct)}%";

    modules-left = "ewmh right-bg-bgb sep-bg-b layout-xmonad right-bg-b sep-huge left bmc right";
    modules-center = "left title right";
    modules-right = "left";

    tray-position = "right";
    tray-padding = 7;
    tray-background = module-background;
    tray-maxsize = 12;
  };

  "bar/powermenu" = {
    "inherit" = "bar/base";

    width = "${builtins.toString (secondary-bar-width-pct)}%";
    offset-x = "${builtins.toString (100 - secondary-bar-width-pct)}%";

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
    label-active-foreground = background;
    label-active-background = accent;
    label-active-padding = 2;

    label-occupied = "%icon%";
    label-occupied-foreground = module-foreground;
    label-occupied-background = module-background-alt;
    label-occupied-padding = 2;

    label-urgent = "%icon%";
    label-urgent-foreground = module-foreground;
    label-urgent-background = attention;
    label-urgent-padding = 2;

    label-empty = "%icon%";
    label-empty-foreground = module-foreground;
    label-empty-background = module-background;
    label-empty-padding = 2;
  };

  "module/layout-xmonad" = {
    type = "custom/script";
    exec = "tail -F \"$HOME/.xmonad/xmonad-layout\"";
    label = "%output%";
    tail = true;
    label-background = module-background-alt;
    label-foreground = module-foreground;
  };

  "module/title" = {
    type = "internal/xwindow";

    format = "<label>";
    format-foreground = module-foreground;
    format-background = module-background;

    label = " %title% ";
    label-maxlen = 100;
  };

  "module/bmc" = {
    type = "custom/script";
    exec = "custom-browsermediacontrol";

    format = "<label>";
    format-background = module-background;
    format-foreground = module-foreground;

    border-size = 2;
    border-color = module-foreground;

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
    format-background = module-background;
    format-foreground = module-foreground;
    format-padding = 1;

    label = "%time%";
  };

  "module/powermenu" = {
    type = "custom/text";
    content = "";
    content-padding = 1;
    content-background = attention;
    content-foreground = module-background;
    click-left = "rofi -modi 'Powermenu:custom-script-sysmenu' -show Powermenu -theme sysmenu -location 3 -yoffset 25 &";
  };

  "module/sep" = separator { color = background; };
  "module/sep-huge" = separator { color = background; size = 7; };
  "module/sep-red" = separator { color = attention; };
  "module/sep-bg" = separator { color = module-background; };
  "module/sep-bg-b" = separator { color = module-background-alt; };

  "module/right-bg-bgb" = filled-half-circle { direction = "right"; color-fg = module-background; color-bg = module-background-alt; };
  "module/left-bg-bgb" = filled-half-circle { direction = "left"; color-fg = module-background; color-bg = module-background-alt; };
  "module/left-bg-b" = filled-half-circle { direction = "left"; color-fg = module-background-alt; };
  "module/right-bg-b" = filled-half-circle { direction = "right"; color-fg = module-background-alt; };
  "module/left" = filled-half-circle { direction = "left"; };
  "module/right" = filled-half-circle { direction = "right"; };
  "module/left-red" = filled-half-circle { direction = "left"; color-fg = attention; };
  "module/right-red" = filled-half-circle { direction = "right"; color-fg = attention; };
}
