
rec {
  
 "color" = {
    # Active Colors
    bg = "#2f343f";
    fg = "#1C1E20";
    fg-alt = "#C4C7C5";
    mf = "#C4C7C5";
    ac = "#B4BC67";

    # Bars;
    bn = "#308634";
    bm = "#E57C46";
    bd = "#E24C49";

    trans = "#00000000";
    white = "#FFFFFF";
    black = "#000000";

    # Colors;
    red = "#EC7875";
    pink = "#EC6798";
    purple = "#BE78D1";
    blue = "#75A4CD";
    cyan = "#00C7DF";
    teal = "#00B19F";
    green = "#61C766";
    lime = "#B9C244";
    yellow = "#EBD369";
    amber = "#EDB83F";
    orange = "#E57C46";
    brown = "#AC8476";
    grey = "#8C8C8C";
    indigo = "#6C77BB";
    blue-gray = "#6D8895";
  };

  "bar/top" = {
    monitor = "HDMI-1";
    width = "100%";
    height = "1.5%";
    radius = 0;

    wm-restack = "i3";

    background = color.bg;
    foreground = color.fg;

    overline-size = 2;
    overline-color = color.ac;

    border-size = 4;
    border-color = color.bg;

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
    label-focused-foreground = color.fg;
    label-focused-background = color.ac;
    label-focused-underline = color.ac;
    label-focused-padding = 1;

    label-unfocused = "%icon%";
    label-unfocused-foreground = color.fg;
    label-unfocused-background = color.mf;
    label-unfocused-underline = color.mf;
    label-unfocused-padding = 1;

    label-visible = "%index%";
    label-visible-underline = "#555555";
    label-visible-padding = 1;

    label-urgent = "%index%";
    label-urgent-foreground = color.fg;
    label-urgent-background = color.red;
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
    format-foreground = color.fg-alt;

    label = "%title%";
    label-maxlen = 100;
  };

  "module/alsa" = {
    type = "internal/alsa";

    format-volume = "<label-volume>";
    format-volume-background = color.mf;
    format-volume-foreground = color.fg;
    format-volume-padding = 1;

    label-volume = "%percentage%%";

    format-muted-background = color.mf;
    format-muted-foreground = color.red;
    format-muted-padding = 1;

    label-muted = "Muted";
    label-muted-foreground = color.red;
  };

  "module/alsa_i" = {
    type = "internal/alsa";

    format-volume = "<ramp-volume>";
    format-volume-background = color.blue;
    format-volume-foreground = color.fg;
    format-volume-padding = 1;

    format-muted-background = color.blue;
    format-muted-foreground = color.red;
    format-muted-padding = 1;

    label-muted = "";
    label-muted-foreground = color.fg;

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
    format-background = color.mf;
    format-foreground = color.fg;
    format-padding = 1;
     
    label = "%time%";
  };

  "module/date_i" = {
    type = "internal/date";

    interval = 1;

    time = "";
    time-alt = "";

    format = "<label>";
    format-background = color.amber;
    format-foreground = color.fg;
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
    format-connected-background = color.mf;
    format-connected-foreground = color.fg;
    format-connected-padding = 1;
     
    format-disconnected = "<label-disconnected>";
    format-disconnected-background = color.mf;
    format-disconnected-foreground = color.fg;
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
    format-connected-background = color.purple;
    format-connected-foreground = color.fg;
    format-connected-padding = 1;
     
    format-disconnected = "<label-disconnected>";
    format-disconnected-background = color.purple;
    format-disconnected-foreground = color.fg;
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
    label-open-foreground = color.mf;
    label-open-background = color.red;
    label-open-padding = 1;

    label-close = "x";
    label-close-background = color.green;
    label-close-foreground = color.fg;
    label-close-padding = 1;

    label-separator = "";
    label-separator-background = color.mf;
    label-separator-foreground = color.mf;

    menu-0-0 = "";
    menu-0-0-background = color.mf;
    menu-0-0-padding = 1;
    menu-0-0-exec = "systemctl reboot";

    menu-0-1 = "";
    menu-0-1-background = color.mf;
    menu-0-1-padding = 1;
    menu-0-1-exec = "systemctl poweroff";
  };

  "module/separator" = {
    type = "custom/text";

    content = "|";
    content-background = color.bg;
    content-foreground = color.bg;
    content-padding = "0.5";
  };
}

