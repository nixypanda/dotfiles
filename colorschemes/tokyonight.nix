let
  utils = import ./utils.nix;
in
rec {
  name = "tokyonight";

  # Note: This kinda feels like a hack and I am none to happy about it maybe in
  # the future I might make this play nicely to with everything else. That is
  # too much effort though. Creating a whole theme for vim
  vim-name = "catppuccin-macchiato";
  vim-statusline = "tokyonight";
  gtk-name = "palenight";
  gtk-icon-name = "Moka";
  bat-theme-name = "base16";
  deadd-css-file = ../modules/deadd/tokyonight.css;

  bg-primary = "#24283b";
  bg-primary-bright = "#1f2335";
  bg-primary-transparent-argb = utils.transparentify accent-primary;
  bg-primary-bright-transparent-argb = utils.transparentify bg-primary-bright;
  fg-primary = bright-white;
  fg-primary-bright = "#fefefe";

  accent-primary = blue;
  accent-secondary = magenta;
  accent-tertiary = "#ffb86c";

  alert = red;
  warning = yellow;

  black = "#15161E";
  red = "#f7768e";
  green = "#9ece6a";
  yellow = "#e0af68";
  blue = "#7aa2f7";
  magenta = "#bb9af7";
  cyan = "#7dcfff";
  white = "#a9b1d6";
  bright-black = "#414868";
  bright-red = "#f7768e";
  bright-green = "#9ece6a";
  bright-yellow = "#e0af68";
  bright-blue = "#7aa2f7";
  bright-magenta = "#bb9af7";
  bright-cyan = "#7dcfff";
  bright-white = "#c0caf5";
}
