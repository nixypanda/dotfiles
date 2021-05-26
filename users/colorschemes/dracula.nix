let
  utils = (import ./utils.nix);
in
rec {
  # Note: This kinda feels like a hack and I am none to happy about it maybe in
  # the future I might make this play nicely to with everything else. That is
  # too much effort though. Creating a whole theme for vim
  vim-name = "dracula";
  vim-statusline = "dracula";
  gtk-name = "Dracula";
  gtk-icon-name = "Moka";
  bat-theme-name = "Dracula";

  bg-primary = black;
  bg-primary-bright = bright-black;
  bg-primary-transparent-argb = utils.transparentify bg-primary;
  bg-primary-bright-transparent-argb = utils.transparentify bg-primary-bright;
  fg-primary = white;
  fg-primary-bright = bright-white;

  accent-primary = blue;
  accent-secondary = magenta;
  accent-tertiary = "#ffb86c";

  alert = red;
  warning = yellow;

  black          = "#282a36";
  red            = "#FF5555";
  green          = "#50FA7B";
  yellow         = "#F1FA8C";
  blue           = "#BD93F9";
  magenta        = "#FF79C6";
  cyan           = "#8BE9FD";
  white          = "#F8F8F2";
  bright-black   = "#44475a";
  bright-red     = "#FF6E6E";
  bright-green   = "#69FF94";
  bright-yellow  = "#FFFFA5";
  bright-blue    = "#D6ACFF";
  bright-magenta = "#FF92DF";
  bright-cyan    = "#A4FFFF";
  bright-white   = "#FFFFFF";
}
