let
  utils = (import ./utils.nix);
in
  rec {
    # Note: This kinda feels like a hack and I am none to happy about it maybe in
    # the future I might make this play nicely to with everything else. That is
    # too much effort though. Creating a whole theme for vim
    vim-name = "one";
    vim-statusline = "onedark";
    gtk-name = "Dracula";
    gtk-icon-name = "Adwaita";
    bat-theme-name = "base16";

    bg-primary = black;
    bg-primary-bright = bright-black;
    bg-primary-transparent-argb = utils.transparentify bg-primary;
    bg-primary-bright-transparent-argb = utils.transparentify bg-primary-bright;
    fg-primary = white;
    fg-primary-bright = bright-white;

    accent-primary = green;
    accent-secondary = blue;
    accent-tertiary = magenta;

    alert = red;
    warning = yellow;

    black    = "#282C34";
    red      = "#E06C75";
    green    = "#98C379";
    yellow   = "#d19a66";
    blue     = "#61AFEF";
    magenta  = "#C678DD";
    cyan     = "#56b6c2";
    white    = "#ABB2BF";
    grey     = "#404247";
    bright-black    = "#3e4451";
    bright-red      = "#be5046";
    bright-green    = "#98C379";
    bright-yellow   = "#E5C07B";
    bright-blue     = "#61AFEF";
    bright-magenta  = "#C678DD";
    bright-cyan     = "#56b6c2";
    bright-white    = "#e6efff";
    light-grey      = "#57595e";
  }
