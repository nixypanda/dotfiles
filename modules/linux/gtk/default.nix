{ pkgs, colorscheme, ... }:
{
  home.packages = with pkgs; [
    lxappearance
    dracula-theme
    moka-icon-theme
    numix-icon-theme-square
    whitesur-icon-theme
    palenight-theme
  ];

  gtk = {
    enable = true;
    font = {
      name = "TeX Gyre Heros 10";
    };
    iconTheme = {
      name = colorscheme.gtk-icon-name;
    };
    theme = {
      name = colorscheme.gtk-name;
    };
  };
}
