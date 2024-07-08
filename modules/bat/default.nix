{ colorscheme, ... }:
{
  programs.bat = {
    enable = true;
    config.theme = colorscheme.bat-theme-name;
  };
}
