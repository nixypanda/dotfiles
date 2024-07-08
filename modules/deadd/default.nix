{ pkgs, colorscheme, ... }:
{
  home.packages = with pkgs; [ deadd-notification-center ];

  home.file.".config/deadd/deadd.css".source = colorscheme.deadd-css-file;
}
