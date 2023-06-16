{ pkgs, colorscheme, ... }: {
  home.packages = with pkgs; [ (nerdfonts.override { fonts = [ "Hack" ]; }) ];

  xresources = {
    properties = {
      "*.foreground" = colorscheme.fg-primary;
      "*.background" = colorscheme.bg-primary;

      "*.color0" = colorscheme.black;
      "*.color1" = colorscheme.red;
      "*.color2" = colorscheme.green;
      "*.color3" = colorscheme.yellow;
      "*.color4" = colorscheme.blue;
      "*.color5" = colorscheme.magenta;
      "*.color6" = colorscheme.cyan;
      "*.color7" = colorscheme.white;

      "*.color8" = colorscheme.bright-black;
      "*.color9" = colorscheme.bright-red;
      "*.color10" = colorscheme.bright-green;
      "*.color11" = colorscheme.bright-yellow;
      "*.color12" = colorscheme.bright-blue;
      "*.color13" = colorscheme.bright-magenta;
      "*.color14" = colorscheme.bright-cyan;
      "*.color15" = colorscheme.bright-white;

      "XTerm*font" = "xft:Hack Nerd Font Mono:pixelsize=12";
      "*.internalBorder" = 4;

      "Xft.dpi" = 96;
      "Xft.antialias" = true;
      "Xft.hinting" = true;
      "Xft.rgba" = "rgb";
      "Xft.autohint" = false;
      "Xft.hintstyle" = "hintslight";
      "Xft.lcdfilter" = "lcddefault";
    };
  };
}
