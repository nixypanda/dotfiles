{
  pkgs,
  colorscheme,
  config,
  ...
}:
{
  programs.kitty = {
    enable = true;
    settings = {
      font_size = 10;
      scrollback_lines = 10000;
      input_delay = 1;

      macos_option_as_alt = "left";
      macos_show_window_title_in = "none";
    };
  };

  home.file = {

    "${config.xdg.configHome}/kitty/dark-theme.auto.conf" = {
      text = ''
        include ${pkgs.kitty-themes}/share/kitty-themes/themes/${colorscheme.kitty-name-dark}.conf
      '';
    };

    "${config.xdg.configHome}/kitty/light-theme.auto.conf" = {
      text = ''
        include ${pkgs.kitty-themes}/share/kitty-themes/themes/${colorscheme.kitty-name-light}.conf
      '';
    };

    "${config.xdg.configHome}/kitty/no-preference-theme.auto.conf" = {
      text = ''
        include ${pkgs.kitty-themes}/share/kitty-themes/themes/${colorscheme.kitty-name-dark}.conf
      '';
    };

  };
}
