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

      enabled_layouts = "Tall,Grid,Stack";
      shell = "${pkgs.nushell}/bin/nu";

      tab_bar_edge = "top";
      tab_bar_style = "powerline";
      tab_powerline_style = "slanted";
      tab_bar_min_tabs = 1;
      tab_title_template = "󰆍 {index} → {title} ┊ 󰕮 ‹{layout_name}›";

      macos_option_as_alt = "left";
      macos_show_window_title_in = "none";
    };
    keybindings = {
      "cmd+enter" = "new_window_with_cwd";
    };
  };

  home.file = {
    "${config.xdg.configHome}/kitty/quick-access-terminal.conf".text = ''
      lines 80
      columns 200
      background_opacity 0.95
      edge center-sized
    '';

    "${config.xdg.configHome}/kitty/dark-theme.auto.conf" = {
      text = ''include ${pkgs.kitty-themes}/share/kitty-themes/themes/${colorscheme.kitty-name-dark}.conf'';
    };
    "${config.xdg.configHome}/kitty/light-theme.auto.conf" = {
      text = ''include ${pkgs.kitty-themes}/share/kitty-themes/themes/${colorscheme.kitty-name-light}.conf'';
    };
    "${config.xdg.configHome}/kitty/no-preference-theme.auto.conf" = {
      text = ''include ${pkgs.kitty-themes}/share/kitty-themes/themes/${colorscheme.kitty-name-dark}.conf'';
    };
  };
}
