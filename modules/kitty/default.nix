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
      font_size = 12;
      scrollback_lines = 10000;
      input_delay = 1;

      enabled_layouts = "Tall,Grid,Stack";
      shell = "${pkgs.nushell}/bin/nu";
      editor = "~/.nix-profile/bin/nvim";

      tab_bar_edge = "top";
      tab_bar_style = "powerline";
      tab_powerline_style = "slanted";
      tab_bar_min_tabs = 1;
      tab_bar_filter = "session:~ or session:^$";
      tab_title_template = " 󰆍 {index} → {title} ";
      active_tab_title_template = " {session_name} ┊ 󰆍 {index} → {title} ┊ 󰕮 ‹{layout_name}› ";

      macos_option_as_alt = "left";
      macos_show_window_title_in = "none";

      # opencode requires remote control
      allow_remote_control = "yes";
      listen_on = "unix:/tmp/kitty";
    };
    keybindings = {
      "cmd+enter" = "new_window_with_cwd";
      "cmd+t" = "new_tab_with_cwd";
      "cmd+shift+r" = "set_tab_title";
      "cmd+g" = "swap_with_window";
      # closing
      "cmd+x" = "close_window";
      "cmd+shift+x" = "close_tab";
      # vim-like keybinds for navigation
      "cmd+j" = "next_tab";
      "cmd+k" = "previous_tab";
      "cmd+l" = "next_window";
      "cmd+h" = "previous_window";
      "cmd+shift+j" = "move_tab_forward";
      "cmd+shift+k" = "move_tab_backward";
      "cmd+shift+l" = "move_window_forward";
      "cmd+shift+h" = "move_window_backward";
      "cmd+d" = "detach_window ask";
      # font size
      "cmd+equal" = "change_font_size all +1.0";
      "cmd+minus" = "change_font_size all -1.0";
      # resizing (not-vim-like - broken)
      "cmd+shift+equal" = "resize_window wider";
      "cmd+shift+minus" = "resize_window narrower";
      # layout
      "cmd+n" = "next_layout";
      "cmd+f" = "toggle_layout stack";
      # sessions
      "cmd+s" = "goto_session ~/.local/state/kitty/sessions";
      "cmd+shift+s" = "save_as_session --use-foreground-process --base-dir ~/.local/state/kitty/session/";
      "cmd+1" = "goto_session ~/.local/state/kitty/sessions/reproducible-me.kitty-session";
      "cmd+2" = "goto_session ~/.local/state/kitty/sessions/monadic-trials.kitty-session";
      "cmd+w" =
        "combine : save_as_session --use-foreground-process --save-only --base-dir ~/.local/state/kitty/sessions : quit";
      # scrollback
      "cmd+/" = "show_scrollback";
      # shell integration
      "cmd+p" = "show_last_command_output";
    };
  };

  home.file = {
    "${config.xdg.configHome}/kitty/quick-access-terminal.conf".text = ''
      lines 80
      columns 200
      background_opacity 0.95
      edge center-sized
    '';
    "${config.xdg.configHome}/kitty/open-actions.conf".text = ''
      protocol file
      mime image/*
      action launch --type=overlay kitten icat --hold -- $FILE_PATH

      protocol file
      mime text/*
      action launch --type=tab $EDITOR $FILE_PATH
    '';

    "${config.xdg.configHome}/kitty/dark-theme.auto.conf" = {
      text = "include ${pkgs.kitty-themes}/share/kitty-themes/themes/${colorscheme.kitty-name-dark}.conf";
    };
    "${config.xdg.configHome}/kitty/light-theme.auto.conf" = {
      text = "include ${pkgs.kitty-themes}/share/kitty-themes/themes/${colorscheme.kitty-name-light}.conf";
    };
    "${config.xdg.configHome}/kitty/no-preference-theme.auto.conf" = {
      text = "include ${pkgs.kitty-themes}/share/kitty-themes/themes/${colorscheme.kitty-name-dark}.conf";
    };
  };
}
