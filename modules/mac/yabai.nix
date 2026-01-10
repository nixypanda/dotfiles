{ pkgs, ... }:
{
  services = {
    yabai = {
      enable = true;
      config = {
        layout = "bsp";
        window_placement = "second_child";

        mouse_follows_focus = "on";

        # padding
        top_padding = 5;
        bottom_padding = 5;
        left_padding = 5;
        right_padding = 5;
        window_gap = 5;
      };
      extraConfig = # sh
        ''
          # rules
          yabai -m rule --add app="^System Settings$"    manage=off
          yabai -m rule --add app="^System Information$" manage=off
          yabai -m rule --add app="^System Preferences$" manage=off
          yabai -m rule --add title="Preferences$"       manage=off
          yabai -m rule --add title="Settings$"          manage=off

          # workspace management
          yabai -m space 1  --label web

          # assign apps to spacess
          yabai -m rule --add app="^Firefox$" space=web
          yabai -m rule --add app="kitty" manage=on fullscreen=on
        '';
    };
  };
}
