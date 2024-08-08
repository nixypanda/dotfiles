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
          yabai -m space 1  --label web-aux
          yabai -m space 2  --label web-main
          yabai -m space 3  --label code
          yabai -m space 4  --label notes

          # assign apps to spacess
          yabai -m rule --add app="^Firefox$" space=web-main
          yabai -m rule --add app="^Google Chrome$" space=web-aux
          yabai -m rule --add app="kitty" space=code
          yabai -m rule --add app="Microsoft OneNote" space=notes
        '';
    };
  };
}
