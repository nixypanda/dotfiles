{ pkgs, ... }:
{
  system.defaults = {
    SoftwareUpdate.AutomaticallyInstallMacOSUpdates = false;
    NSGlobalDomain = {
      InitialKeyRepeat = 10;
      KeyRepeat = 1;
      AppleInterfaceStyle = "Dark";
      NSAutomaticCapitalizationEnabled = false;
      NSAutomaticDashSubstitutionEnabled = false;
      NSAutomaticPeriodSubstitutionEnabled = false;
      NSAutomaticQuoteSubstitutionEnabled = false;
      NSAutomaticSpellingCorrectionEnabled = false;
      NSAutomaticWindowAnimationsEnabled = false;

    };
    dock = {
      autohide = true;
      mru-spaces = false;
      show-recents = false;
    };
  };
  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToEscape = true;
  };
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
      extraConfig = ''
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

    skhd = {
      enable = true;
      skhdConfig =
        let
          yabai = "${pkgs.yabai}/bin/yabai";
          jq = "${pkgs.jq}/bin/jq";
        in
        ''
          # window focus
          alt - h : ${yabai} -m window --focus west
          alt - l : ${yabai} -m window --focus east

          # focus window in stacked
          alt - j : if [ "$(${yabai} -m query --spaces --space | ${jq} -r '.type')" = "stack" ]; then ${yabai} -m window --focus stack.next || ${yabai} -m window --focus stack.first; else ${yabai} -m window --focus south; fi
          alt - v : if [ "$(${yabai} -m query --spaces --space | ${jq} -r '.type')" = "stack" ]; then ${yabai} -m window --focus stack.next || ${yabai} -m window --focus stack.first; else ${yabai} -m window --focus south; fi

          alt - k : if [ "$(${yabai} -m query --spaces --space | ${jq} -r '.type')" = "stack" ]; then ${yabai} -m window --focus stack.prev || ${yabai} -m window --focus stack.last; else ${yabai} -m window --focus north; fi
          alt - c : if [ "$(${yabai} -m query --spaces --space | ${jq} -r '.type')" = "stack" ]; then ${yabai} -m window --focus stack.prev || ${yabai} -m window --focus stack.last; else ${yabai} -m window --focus north; fi


          # toggle layout
          alt - d : ${yabai} -m space --layout $(${yabai} -m query --spaces --space | ${jq} -r 'if .type == "bsp" then "stack" else "bsp" end')

          # display focus
          alt - p: ${yabai} -m display --focus east
          alt - a: ${yabai} -m display --focus west

          # maximize a window
          alt - m : ${yabai} -m window --toggle zoom-fullscreen

          # swap spaces
          alt - s : ${yabai} -m space --display recent
        '';
    };
  };
}
