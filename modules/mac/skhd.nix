{ pkgs, ... }:
{
  services = {
    skhd = {
      enable = true;
      skhdConfig =
        let
          yabai = "${pkgs.yabai}/bin/yabai";
          jq = "${pkgs.jq}/bin/jq";
        in
        # sh
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
