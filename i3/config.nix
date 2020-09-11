{ colorscheme }:
  rec {
    fonts = ["Hack Nerd Font 12"];
    modifier = "Mod1";
    startup = [
      {
        command = "custom-i3-polybar-launch";
        always = true;
        notification = false;
      }
    ];
    gaps = {
      outer = 10;
      inner = 10;

      smartGaps = false;
      smartBorders = "no_gaps";
    };
    bars = [ ];

    keybindings = (import ./keybindings.nix) {mod = modifier; };

    window = {
      border = 4;
    };
    
    colors = {
      background = colorscheme.bg-primary;
      focused = {
        background = colorscheme.accent-primary;
        text = colorscheme.fg-primary;
        # looks like this is brighter accent
        indicator = colorscheme.accent-primary-bright;
        # looks like this is dimmer accent. Maybe I can right a function that
        # computes these
        border = "#4c7899";
        childBorder = colorscheme.accent-primary;
      };
    };
  }
