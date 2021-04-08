{ colors }:
  {
    env = {
      "TERM" = "xterm-256color";
    };

    background_opacity = 1.0;

    window = {
      # Window dimensions (changes require restart)
      #
      # Specified in number of columns/lines, not pixels.
      # If both are `0`, this setting is ignored.
      # dimensions = {
      #   columns = 200;
      #   lines = 60;
      # };

      # Window padding (changes require restart)
      #
      # Blank space added around the window in pixels. This padding is scaled
      # by DPI and the specified value is always added at both opposing sides.
      padding = {x = 5; y = 5;};
      dynamic_padding = false;

      # Window decorations
      #
      # Available values:
      # - `full`: Window with title bar and title bar buttons
      # - `none`: Window without title bar, rounded corners, or drop shadow
      # - `transparent`: Window with title bar with transparent background and title
      #   bar buttons
      # - `buttonless`: Window with title bar with transparent background and no
      #   title bar buttons
      # decorations = "full";
    };

    font = {
      size = 10;

      normal.family = "Hack Nerd Font Mono";
      normal.style = "Regular";
      bold.family = "Hack Nerd Font Mono";
      bold.style = "Bold";
      italic.family = "Hack Nerd Font Mono";
      italic.style = "Italic";
      bold_italic.family = "Hack Nerd Font Mono";
      bold_italic.style = "Bold Italic";
    };

    # Cursor style
    #
    # Values for 'style':
    #   - ▇ Block
    #   - _ Underline
    #   - | Beam
    cursor = {
      style = "Block";
      unfocused_hollow = true;
    };

    # Shell
    #
    # You can set `shell.program` to the path of your favorite shell, e.g. `/bin/fish`.
    # Entries in `shell.args` are passed unmodified as arguments to the shell.
    shell = {
      program = "/home/sherub/.nix-profile/bin/nu";
    };

    # Colors (One Dark)
    colors = {
      # Default colors
      primary = {
        background  = colors.bg-primary;
        foreground  = colors.fg-primary;

        # Bright and dim foreground colors
        #
        # The dimmed foreground color is calculated automatically if it is not
        # present.  If the bright foreground color is not set, or
        # `draw_bold_text_with_bright_colors` is `false`, the normal foreground
        # color will be used.
        #dim_foreground  = "0x9a9a9a";
        bright_foreground  = colors.fg-primary-bright;
      };

      # Cursor colors
      #
      # Colors which should be used to draw the terminal cursor. If these are unset,
      # the cursor color will be the inverse of the cell color.
      #cursor 
      #  text  = "0x000000";
      #  cursor  = "0xffffff";

      # Normal colors
      normal = {
        black    = colors.black  ;
        red      = colors.red    ;
        green    = colors.green  ;
        yellow   = colors.yellow ;
        blue     = colors.blue   ;
        magenta  = colors.magenta;
        cyan     = colors.cyan   ;
        white    = colors.white  ;
      };

      # # Bright colors
      # bright = {
      #   black    = "0x5c6370";
      #   red      = "0xe06c75";
      #   green    = "0x98c379";
      #   yellow   = "0xd19a66";
      #   blue     = "0x61afef";
      #   magenta  = "0xc678dd";
      #   cyan     = "0x56b6c2";
      #   white    = "0xe6efff";
      # };
      #
      # # Dim colors
      # #
      # # If the dim colors are not set, they will be calculated automatically based
      # # on the `normal` colors.
      # dim = {
      #   black    = "0x1e2127";
      #   red      = "0xe06c75";
      #   green    = "0x98c379";
      #   yellow   = "0xd19a66";
      #   blue     = "0x61afef";
      #   magenta  = "0xc678dd";
      #   cyan     = "0x56b6c2";
      #   white    = "0x828791";
      # };
    };

    key_bindings = [
      { key = "V";        mods = "Command"; action = "Paste"; }
      { key = "C";        mods = "Command"; action = "Copy" ; }
      { key = "Paste";                      action = "Paste"; }
      { key = "Copy";                       action = "Copy" ; }
      { key = "H";        mods = "Command"; action = "Hide" ; }
      { key = "Q";        mods = "Command"; action = "Quit" ; }
      { key = "W";        mods = "Command"; action = "Quit" ; }
    ];
  }
