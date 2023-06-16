let
  # Regex that matches all strings starting with 'eww' that don't end in 'bg'
  non-background-eww-stuff = "^eww(?!.+-bg$).+$";
in {
  services.picom = {
    enable = true;
    experimentalBackends = true;
    backend = "glx";
    vSync = true;

    # Transparency/Opacity
    inactiveOpacity = 0.8;
    activeOpacity = 0.95;
    opacityRules = [
      "100:class_g   *?= 'Firefox'"
      "100:class_g   *?= 'Deadd-notification-center'"
      "100:class_g   *?= 'Rofi'"
    ];

    # Fading
    fade = true;
    fadeDelta = 10;

    # Shadows
    shadow = true;
    shadowExclude = [
      "class_g = 'eww-topbar-btw'"
      "class_g ~= '${non-background-eww-stuff}'"
    ];

    settings = {
      # Blur
      blur-method = "dual_kawase";
      blur-strength = 8;
      blur-backgroud-exclude = [ "class_g = 'eww-topbar-btw'" ];

      # Radius
      corner-radius = 10;
      round-borders = 1;
      rounded-corners-exclude = [ "class_g = 'Custom-taffybar'" ];
    };

  };
}
