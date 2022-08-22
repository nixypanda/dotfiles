{ config, pkgs, lib, colorscheme, ... }:
{
  services.picom = {
    enable = true;
    inactiveOpacity = 0.98;
    activeOpacity = 1.00;
    experimentalBackends = true;
    opacityRules = [
      "100:class_g   *?= 'Firefox'"
      "95:class_g   *?= 'Deadd-notification-center'"
      "75:class_g   *?= 'Rofi'"
    ];
    settings = {
      blur-method = "dual_kawase";
      blur-strength = 8;
      corner-radius = 8;
      round-borders = 1;
      rounded-corners-exclude = [
        "class_g = 'Custom-taffybar'"
      ];
    };
    fade = true;
    fadeDelta = 5;
  };
}
