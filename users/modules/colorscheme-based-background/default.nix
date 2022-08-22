{ config, pkgs, lib, colorscheme, ... }:
{
  services.random-background = {
    enable = true;
    imageDirectory = "%h/Pictures/backgrounds/${colorscheme.name}";
  };
}
