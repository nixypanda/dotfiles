{ config, pkgs, libs, ... }:
{
  home.packages = with pkgs; [
    google-chrome
  ];
}
