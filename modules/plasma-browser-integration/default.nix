{ config, pkgs, lib, colorscheme, ... }:
{
  home.packages = with pkgs; [
    plasma-browser-integration
  ];

  home.file.".mozilla/native-messaging-hosts/org.kde.plasma.browser_integration.json".source =
    "${pkgs.plasma-browser-integration}/lib/mozilla/native-messaging-hosts/org.kde.plasma.browser_integration.json";
}
