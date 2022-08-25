{ config, pkgs, lib, ... }:
let
  merge = lib.foldr (a: b: a // b) { };
in
{
  programs.firefox = {
    enable = true;
    package = if pkgs.stdenv.isLinux then pkgs.firefox else pkgs.firefox-bin;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      # return-youtube-dislikes
      clearurls
      darkreader
      df-youtube
      facebook-container
      grammarly
      octotree
      okta-browser-plugin
      onepassword-password-manager
      plasma-integration
      sponsorblock
      ublock-origin
      vimium
    ];
    profiles = {
      default = {
        name = "Default";
        settings = merge [
          (import ./config/annoyances.nix)
          (import ./config/browser-features.nix)
          (import ./config/privacy.nix)
          (import ./config/tracking.nix)
          (import ./config/security.nix)
        ];
      };
      # This does not have as strict privacy settings as the default profile.
      # It uses the default firefox settings. Useful when something is not
      # working using the default profile
      shit = {
        name = "crap";
        id = 1;
      };
    };
  };
}
