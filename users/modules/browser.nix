{ config, pkgs, libs, ... }:
{
  programs.firefox = {
    enable = true;
    package = if pkgs.stdenv.isLinux then pkgs.firefox-unwrapped else pkgs.firefox-bin;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      onepassword-password-manager
      ublock-origin
      darkreader
      vimium
      octotree
      plasma-integration

      grammarly
      # return-youtube-dislikes

      okta-browser-plugin
      df-youtube
      clearurls
      sponsorblock
    ];
    profiles = {
      default = {
        name = "Default";
        settings = import ./firefox/config.nix;
      };
    };
  };

}

