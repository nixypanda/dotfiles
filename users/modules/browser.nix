{ config, pkgs, libs, ... }:
{
  programs.firefox = {
    enable = true;
    package = if pkgs.stdenv.isLinux then pkgs.firefox-unwrapped else pkgs.firefox-bin;
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
        settings = import ./firefox/config.nix;
      };
    };
  };

}

