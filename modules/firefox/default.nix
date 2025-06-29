{ pkgs, lib, ... }:
let
  merge = lib.foldr (a: b: a // b) { };
in
{
  programs.firefox = {
    enable = true;
    # package = if pkgs.stdenv.isLinux then pkgs.firefox else pkgs.firefox-bin;
    # package = pkgs.firefox-unwrapped;
    profiles = {
      default = {
        name = "privacy-friendly";
        settings = merge [
          (import ./config/annoyances.nix)
          (import ./config/browser-features.nix)
          (import ./config/privacy.nix)
          (import ./config/tracking.nix)
          (import ./config/security.nix)
        ];
        extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
          clearurls
          darkreader
          facebook-container
          onepassword-password-manager
          return-youtube-dislikes
          sponsorblock
          ublock-origin
          vimium
          temporary-containers
          multi-account-containers
        ];
      };
    };
  };
}
