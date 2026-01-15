{
  pkgs,
  nixpkgs-pinned,
  lib,
  ...
}:
let
  merge = lib.foldr (a: b: a // b) { };
in
{
  programs.firefox = {
    enable = true;
    package = nixpkgs-pinned.firefox;
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
          bitwarden
          clearurls
          darkreader
          facebook-container
          multi-account-containers
          return-youtube-dislikes
          sponsorblock
          temporary-containers
          ublock-origin
          vimium
        ];
      };
    };
  };
}
