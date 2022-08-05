{ config, pkgs, libs, ... }:
let
  one-password-id = "aeblfdkhhhdcdjpifhhbdiojplfjncoa";
  adblock-id = "gighmmpiobklfepjocnamgkkbiglidom";
  dark-reader-id = "eimadpbcbfnmbkopoojfekhnkhdbieeh";
  vimium-id = "dbepggeogbaibhgnhhndojpepiihcmeb";
  octotree-id = "bkhaagjahfmjljalopjnoealnfndnagc";
  plasma-integration-id = "cimiefiiaegbelhefglklhhakcgmhkai";
  keepa-id = "neebplgakaahbhdphmkckjjcegoiijjo"; # Amazon price tracker
  grammerly-id = "kbfnbcaeplbcioakkpcpgfkobkghlhen";
  return-youtube-dislikes = "gebbhagfogifgggkldgodflihgfeippi";
  pocket-tube-id = "kdmnjgijlmjgmimahnillepgcgeemffb"; # Youtube subscription manager
  okta-id = "glnpjglilkicbckjpbgcfkogebgllemb";
  unhook-id = "khncfooichmfjbepaaaebmommgaepoid";
in
{
  programs.chromium = {
    enable = true;
    extensions = [
      { id = one-password-id; }
      { id = adblock-id; }
      { id = dark-reader-id; }
      { id = vimium-id; }
      { id = octotree-id; }
      { id = plasma-integration-id; }
      { id = keepa-id; }
      { id = grammerly-id; }
      { id = return-youtube-dislikes; }
      { id = pocket-tube-id; }
      { id = okta-id; }
      { id = unhook-id; }
    ];
  };
  programs.firefox = {
    enable = true;
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
        settings = {
          # Privacy settings
          "privacy.donottrackheader.enabled" = true;
          "privacy.trackingprotection.enabled" = true;
          "privacy.trackingprotection.socialtracking.enabled" = true;
          "privacy.partition.network_state.ocsp_cache" = true;

          # Disable all sorts of telemetry
          "browser.newtabpage.activity-stream.feeds.telemetry" = false;
          "browser.newtabpage.activity-stream.telemetry" = false;
          "browser.ping-centre.telemetry" = false;
          "toolkit.telemetry.archive.enabled" = false;
          "toolkit.telemetry.bhrPing.enabled" = false;
          "toolkit.telemetry.enabled" = false;
          "toolkit.telemetry.firstShutdownPing.enabled" = false;
          "toolkit.telemetry.hybridContent.enabled" = false;
          "toolkit.telemetry.newProfilePing.enabled" = false;
          "toolkit.telemetry.reportingpolicy.firstRun" = false;
          "toolkit.telemetry.shutdownPingSender.enabled" = false;
          "toolkit.telemetry.unified" = false;
          "toolkit.telemetry.updatePing.enabled" = false;

          # As well as Firefox 'experiments'
          "experiments.activeExperiment" = false;
          "experiments.enabled" = false;
          "experiments.supported" = false;
          "network.allow-experiments" = false;

          # Disable Pocket Integration
          "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
          "extensions.pocket.enabled" = false;
          "extensions.pocket.api" = "";
          "extensions.pocket.oAuthConsumerKey" = "";
          "extensions.pocket.showHome" = false;
          "extensions.pocket.site" = "";
        };
      };
    };
  };

}

