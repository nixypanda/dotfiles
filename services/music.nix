{ homelab, ... }:

let
  musicLibrary = "/srv/media/library/music";
  navidromeState = "/srv/.state/navidrome";
  ports = homelab.services;
in
{
  # Lidarr owns and organizes the music files. Prowlarr discovers this
  # nixarr-managed instance automatically through its existing settings sync.
  nixarr.lidarr = {
    enable = true;
    openFirewall = false;
    port = ports.lidarr.local;
  };

  services = {
    lidarr.settings = {
      auth.required = "DisabledForLocalAddresses";
      log.analyticsEnabled = false;
      server.bindaddress = "127.0.0.1";
      update = {
        automatically = false;
        mechanism = "external";
      };
    };

    # Navidrome only reads Lidarr's library. Its database, playlists, user
    # accounts, and cache stay outside the media tree.
    navidrome = {
      enable = true;
      group = "media";
      openFirewall = false;
      settings = {
        Address = "127.0.0.1";
        Port = ports.navidrome.local;
        MusicFolder = musicLibrary;
        DataFolder = navidromeState;
        CacheFolder = "${navidromeState}/cache";
        EnableInsightsCollector = false;
        ScanSchedule = "@every 5m";
      };
    };
  };
}
