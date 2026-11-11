{
  config,
  homelab,
  lib,
  paths,
  ...
}:

let
  musicLibrary = paths.library.music;
  soulsyncLibrary = paths.library.musicSoulSync;
  navidromeState = paths.state.navidrome;
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

    # Navidrome reads Lidarr's music library. Its database, playlists, user
    # accounts, and cache stay outside the media tree. SoulSync's library is
    # mounted read-only as well so Settings -> Libraries can add it.
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
        Scanner.Schedule = "@every 5m";
      };
    };
  };

  # The NixOS Navidrome module only binds MusicFolder read-only. Bind the
  # SoulSync tree as well so Settings -> Libraries can add it to the same
  # instance.
  systemd.services = lib.mkIf config.services.navidrome.enable {
    navidrome.serviceConfig.BindReadOnlyPaths = lib.mkAfter [
      soulsyncLibrary
    ];
  };
}
