{
  config,
  homelab,
  lib,
  paths,
  ...
}:

let
  musicLibrary = paths.library.music;
  beetsLibrary = paths.library.musicBeets;
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

    # Navidrome reads two music libraries: Lidarr's tree and the separate Beets
    # import tree. Both are read-only to Navidrome; its database, playlists,
    # user accounts, and cache stay outside the media tree.
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

  # The NixOS Navidrome module only binds MusicFolder read-only. Bind the Beets
  # tree as well so Settings -> Libraries can add it to the same instance.
  systemd.services = lib.mkIf config.services.navidrome.enable {
    navidrome.serviceConfig.BindReadOnlyPaths = lib.mkAfter [ beetsLibrary ];
  };
}
