{
  config,
  homelab,
  lib,
  ...
}:

let
  mediaRoot = "/srv/media";
  downloadsRoot = "/srv/downloads";
  movieLibrary = "${mediaRoot}/movies";
  tvLibrary = "${mediaRoot}/tv";
  torrentRoot = "${downloadsRoot}/torrents";
  ports = homelab.services;
  qBittorrentDownloadClient = {
    name = "qBittorrent";
    implementation = "QBittorrent";
    fields = {
      host = "localhost";
      port = ports.qbittorrent.syncApi;
      username = "admin";
      password.secret = config.age.secrets.qbittorrentPassword.path;
    };
  };
in
{
  nixarr = {
    enable = true;
    mediaDir = mediaRoot;
    stateDir = "/srv/.state/nixarr";
    mediaUsers = [ "nixypanda" ];

    jellyfin = {
      enable = true;
      openFirewall = true;
    };

    radarr = {
      enable = true;
      openFirewall = true;
      settings-sync.downloadClients = [ qBittorrentDownloadClient ];
    };

    sonarr = {
      enable = true;
      openFirewall = true;
      settings-sync.downloadClients = [ qBittorrentDownloadClient ];
    };

    prowlarr = {
      enable = true;
      openFirewall = true;
      settings-sync = {
        enable-nixarr-apps = false;
        radarr.enable = false;
        sonarr.enable = false;
      };
    };

    qbittorrent = {
      enable = true;
      openFirewall = true;
      webuiPort = ports.qbittorrent.webui;
      peerPort = ports.qbittorrent.peer;
      extraConfig = {
        LegalNotice.Accepted = true;
        Preferences = {
          Downloads = {
            SavePath = "${torrentRoot}/complete";
            TempPath = "${torrentRoot}/incomplete";
            TempPathEnabled = true;
          };
          WebUI = {
            LocalHostAuth = false;
            # qBittorrent stores the WebUI password in its PBKDF2 config format;
            # automation clients use the age-managed secret above.
            Password_PBKDF2 = "@ByteArray(oaZn1TWvluvmROj2WQSsbg==:ASs3XhI21VWkpP0EGEQRTBe97oJkUEvYkcS1zEiyuZb/+g7eZg3y+Q5LUzYsKwvlumNsj1lVWnHxL0Dosrc86w==)";
          };
        };
      };
    };

    seerr = {
      enable = true;
      openFirewall = true;
      port = ports.seerr.local;
    };
  };

  services = {
    # These apps are reached locally or through the tailnet Caddy proxy, not
    # directly from the public internet.
    radarr.settings = {
      auth.required = "Enabled";
      log.analyticsEnabled = false;
      server.port = ports.radarr.local;
      update = {
        automatically = false;
        mechanism = "external";
      };
    };
    sonarr.settings = {
      auth.required = "Enabled";
      log.analyticsEnabled = false;
      server.port = ports.sonarr.local;
      update = {
        automatically = false;
        mechanism = "external";
      };
    };
    prowlarr.settings = {
      auth.required = "Enabled";
      log.analyticsEnabled = false;
      server.port = ports.prowlarr.local;
      update = {
        automatically = false;
        mechanism = "external";
      };
    };
  };

  networking.firewall.allowedUDPPorts = [
    ports.qbittorrent.peer
  ];

  assertions = [
    {
      assertion = movieLibrary != torrentRoot;
      message = "Media library and torrent download paths must be separated.";
    }
    {
      assertion = tvLibrary != torrentRoot;
      message = "TV library and torrent download paths must be separated.";
    }
    {
      assertion = movieLibrary != tvLibrary;
      message = "Movie and TV libraries must be separated.";
    }
  ];

  users = {
    groups.arr-secrets = { };
    users = {
      radarr.extraGroups = [ "arr-secrets" ];
      sonarr.extraGroups = [ "arr-secrets" ];
    };
  };

  environment.etc."homelab/media-paths".text = lib.generators.toKeyValue { } {
    movie_library = movieLibrary;
    tv_library = tvLibrary;
    torrent_downloads = torrentRoot;
    torrent_complete = "${torrentRoot}/complete";
    torrent_incomplete = "${torrentRoot}/incomplete";
  };
}
