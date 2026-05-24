{ config, lib, ... }:

let
  mediaRoot = "/srv/media";
  downloadsRoot = "/srv/downloads";
  movieLibrary = "${mediaRoot}/movies";
  torrentRoot = "${downloadsRoot}/torrents";
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
      settings-sync.downloadClients = [
        {
          name = "qBittorrent";
          implementation = "QBittorrent";
          fields = {
            host = "localhost";
            port = 8085;
            username = "admin";
            password.secret = config.age.secrets.qbittorrentPassword.path;
          };
        }
      ];
    };

    prowlarr = {
      enable = true;
      openFirewall = true;
      settings-sync = {
        enable-nixarr-apps = true;
        radarr.enable = true;

      };
    };

    qbittorrent = {
      enable = true;
      openFirewall = true;
      webuiPort = 8080;
      peerPort = 58181;
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
            Password_PBKDF2 = "@ByteArray(oaZn1TWvluvmROj2WQSsbg==:ASs3XhI21VWkpP0EGEQRTBe97oJkUEvYkcS1zEiyuZb/+g7eZg3y+Q5LUzYsKwvlumNsj1lVWnHxL0Dosrc86w==)";
          };
        };
      };
    };

    seerr = {
      enable = true;
      openFirewall = true;
    };
  };

  services = {
    radarr.settings = {
      auth.required = "DisabledForLocalAddresses";
      log.analyticsEnabled = false;
      update = {
        automatically = false;
        mechanism = "external";
      };
    };
    prowlarr.settings = {
      auth.required = "DisabledForLocalAddresses";
      log.analyticsEnabled = false;
      update = {
        automatically = false;
        mechanism = "external";
      };
    };
  };

  networking.firewall.allowedUDPPorts = [
    58181
  ];

  assertions = [
    {
      assertion = movieLibrary != torrentRoot;
      message = "Media library and torrent download paths must be separated.";
    }
  ];

  environment.etc."homelab/media-paths".text = lib.generators.toKeyValue { } {
    movie_library = movieLibrary;
    torrent_downloads = torrentRoot;
    torrent_complete = "${torrentRoot}/complete";
    torrent_incomplete = "${torrentRoot}/incomplete";
  };
}
