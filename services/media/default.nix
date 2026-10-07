{
  config,
  homelab,
  lib,
  pkgs,
  ...
}:

let
  mediaRoot = "/srv/media";
  downloadsRoot = "${mediaRoot}/downloads";
  movieLibrary = "${mediaRoot}/library/movies";
  tvLibrary = "${mediaRoot}/library/shows";
  musicLibrary = "${mediaRoot}/library/music";
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
  bazarrSyncArrSettings = import ./bazarr-sync-arr-settings.nix {
    inherit pkgs;
    stateDir = config.nixarr.stateDir;
    sonarrPort = ports.sonarr.local;
    radarrPort = ports.radarr.local;
    bazarrPort = ports.bazarr.local;
  };
  mediaUnlinked = import ./media-unlinked.nix { inherit pkgs; };
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

    bazarr = {
      enable = true;
      openFirewall = true;
      port = ports.bazarr.local;
      settings-sync = {
        radarr = {
          enable = true;
          config.sync_only_monitored_movies = true;
        };
        sonarr = {
          enable = true;
          config = {
            sync_only_monitored_series = true;
            sync_only_monitored_episodes = true;
          };
        };
      };
    };

    radarr = {
      enable = true;
      openFirewall = true;
      port = ports.radarr.local;
      settings-sync.downloadClients = [ qBittorrentDownloadClient ];
    };

    sonarr = {
      enable = true;
      openFirewall = true;
      port = ports.sonarr.local;
      settings-sync.downloadClients = [ qBittorrentDownloadClient ];
    };

    recyclarr = {
      enable = true;
      schedule = "daily";
      configuration = {
        radarr.movies = {
          base_url = "http://127.0.0.1:${toString ports.radarr.local}";
          api_key = "!env_var RADARR_API_KEY";

          # Use the maintained TRaSH UHD profile. Only its existing 1080p
          # Bluray/WEB qualities are enabled as fallback; CAM, telesync, DVD,
          # SD, Remux, and other unwanted sources remain disabled. Radarr's
          # global indexer limit provides the separate absolute 25 GB ceiling.
          quality_definition.type = "movie";
          quality_profiles = [
            {
              trash_id = "64fb5f9858489bdac2af690e27c8f42f"; # UHD Bluray + WEB
              reset_unmatched_scores.enabled = true;
              qualities = [
                { name = "Bluray-2160p"; }
                {
                  name = "WEB 2160p";
                  qualities = [
                    "WEBDL-2160p"
                    "WEBRip-2160p"
                  ];
                }
                { name = "Bluray-1080p"; }
                {
                  name = "WEB 1080p";
                  qualities = [
                    "WEBDL-1080p"
                    "WEBRip-1080p"
                  ];
                }
              ];
            }
          ];

          # Remove formats previously managed by profiles no longer configured.
          delete_old_custom_formats = true;

          media_naming = {
            folder = "jellyfin-tmdb";
            movie = {
              rename = true;
              standard = "standard";
            };
          };
          media_management.propers_and_repacks = "do_not_prefer";
        };

        sonarr.shows = {
          base_url = "http://127.0.0.1:${toString ports.sonarr.local}";
          api_key = "!env_var SONARR_API_KEY";

          # Use the maintained standard WEB profile, with only its existing
          # 1080p WEB qualities enabled as fallback. Lower-quality sources and
          # Bluray/Remux releases remain disabled.
          quality_definition.type = "series";
          quality_profiles = [
            {
              trash_id = "d1498e7d189fbe6c7110ceaabb7473e6"; # WEB-2160p
              reset_unmatched_scores.enabled = true;
              qualities = [
                {
                  name = "WEB 2160p";
                  qualities = [
                    "WEBRip-2160p"
                    "WEBDL-2160p"
                  ];
                }
                {
                  name = "WEB 1080p";
                  qualities = [
                    "WEBRip-1080p"
                    "WEBDL-1080p"
                  ];
                }
              ];
            }
          ];

          delete_old_custom_formats = true;

          media_naming = {
            series = "jellyfin-tvdb";
            season = "default";
            episodes = {
              rename = true;
              standard = "default";
              daily = "default";
              anime = "default";
            };
          };
          media_management.propers_and_repacks = "do_not_prefer";
        };
      };
    };

    prowlarr = {
      enable = true;
      openFirewall = true;
      port = ports.prowlarr.local;
      settings-sync.enable-nixarr-apps = true;
    };

    qbittorrent = {
      enable = true;
      openFirewall = true;
      webuiPort = ports.qbittorrent.webui;
      peerPort = ports.qbittorrent.peer;
      extraConfig = {
        BitTorrent = {
          "Session\\DefaultSavePath" = "${torrentRoot}/complete";
          "Session\\TempPath" = "${torrentRoot}/incomplete";
          "Session\\TempPathEnabled" = true;
        };
        LegalNotice.Accepted = true;
        Preferences = {
          "Downloads\\SavePath" = "${torrentRoot}/complete";
          "Downloads\\TempPath" = "${torrentRoot}/incomplete";
          "Downloads\\TempPathEnabled" = true;
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
      auth.required = "DisabledForLocalAddresses";
      log.analyticsEnabled = false;
      update = {
        automatically = false;
        mechanism = "external";
      };
    };
    sonarr.settings = {
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

  systemd.services = {
    # Jellyfin and the media automation services share the media group. Keep
    # downloaded media, artwork, and metadata group-writable so the separate
    # service users can create hardlinks and refresh files in place.
    jellyfin.serviceConfig.UMask = lib.mkForce "0002";
    # qBittorrent can exit cleanly right after boot (for example while the
    # media mount settles) and then stays down, which breaks every *arr app
    # and qui. The exit status is 0, so "on-failure" would not catch it.
    qbittorrent.serviceConfig = {
      UMask = lib.mkForce "0002";
      Restart = "always";
      RestartSec = 5;
    };

    # The pinned nixarr helper sends JSON, but Bazarr 1.6 only reads form data
    # on this endpoint. Keep nixarr's ordering and credential groups while
    # replacing only the incompatible command.
    bazarr-sync-config.serviceConfig.ExecStart = lib.mkForce "${lib.getExe bazarrSyncArrSettings}";

  };

  networking.firewall.allowedUDPPorts = [
    ports.qbittorrent.peer
  ];

  environment.systemPackages = [ mediaUnlinked ];

  systemd.tmpfiles.rules = [
    "d ${torrentRoot} 2775 qbittorrent media - -"
    "d ${torrentRoot}/complete 2775 qbittorrent media - -"
    "d ${torrentRoot}/incomplete 2775 qbittorrent media - -"
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
      assertion = musicLibrary != torrentRoot;
      message = "Music library and torrent download paths must be separated.";
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
    music_library = musicLibrary;
    torrent_downloads = torrentRoot;
    torrent_complete = "${torrentRoot}/complete";
    torrent_incomplete = "${torrentRoot}/incomplete";
  };
}
