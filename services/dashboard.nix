{ homelab, pkgs, ... }:
let
  inherit (homelab) tailnetHost tailnetUrl localUrl;
  inherit (homelab) finance services;

  homepagePkg = pkgs.homepage-dashboard.override {
    enableLocalIcons = true;
  };

  homepageWithPaisa = homepagePkg.overrideAttrs (old: {
    postInstall = (old.postInstall or "") + ''
      cp ${pkgs.paisa.src}/brand/logo.svg $out/share/homepage/public/icons/paisa.svg
    '';
  });
in
{
  services.homepage-dashboard = {
    enable = true;
    package = homepageWithPaisa;
    listenPort = services.homepage.local;
    openFirewall = false;
    environmentFiles = [ "/var/lib/stremio-local-debrid/homepage.env" ];
    allowedHosts = builtins.concatStringsSep "," [
      "localhost:${toString services.homepage.local}"
      "127.0.0.1:${toString services.homepage.local}"
      "srt-n01-rivendell:${toString services.homepage.local}"
      "100.127.3.54:${toString services.homepage.local}"
      tailnetHost
      "${tailnetHost}:${toString services.homepage.local}"
    ];

    settings = {
      title = "Rivendell";
      headerStyle = "clean";
      statusStyle = "dot";
      target = "_blank";
      useEqualHeights = true;
      maxGroupColumns = 3;
    };

    widgets = [
      {
        resources = {
          label = "Rivendell";
          cpu = true;
          memory = true;
          disk = "/";
          network = "eno1";
          uptime = true;
          units = "metric";
        };
      }
      {
        search = {
          provider = "duckduckgo";
          target = "_blank";
        };
      }
    ];

    services = [
      {
        Health = [
          {
            "Home Assistant" = {
              icon = "home-assistant.png";
              href = tailnetUrl services.homeAssistant.tailnet;
              description = "Home automation";
              siteMonitor = localUrl services.homeAssistant.local;
            };
          }
          {
            "CalCo" = {
              icon = "${tailnetUrl services.calco.tailnet}/favicon-32x32.png";
              href = tailnetUrl services.calco.tailnet;
              description = "Food and nutrition tracker";
              siteMonitor = "${localUrl services.calco.local}/api/health";
            };
          }
        ];
      }
      {
        Finance = [
          {
            "Hedger" = {
              icon = "${tailnetUrl finance.mine.hedgerTailnet}/icon.svg";
              href = tailnetUrl finance.mine.hedgerTailnet;
              description = "Ledger dashboard";
              siteMonitor = "${localUrl finance.mine.hedger}/api/v1/health/ready";
            };
          }
          {
            "Paisa" = {
              icon = "/icons/paisa.svg";
              href = tailnetUrl finance.mine.paisaTailnet;
              description = "Mine finance dashboard";
              siteMonitor = localUrl finance.mine.paisa;
            };
          }
        ];
      }
      {
        Library = [
          {
            Kavita = {
              icon = "kavita.png";
              href = tailnetUrl services.kavita.tailnet;
              description = "Books and manga";
              siteMonitor = localUrl services.kavita.local;
            };
          }
          {
            Audiobookshelf = {
              icon = "audiobookshelf.png";
              href = tailnetUrl services.audiobookshelf.tailnet;
              description = "Audiobooks and podcasts";
              siteMonitor = localUrl services.audiobookshelf.local;
            };
          }
        ];
      }
      {
        Media = [
          {
            Jellyfin = {
              icon = "jellyfin.png";
              href = tailnetUrl services.jellyfin.tailnet;
              description = "Movies, TV, and anime";
              siteMonitor = localUrl services.jellyfin.local;
            };
          }
          {
            Navidrome = {
              icon = "navidrome.png";
              href = tailnetUrl services.navidrome.tailnet;
              description = "Music streaming";
              siteMonitor = localUrl services.navidrome.local;
            };
          }
          {
            SoulSync = {
              icon = "${tailnetUrl services.soulsync.tailnet}/static/favicon.png";
              href = tailnetUrl services.soulsync.tailnet;
              description = "Music discovery and collection";
              siteMonitor = localUrl services.soulsync.local;
            };
          }
        ];
      }
      {
        Discover = [
          {
            Seerr = {
              icon = "jellyseerr.png";
              href = tailnetUrl services.seerr.tailnet;
              description = "Media requests";
              siteMonitor = localUrl services.seerr.local;
            };
          }
          {
            Shelfmark = {
              icon = "mdi-book-search";
              href = tailnetUrl services.shelfmark.tailnet;
              description = "Book and audiobook discovery";
              siteMonitor = "${localUrl services.shelfmark.local}/api/health";
            };
          }
        ];
      }
      {
        Downloads = [
          {
            qBittorrent = {
              icon = "qbittorrent.png";
              href = tailnetUrl services.qbittorrent.tailnet;
              description = "Torrent client";
              siteMonitor = localUrl services.qbittorrent.webui;
            };
          }
        ];
      }
      {
        Automation = [
          {
            Radarr = {
              icon = "radarr.png";
              href = tailnetUrl services.radarr.tailnet;
              description = "Movie automation";
              siteMonitor = localUrl services.radarr.local;
            };
          }
          {
            Sonarr = {
              icon = "sonarr.png";
              href = tailnetUrl services.sonarr.tailnet;
              description = "TV and anime automation";
              siteMonitor = localUrl services.sonarr.local;
            };
          }
          {
            Lidarr = {
              icon = "lidarr.png";
              href = tailnetUrl services.lidarr.tailnet;
              description = "Music automation";
              siteMonitor = localUrl services.lidarr.local;
            };
          }
          {
            Prowlarr = {
              icon = "prowlarr.png";
              href = tailnetUrl services.prowlarr.tailnet;
              description = "Indexer management";
              siteMonitor = localUrl services.prowlarr.local;
            };
          }
          {
            Bazarr = {
              icon = "bazarr.png";
              href = tailnetUrl services.bazarr.tailnet;
              description = "Subtitle automation";
              siteMonitor = localUrl services.bazarr.local;
            };
          }
          {
            OnePacerr = {
              icon = "onepacerr.png";
              href = tailnetUrl services.onepacerr.tailnet;
              description = "One Pace automatic downloader and Jellyfin organizer";
              siteMonitor = "${localUrl services.onepacerr.local}/api/v1/healthz";
            };
          }
          {
            "Stremio Local Debrid" = {
              icon = "stremio.png";
              href = "{{HOMEPAGE_VAR_STREMIO_LOCAL_DEBRID_URL}}";
              description = "Torrent cache and download dashboard";
              siteMonitor = "{{HOMEPAGE_VAR_STREMIO_LOCAL_DEBRID_MONITOR}}";
            };
          }
        ];
      }
      {
        Network = [
          {
            "Pi-hole" = {
              icon = "pi-hole.png";
              href = tailnetUrl services.pihole.tailnet;
              description = "DNS and ad blocking";
              siteMonitor = localUrl services.pihole.web;
            };
          }
        ];
      }
    ];
  };

  # The dashboard link comes from a runtime-generated environment file, so
  # start it after the addon has written the current tokenized URL.
  systemd.services.homepage-dashboard = {
    wants = [ "stremio-local-debrid.service" ];
    after = [ "stremio-local-debrid.service" ];
  };
}
