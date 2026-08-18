{ homelab, pkgs, ... }:
let
  inherit (homelab) tailnetHost;
  inherit (homelab) finance services;

  homepagePkg = pkgs.homepage-dashboard.override {
    enableLocalIcons = true;
  };

  homepageWithPaisa = homepagePkg.overrideAttrs (old: {
    postInstall = (old.postInstall or "") + ''
      cp ${pkgs.paisa.src}/brand/logo.svg $out/share/homepage/public/icons/paisa.svg
    '';
  });

  tailnetUrl = port: "https://${tailnetHost}:${toString port}";
  localUrl = port: "http://127.0.0.1:${toString port}";
in
{
  services.homepage-dashboard = {
    enable = true;
    package = homepageWithPaisa;
    listenPort = services.homepage.local;
    openFirewall = false;
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
            "Hedger Mine" = {
              icon = "${tailnetUrl finance.mine.hedgerTailnet}/icon.svg";
              href = tailnetUrl finance.mine.hedgerTailnet;
              description = "Mine ledger dashboard";
              siteMonitor = "${localUrl finance.mine.hedger}/api/v1/health/ready";
            };
          }
          {
            "Hedger Wife" = {
              icon = "${tailnetUrl finance.wife.hedgerTailnet}/icon.svg";
              href = tailnetUrl finance.wife.hedgerTailnet;
              description = "Wife ledger dashboard";
              siteMonitor = "${localUrl finance.wife.hedger}/api/v1/health/ready";
            };
          }
          {
            "Hedger Combined" = {
              icon = "${tailnetUrl finance.combined.hedgerTailnet}/icon.svg";
              href = tailnetUrl finance.combined.hedgerTailnet;
              description = "Combined ledger dashboard";
              siteMonitor = "${localUrl finance.combined.hedger}/api/v1/health/ready";
            };
          }
          {
            "Hedger Dummy" = {
              icon = "${tailnetUrl finance.dummy.hedgerTailnet}/icon.svg";
              href = tailnetUrl finance.dummy.hedgerTailnet;
              description = "Dummy ledger dashboard";
              siteMonitor = "${localUrl finance.dummy.hedger}/api/v1/health/ready";
            };
          }
          {
            "Paisa Mine" = {
              icon = "/icons/paisa.svg";
              href = tailnetUrl finance.mine.paisaTailnet;
              description = "Mine finance dashboard";
              siteMonitor = localUrl finance.mine.paisa;
            };
          }
        ];
      }
      {
        Books = [
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
            Seerr = {
              icon = "jellyseerr.png";
              href = tailnetUrl services.seerr.tailnet;
              description = "Media requests";
              siteMonitor = localUrl services.seerr.local;
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
        System = [
          {
            Grafana = {
              icon = "grafana.png";
              href = tailnetUrl services.observability.grafana.tailnet;
              description = "Metrics, logs, and traces";
              siteMonitor = "${localUrl services.observability.grafana.local}/api/health";
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
            Prowlarr = {
              icon = "prowlarr.png";
              href = tailnetUrl services.prowlarr.tailnet;
              description = "Indexer management";
              siteMonitor = localUrl services.prowlarr.local;
            };
          }
        ];
      }
    ];
  };
}
