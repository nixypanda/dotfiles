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
        Finance = [
          {
            "hledger Mine" = {
              icon = "hledger.png";
              href = tailnetUrl finance.mine.hledgerTailnet;
              description = "Mine ledger API";
              siteMonitor = localUrl finance.mine.hledger;
            };
          }
          {
            "hledger Wife" = {
              icon = "hledger.png";
              href = tailnetUrl finance.wife.hledgerTailnet;
              description = "Wife ledger API";
              siteMonitor = localUrl finance.wife.hledger;
            };
          }
          {
            "hledger Combined" = {
              icon = "hledger.png";
              href = tailnetUrl finance.combined.hledgerTailnet;
              description = "Combined ledger API";
              siteMonitor = localUrl finance.combined.hledger;
            };
          }
          {
            "hledger Dummy" = {
              icon = "hledger.png";
              href = tailnetUrl finance.dummy.hledgerTailnet;
              description = "Dummy ledger API";
              siteMonitor = localUrl finance.dummy.hledger;
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
          {
            "Paisa Wife" = {
              icon = "/icons/paisa.svg";
              href = tailnetUrl finance.wife.paisaTailnet;
              description = "Wife finance dashboard";
              siteMonitor = localUrl finance.wife.paisa;
            };
          }
          {
            "Paisa Combined" = {
              icon = "/icons/paisa.svg";
              href = tailnetUrl finance.combined.paisaTailnet;
              description = "Combined finance dashboard";
              siteMonitor = localUrl finance.combined.paisa;
            };
          }
          {
            "Paisa Dummy" = {
              icon = "/icons/paisa.svg";
              href = tailnetUrl finance.dummy.paisaTailnet;
              description = "Dummy finance dashboard";
              siteMonitor = localUrl finance.dummy.paisa;
            };
          }
        ];
      }
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
          {
            Kavita = {
              icon = "kavita.png";
              href = tailnetUrl services.kavita.tailnet;
              description = "Books and manga";
              siteMonitor = localUrl services.kavita.local;
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
