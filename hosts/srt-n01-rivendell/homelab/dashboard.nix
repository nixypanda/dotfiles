{ pkgs, ... }:
let
  tailnetHost = "srt-n01-rivendell.taila65e7f.ts.net";

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
    listenPort = 8082;
    openFirewall = false;
    allowedHosts = builtins.concatStringsSep "," [
      "localhost:8082"
      "127.0.0.1:8082"
      "srt-n01-rivendell:8082"
      "100.127.3.54:8082"
      tailnetHost
      "${tailnetHost}:8082"
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
          network = "wlp0s20f0u4i2";
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
        Media = [
          {
            Jellyfin = {
              icon = "jellyfin.png";
              href = "https://${tailnetHost}:9443";
              description = "Movies, TV, and anime";
              siteMonitor = "http://127.0.0.1:8096";
            };
          }
          {
            Seerr = {
              icon = "jellyseerr.png";
              href = "https://${tailnetHost}:9444";
              description = "Media requests";
              siteMonitor = "http://127.0.0.1:5055";
            };
          }
        ];
      }
      {
        Downloads = [
          {
            qBittorrent = {
              icon = "qbittorrent.png";
              href = "https://${tailnetHost}:9445";
              description = "Torrent client";
              siteMonitor = "http://127.0.0.1:8080";
            };
          }
        ];
      }
      {
        Automation = [
          {
            Radarr = {
              icon = "radarr.png";
              href = "https://${tailnetHost}:9446";
              description = "Movie automation";
              siteMonitor = "http://127.0.0.1:7878";
            };
          }
          {
            Sonarr = {
              icon = "sonarr.png";
              href = "https://${tailnetHost}:9449";
              description = "TV and anime automation";
              siteMonitor = "http://127.0.0.1:8989";
            };
          }
          {
            Prowlarr = {
              icon = "prowlarr.png";
              href = "https://${tailnetHost}:9447";
              description = "Indexer management";
              siteMonitor = "http://127.0.0.1:9696";
            };
          }
        ];
      }
      {
        Network = [
          {
            "Pi-hole" = {
              icon = "pi-hole.png";
              href = "https://${tailnetHost}:9448";
              description = "DNS and ad blocking";
              siteMonitor = "http://127.0.0.1:8081";
            };
          }
        ];
      }
      {
        Finance = [
          {
            "hledger Mine" = {
              icon = "hledger.png";
              href = "https://${tailnetHost}:9450";
              description = "Mine ledger API";
              siteMonitor = "http://127.0.0.1:5001";
            };
          }
          {
            "hledger Wife" = {
              icon = "hledger.png";
              href = "https://${tailnetHost}:9451";
              description = "Wife ledger API";
              siteMonitor = "http://127.0.0.1:5002";
            };
          }
          {
            "hledger Combined" = {
              icon = "hledger.png";
              href = "https://${tailnetHost}:9452";
              description = "Combined ledger API";
              siteMonitor = "http://127.0.0.1:5003";
            };
          }
          {
            "hledger Dummy" = {
              icon = "hledger.png";
              href = "https://${tailnetHost}:9453";
              description = "Dummy ledger API";
              siteMonitor = "http://127.0.0.1:5004";
            };
          }
          {
            "Paisa Mine" = {
              icon = "/icons/paisa.svg";
              href = "https://${tailnetHost}:9460";
              description = "Mine finance dashboard";
              siteMonitor = "http://127.0.0.1:5101";
            };
          }
          {
            "Paisa Wife" = {
              icon = "/icons/paisa.svg";
              href = "https://${tailnetHost}:9461";
              description = "Wife finance dashboard";
              siteMonitor = "http://127.0.0.1:5102";
            };
          }
          {
            "Paisa Combined" = {
              icon = "/icons/paisa.svg";
              href = "https://${tailnetHost}:9462";
              description = "Combined finance dashboard";
              siteMonitor = "http://127.0.0.1:5103";
            };
          }
          {
            "Paisa Dummy" = {
              icon = "/icons/paisa.svg";
              href = "https://${tailnetHost}:9463";
              description = "Dummy finance dashboard";
              siteMonitor = "http://127.0.0.1:5104";
            };
          }
        ];
      }
      {
        Tracking = [
          {
            "CalCo" = {
              icon = "https://${tailnetHost}:9464/favicon-32x32.png";
              href = "https://${tailnetHost}:9464";
              description = "Food and nutrition tracker";
              siteMonitor = "http://127.0.0.1:3002/api/health";
            };
          }
        ];
      }
    ];
  };
}
