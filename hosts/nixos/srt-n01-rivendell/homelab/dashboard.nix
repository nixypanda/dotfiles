let
  tailnetHost = "srt-n01-rivendell.taila65e7f.ts.net";
in
_: {
  services.homepage-dashboard = {
    enable = true;
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
    ];
  };
}
