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
              href = "http://srt-n01-rivendell:8096";
              description = "Movies and TV";
              siteMonitor = "http://127.0.0.1:8096";
            };
          }
          {
            Seerr = {
              icon = "jellyseerr.png";
              href = "http://srt-n01-rivendell:5055";
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
              href = "http://srt-n01-rivendell:8080";
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
              href = "http://srt-n01-rivendell:7878";
              description = "Movie automation";
              siteMonitor = "http://127.0.0.1:7878";
            };
          }
          {
            Prowlarr = {
              icon = "prowlarr.png";
              href = "http://srt-n01-rivendell:9696";
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
              href = "http://srt-n01-rivendell:8081";
              description = "DNS and ad blocking";
              siteMonitor = "http://127.0.0.1:8081";
            };
          }
        ];
      }
    ];
  };
}
