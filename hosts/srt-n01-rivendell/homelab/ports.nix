{
  tailnetHost = "srt-n01-rivendell.taila65e7f.ts.net";

  firewall = {
    ssh = 22;
  };

  services = {
    homepage = {
      local = 8082;
    };

    jellyfin = {
      local = 8096;
      tailnet = 9443;
    };

    seerr = {
      local = 5055;
      tailnet = 9444;
    };

    qbittorrent = {
      webui = 8080;
      syncApi = 8085;
      peer = 58181;
      tailnet = 9445;
    };

    radarr = {
      local = 7878;
      tailnet = 9446;
    };

    prowlarr = {
      local = 9696;
      tailnet = 9447;
    };

    pihole = {
      web = 8081;
      tailnet = 9448;
    };

    sonarr = {
      local = 8989;
      tailnet = 9449;
    };

    onepacerr = {
      local = 3010;
    };

    calco = {
      local = 3002;
      tailnet = 9464;
    };

    hedger = {
      local = 8083;
    };

    kavita = {
      local = 5000;
      tailnet = 9465;
    };

    audiobookshelf = {
      local = 8000;
      tailnet = 9466;
    };

    shelfmark = {
      local = 8084;
      tailnet = 9467;
    };

    observability = {
      grafana = {
        local = 3000;
        tailnet = 9468;
      };
      loki = {
        http = 3100;
        grpc = 9096;
      };
      tempo = {
        http = 3200;
        grpc = 9095;
        otlpGrpc = 14317;
        otlpHttp = 14318;
      };
      prometheus.local = 9090;
      otel = {
        otlpGrpc = 4317;
        otlpHttp = 4318;
        metrics = 8889;
        telemetry = 8888;
      };
      systemdExporter.local = 9558;
    };
  };

  finance = {
    mine = {
      hedger = 5001;
      hedgerTailnet = 9450;
      paisa = 5101;
      paisaTailnet = 9460;
    };

    wife = {
      hedger = 5002;
      hedgerTailnet = 9451;
      paisa = 5102;
      paisaTailnet = 9461;
    };

    combined = {
      hedger = 5003;
      hedgerTailnet = 9452;
      paisa = 5103;
      paisaTailnet = 9462;
    };

    dummy = {
      hedger = 5004;
      hedgerTailnet = 9453;
      paisa = 5104;
      paisaTailnet = 9463;
    };
  };
}
