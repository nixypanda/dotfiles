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

    calco = {
      local = 3002;
      tailnet = 9464;
    };

    kavita = {
      local = 5000;
      tailnet = 9465;
    };
  };

  finance = {
    mine = {
      hledger = 5001;
      hledgerTailnet = 9450;
      paisa = 5101;
      paisaTailnet = 9460;
    };

    wife = {
      hledger = 5002;
      hledgerTailnet = 9451;
      paisa = 5102;
      paisaTailnet = 9461;
    };

    combined = {
      hledger = 5003;
      hledgerTailnet = 9452;
      paisa = 5103;
      paisaTailnet = 9462;
    };

    dummy = {
      hledger = 5004;
      hledgerTailnet = 9453;
      paisa = 5104;
      paisaTailnet = 9463;
    };
  };
}
