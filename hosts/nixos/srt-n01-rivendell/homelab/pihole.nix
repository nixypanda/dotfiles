_: {
  services = {
    pihole-ftl = {
      enable = true;
      openFirewallDNS = true;
      openFirewallWebserver = true;
      queryLogDeleter.enable = true;
      settings = {
        dns = {
          domainNeeded = true;
          expandHosts = true;
          listeningMode = "ALL";
          upstreams = [
            "1.1.1.1"
            "1.0.0.1"
            "9.9.9.9"
          ];
          hosts = [
            "192.168.1.76 srt-n01-rivendell rivendell"
            "192.168.1.76 jellyfin"
            "192.168.1.76 radarr"
            "192.168.1.76 prowlarr"
            "192.168.1.76 qbittorrent"
            "192.168.1.76 jellyseerr seerr"
          ];
        };
      };
    };

    pihole-web = {
      enable = true;
      hostName = "pi.hole";
      ports = [ 8081 ];
    };
  };
}
