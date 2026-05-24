{ pkgs, ... }:

let
  tailnetHost = "srt-n01-rivendell.taila65e7f.ts.net";
  certDir = "/var/lib/caddy/tailscale-certs";
  certFile = "${certDir}/${tailnetHost}.crt";
  keyFile = "${certDir}/${tailnetHost}.key";

  tlsConfig = ''
    tls ${certFile} ${keyFile}
  '';

  proxy = port: ''
    ${tlsConfig}
    reverse_proxy 127.0.0.1:${toString port}
  '';
in
{
  services.caddy = {
    enable = true;
    openFirewall = false;
    virtualHosts = {
      "${tailnetHost}".extraConfig = proxy 8082;
      "https://${tailnetHost}:9443".extraConfig = proxy 8096;
      "https://${tailnetHost}:9444".extraConfig = proxy 5055;
      "https://${tailnetHost}:9445".extraConfig = proxy 8080;
      "https://${tailnetHost}:9446".extraConfig = proxy 7878;
      "https://${tailnetHost}:9447".extraConfig = proxy 9696;
      "https://${tailnetHost}:9448".extraConfig = proxy 8081;
      "https://${tailnetHost}:9449".extraConfig = proxy 8989;
    };
  };

  systemd = {
    services = {
      caddy = {
        after = [ "tailscale-cert-rivendell.service" ];
        requires = [ "tailscale-cert-rivendell.service" ];
      };

      tailscale-cert-rivendell = {
        description = "Fetch Tailscale TLS certificate for Rivendell";
        after = [
          "network-online.target"
          "tailscaled.service"
        ];
        wants = [ "network-online.target" ];
        path = [
          pkgs.coreutils
          pkgs.systemd
          pkgs.tailscale
        ];
        serviceConfig = {
          Type = "oneshot";
          StateDirectory = "caddy/tailscale-certs";
          StateDirectoryMode = "0755";
          UMask = "0077";
        };
        script = ''
          tailscale cert \
            --cert-file ${certFile} \
            --key-file ${keyFile} \
            ${tailnetHost}

          chown caddy:caddy ${certFile} ${keyFile}
          chmod 0640 ${certFile} ${keyFile}
        '';
      };
    };

    timers.tailscale-cert-rivendell = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "daily";
        RandomizedDelaySec = "1h";
        Persistent = true;
      };
    };
  };
}
