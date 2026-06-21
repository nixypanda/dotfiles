{ config, pkgs, ... }:

let
  tailnetHost = "srt-n01-rivendell.taila65e7f.ts.net";
  # Caddy serves this host only on the tailnet. Tailscale owns the certificate
  # lifecycle, so a systemd unit fetches cert files with `tailscale cert`.
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
      # Tailnet-facing ports are stable aliases for services listening on local
      # ports, which keeps the services bound to localhost while exposing HTTPS.
      "${tailnetHost}".extraConfig = proxy 8082;
      "https://${tailnetHost}:9443".extraConfig = proxy 8096;
      "https://${tailnetHost}:9444".extraConfig = proxy 5055;
      "https://${tailnetHost}:9445".extraConfig = proxy 8080;
      "https://${tailnetHost}:9446".extraConfig = proxy 7878;
      "https://${tailnetHost}:9447".extraConfig = proxy 9696;
      "https://${tailnetHost}:9448".extraConfig = proxy 8081;
      "https://${tailnetHost}:9449".extraConfig = proxy 8989;
      "https://${tailnetHost}:9450".extraConfig = proxy 5001;
      "https://${tailnetHost}:9451".extraConfig = proxy 5002;
      "https://${tailnetHost}:9452".extraConfig = proxy 5003;
      "https://${tailnetHost}:9453".extraConfig = proxy 5004;
      "https://${tailnetHost}:9460".extraConfig = proxy 5101;
      "https://${tailnetHost}:9461".extraConfig = proxy 5102;
      "https://${tailnetHost}:9462".extraConfig = proxy 5103;
      "https://${tailnetHost}:9463".extraConfig = proxy 5104;
      "https://${tailnetHost}:9464".extraConfig = ''
        ${tlsConfig}
        handle /api* {
          reverse_proxy 127.0.0.1:3002
        }
        handle {
          root * ${config.services.calco.frontendPackage}/share/calco/web
          try_files {path} /index.html
          file_server
        }
      '';
      "https://${tailnetHost}:9465".extraConfig = proxy 5000;
    };
  };

  systemd = {
    services = {
      caddy = {
        # Caddy reads static cert files, so it must wait until the Tailscale cert
        # fetch has populated /var/lib/caddy/tailscale-certs.
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
          pkgs.jq
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
          # tailscaled.service can be active before Tailscale has a usable
          # netmap; wait for that before asking Tailscale for TLS certs.
          for _ in $(seq 1 30); do
            if tailscale status --json \
              | jq -e '.BackendState == "Running" and (.Self.Online // false)' \
              >/dev/null; then
              tailscale cert --cert-file ${certFile} --key-file ${keyFile} ${tailnetHost}

              chown caddy:caddy ${certFile} ${keyFile}
              chmod 0640 ${certFile} ${keyFile}
              exit 0
            fi

            sleep 2
          done

          echo "Tailscale did not become ready within 60 seconds"
          exit 1
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
