{
  config,
  homelab,
  pkgs,
  ...
}:

let
  inherit (homelab) tailnetHost;
  inherit (homelab) finance services;

  calcoWebRoot = "${config.services.calco.frontendPackage}/share/calco/web";
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

  tailnetUrl = port: "https://${tailnetHost}:${toString port}";

  proxiedHosts = {
    "${tailnetHost}".extraConfig = proxy services.homepage.local;
    "${tailnetUrl services.jellyfin.tailnet}".extraConfig = proxy services.jellyfin.local;
    "${tailnetUrl services.seerr.tailnet}".extraConfig = proxy services.seerr.local;
    "${tailnetUrl services.qbittorrent.tailnet}".extraConfig = proxy services.qbittorrent.webui;
    "${tailnetUrl services.radarr.tailnet}".extraConfig = proxy services.radarr.local;
    "${tailnetUrl services.prowlarr.tailnet}".extraConfig = proxy services.prowlarr.local;
    "${tailnetUrl services.pihole.tailnet}".extraConfig = proxy services.pihole.web;
    "${tailnetUrl services.sonarr.tailnet}".extraConfig = proxy services.sonarr.local;
    "${tailnetUrl services.kavita.tailnet}".extraConfig = proxy services.kavita.local;
  };

  financeHosts = builtins.listToAttrs (
    builtins.concatLists (
      builtins.attrValues (
        builtins.mapAttrs (_: instance: [
          {
            name = tailnetUrl instance.hledgerTailnet;
            value.extraConfig = proxy instance.hledger;
          }
          {
            name = tailnetUrl instance.paisaTailnet;
            value.extraConfig = proxy instance.paisa;
          }
        ]) finance
      )
    )
  );

  calcoHost = {
    "${tailnetUrl services.calco.tailnet}".extraConfig = ''
      ${tlsConfig}
      handle /api* {
        reverse_proxy 127.0.0.1:${toString services.calco.local}
      }
    '';
  };
in
{
  services.caddy = {
    enable = true;
    openFirewall = false;
    # Tailnet-facing ports are stable aliases for services listening on local
    # ports, which keeps the services bound to localhost while exposing HTTPS.
    virtualHosts = proxiedHosts // financeHosts // calcoHost;
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
