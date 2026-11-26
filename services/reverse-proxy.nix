{
  config,
  homelab,
  onepacerr-ui,
  pkgs,
  ...
}:

let
  inherit (homelab) tailnetHost tailnetUrl;
  inherit (homelab) finance services;
  system = pkgs.stdenv.hostPlatform.system;

  calcoWebRoot = "${config.services.calco.frontendPackage}/share/calco/web";
  onepacerrWebRoot = "${onepacerr-ui.packages.${system}.frontend}/share/onepacerr-ui";
  # Hedger's upstream service worker intercepts navigations, which prevents
  # browsers from showing Caddy's HTTP Basic Auth challenge. Keep registration
  # working, but do not intercept requests while the app is behind Basic Auth.
  hedgerServiceWorker = pkgs.writeTextDir "sw.js" ''
    self.addEventListener("install", () => self.skipWaiting());
    self.addEventListener("activate", (event) => event.waitUntil(self.clients.claim()));
  '';
  # Caddy serves this host only on the tailnet. Tailscale owns the certificate
  # lifecycle, so a systemd unit fetches cert files with `tailscale cert`.
  certDir = "/var/lib/caddy/tailscale-certs";
  certFile = "${certDir}/${tailnetHost}.crt";
  keyFile = "${certDir}/${tailnetHost}.key";
  financeAuthDir = "/var/lib/finance-auth";
  financeAuthFile = "${financeAuthDir}/caddy";

  tlsConfig = ''
    tls ${certFile} ${keyFile}
  '';

  proxy = port: ''
    ${tlsConfig}
    reverse_proxy 127.0.0.1:${toString port}
  '';

  hedgerProxy = hostName: ''
    ${tlsConfig}
    import ${financeAuthFile}
    handle /sw.js {
      root * ${hedgerServiceWorker}
      header Content-Type application/javascript
      header Cache-Control "no-cache"
      file_server
    }
    handle {
      reverse_proxy 127.0.0.1:${toString services.hedger.local} {
        header_up Host ${hostName}
      }
    }
  '';

  proxiedHosts = {
    "${tailnetHost}".extraConfig = proxy services.homepage.local;
    "${tailnetUrl services.bazarr.tailnet}".extraConfig = proxy services.bazarr.local;
    "${tailnetUrl services.jellyfin.tailnet}".extraConfig = proxy services.jellyfin.local;
    "${tailnetUrl services.lidarr.tailnet}".extraConfig = proxy services.lidarr.local;
    "${tailnetUrl services.soulsync.tailnet}".extraConfig = proxy services.soulsync.local;
    "${tailnetUrl services.homeAssistant.tailnet}".extraConfig = proxy services.homeAssistant.local;
    "${tailnetUrl services.navidrome.tailnet}".extraConfig = proxy services.navidrome.local;
    "${tailnetUrl services.seerr.tailnet}".extraConfig = proxy services.seerr.local;
    "${tailnetUrl services.qbittorrent.tailnet}".extraConfig = proxy services.qbittorrent.webui;
    "${tailnetUrl services.radarr.tailnet}".extraConfig = proxy services.radarr.local;
    "${tailnetUrl services.prowlarr.tailnet}".extraConfig = proxy services.prowlarr.local;
    "${tailnetUrl services.pihole.tailnet}".extraConfig = proxy services.pihole.web;
    "${tailnetUrl services.sonarr.tailnet}".extraConfig = proxy services.sonarr.local;
    "${tailnetUrl services.bookorbit.tailnet}".extraConfig = proxy services.bookorbit.local;
    "${tailnetUrl services.stremioLocalDebrid.tailnet}".extraConfig =
      proxy services.stremioLocalDebrid.local;
  };

  financeHosts = {
    "${tailnetUrl finance.mine.hedgerTailnet}".extraConfig =
      hedgerProxy config.services.hedger.instances.mine.hostName;

    "${tailnetUrl finance.mine.paisaTailnet}".extraConfig = ''
      ${tlsConfig}
      import ${financeAuthFile}
      reverse_proxy 127.0.0.1:${toString finance.mine.paisa}
    '';
  };

  calcoHost = {
    "${tailnetUrl services.calco.tailnet}".extraConfig = ''
      ${tlsConfig}
      handle /api* {
        reverse_proxy 127.0.0.1:${toString services.calco.local}
      }
      handle /assets/* {
        root * ${calcoWebRoot}
        header Cache-Control "public, max-age=31536000, immutable"
        file_server
      }
      handle /version.json {
        root * ${calcoWebRoot}
        header Cache-Control "no-store"
        file_server
      }
      handle /site.webmanifest {
        root * ${calcoWebRoot}
        header Cache-Control "no-cache"
        file_server
      }
      handle /index.html {
        root * ${calcoWebRoot}
        header Cache-Control "no-cache"
        file_server
      }
      handle {
        root * ${calcoWebRoot}
        header Cache-Control "no-cache"
        try_files {path} /index.html
        file_server
      }
    '';
  };

  onepacerrHost = {
    "${tailnetUrl services.onepacerr.tailnet}".extraConfig = ''
      ${tlsConfig}
      handle /api/* {
        uri replace /api/ /api/v1/
        reverse_proxy 127.0.0.1:${toString services.onepacerr.local}
      }
      handle /assets/* {
        root * ${onepacerrWebRoot}
        header Cache-Control "public, max-age=31536000, immutable"
        file_server
      }
      handle {
        root * ${onepacerrWebRoot}
        header Cache-Control "no-cache"
        try_files {path} /index.html
        file_server
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
    virtualHosts = proxiedHosts // financeHosts // calcoHost // onepacerrHost;
  };

  systemd = {
    services = {
      caddy = {
        # Caddy reads static cert files, so it must wait until the Tailscale cert
        # fetch has populated /var/lib/caddy/tailscale-certs.
        after = [
          "tailscale-cert-rivendell.service"
          "finance-auth.service"
        ];
        requires = [
          "tailscale-cert-rivendell.service"
          "finance-auth.service"
        ];
      };

      finance-auth = {
        description = "Generate Caddy login for Hedger and Paisa";
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          Group = "caddy";
          StateDirectory = "finance-auth";
          StateDirectoryMode = "0750";
          UMask = "0027";
        };
        script = ''
          password_file=${financeAuthDir}/password
          if [ ! -s "$password_file" ]; then
            umask 077
            ${pkgs.openssl}/bin/openssl rand -base64 36 > "$password_file"
          fi
          chmod 0600 "$password_file"

          hash=$(${pkgs.caddy}/bin/caddy hash-password --plaintext "$(cat "$password_file")")
          config_file=$(mktemp ${financeAuthDir}/caddy.XXXXXX)
          printf 'basic_auth {\n  finance %s\n}\n' "$hash" > "$config_file"
          chmod 0640 "$config_file"
          mv -f "$config_file" ${financeAuthFile}
        '';
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
