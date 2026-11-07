{
  homelab,
  lib,
  paths,
  pkgs,
  ...
}:

let
  inherit (homelab) tailnetHost;
  ports = homelab.services;

  package = pkgs.callPackage ../pkgs/stremio-local-debrid { };

  stateDir = "/var/lib/stremio-local-debrid";
  cacheDir = paths.cache.stremioDebrid;

  baseUrl = "https://${tailnetHost}:${toString ports.stremioLocalDebrid.tailnet}";

  # Torrentio's default manifest. Replace this with your configured Torrentio
  # manifest URL if you use custom providers, quality filters, or debrid.
  torrentioManifest = "https://torrentio.strem.fun/manifest.json";

  configTemplate = pkgs.writeText "stremio-local-debrid-config.json" (
    builtins.toJSON {
      host = "127.0.0.1";
      port = ports.stremioLocalDebrid.local;
      inherit baseUrl cacheDir stateDir;
      maxCacheBytes = 100 * 1024 * 1024 * 1024;
      minFreeBytes = 10 * 1024 * 1024 * 1024;
      prefetchEpisodes = 1;
      metadataSource = "https://v3-cinemeta.strem.io/manifest.json";
      autoDiscoverAddons = false;
      addonRefreshMs = 60000;
      preventSleep = false;
      language = "en-US";
      sources = [
        {
          name = "Torrentio";
          manifestUrl = torrentioManifest;
        }
      ];
    }
  );

  startScript = pkgs.writeShellScript "stremio-local-debrid-start" ''
    set -eu

    state_dir=${lib.escapeShellArg stateDir}
    cache_dir=${lib.escapeShellArg cacheDir}
    mkdir -p "$state_dir" "$cache_dir"

    token_file="$state_dir/token"
    if [ ! -s "$token_file" ]; then
      ${lib.getExe pkgs.openssl} rand -hex 24 > "$token_file"
      chmod 600 "$token_file"
    fi
    token="$(${lib.getExe' pkgs.coreutils "cat"} "$token_file")"

    ${lib.getExe pkgs.jq} --arg token "$token" \
      '. + { token: $token }' \
      ${configTemplate} > "$state_dir/config.json"
    chmod 600 "$state_dir/config.json"

    printf '%s\n' "${baseUrl}/$token/manifest.json" > "$state_dir/addon-url.txt"
    printf '%s\n' "${baseUrl}/$token/" > "$state_dir/status-url.txt"

    # Homepage reads these through EnvironmentFile. Keeping the token here
    # avoids putting the private status URL in the Nix store or the repo.
    {
      printf 'HOMEPAGE_VAR_STREMIO_LOCAL_DEBRID_URL=%s\n' "${baseUrl}/$token/"
      printf 'HOMEPAGE_VAR_STREMIO_LOCAL_DEBRID_MONITOR=%s\n' "http://127.0.0.1:${toString ports.stremioLocalDebrid.local}/$token/"
    } > "$state_dir/homepage.env"

    chmod 600 "$state_dir/addon-url.txt" "$state_dir/status-url.txt" "$state_dir/homepage.env"

    export STREMIO_LOCAL_DEBRID_CONFIG="$state_dir/config.json"
    exec ${lib.getExe pkgs.nodejs_24} ${package}/lib/node_modules/stremio-local-debrid/server.mjs
  '';
in
{
  users.users.stremio = {
    isSystemUser = true;
    group = "media";
    home = stateDir;
    createHome = false;
    shell = "${pkgs.shadow}/bin/nologin";
  };

  systemd.tmpfiles.rules = [
    "d ${stateDir} 0700 stremio media - -"
    "d ${cacheDir} 2775 stremio media - -"
    "f ${stateDir}/homepage.env 0600 stremio media - -"
  ];

  systemd.services.stremio-local-debrid = {
    description = "Stremio Local Debrid torrent cache addon";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    environment = {
      HOME = stateDir;
      NODE_ENV = "production";
    };
    serviceConfig = {
      Type = "simple";
      User = "stremio";
      Group = "media";
      ExecStart = startScript;
      Restart = "always";
      RestartSec = "10s";
      UMask = "0077";
      ReadWritePaths = [
        stateDir
        cacheDir
      ];
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      NoNewPrivileges = true;
      CapabilityBoundingSet = "";
      RestrictAddressFamilies = [
        "AF_INET"
        "AF_INET6"
        "AF_NETLINK"
        "AF_UNIX"
      ];
      SystemCallArchitectures = "native";
    };
  };
}
