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
  stateDir = "/var/lib/stremio-local-debrid";
in
{
  services.stremio-local-debrid = {
    enable = true;
    user = "stremio";
    group = "media";
    inherit stateDir;
    cacheDir = paths.cache.stremioDebrid;
    openFirewall = false;

    settings = {
      host = "127.0.0.1";
      port = ports.stremioLocalDebrid.local;
      baseUrl = "https://${tailnetHost}:${toString ports.stremioLocalDebrid.tailnet}";
      maxCacheBytes = 100 * 1024 * 1024 * 1024;
      minFreeBytes = 10 * 1024 * 1024 * 1024;
      prefetchEpisodes = 1;
      metadataSource = "https://v3-cinemeta.strem.io/manifest.json";
      autoDiscoverAddons = false;
      addonRefreshMs = 60000;
      preventSleep = false;
      # Credential-bearing source URLs belong in a private runtime configFile.
      sources = [
        {
          name = "Torrentio";
          manifestUrl = "https://torrentio.strem.fun/manifest.json";
        }
      ];
    };
  };

  # Keep the environment file available even before the first successful start.
  systemd.tmpfiles.rules = [ "f ${stateDir}/homepage.env 0600 stremio media - -" ];

  # Upstream prepares the private config and preserves the existing token first.
  # Only this Homepage-specific adapter remains local; no secret enters the store.
  systemd.services.stremio-local-debrid.preStart = lib.mkAfter ''
    umask 077
    homepage_env=${lib.escapeShellArg "${stateDir}/homepage.env"}
    trap 'rm -f "$homepage_env.tmp"' EXIT
    ${lib.getExe pkgs.jq} -r \
      --arg monitor "http://127.0.0.1:${toString ports.stremioLocalDebrid.local}" \
      '"HOMEPAGE_VAR_STREMIO_LOCAL_DEBRID_URL=\(.baseUrl | rtrimstr("/"))/\(.token)/",
       "HOMEPAGE_VAR_STREMIO_LOCAL_DEBRID_MONITOR=\($monitor)/\(.token)/"' \
      "$STREMIO_LOCAL_DEBRID_CONFIG" > "$homepage_env.tmp"
    chmod 600 "$homepage_env.tmp"
    mv -f "$homepage_env.tmp" "$homepage_env"
    trap - EXIT
  '';
}
