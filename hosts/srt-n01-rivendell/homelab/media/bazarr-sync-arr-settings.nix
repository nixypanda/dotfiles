{
  pkgs,
  stateDir,
  sonarrPort,
  radarrPort,
  bazarrPort,
}:
pkgs.writers.writePython3Bin "bazarr-sync-arr-settings" { } ''
  import pathlib
  import urllib.parse
  import urllib.request


  secrets_dir = pathlib.Path("${stateDir}/secrets")


  def read_secret(name):
      return (secrets_dir / name).read_text(encoding="utf-8").strip()


  settings = {
      "settings-general-use_sonarr": "true",
      "settings-sonarr-ip": "127.0.0.1",
      "settings-sonarr-port": "${toString sonarrPort}",
      "settings-sonarr-base_url": "",
      "settings-sonarr-ssl": "false",
      "settings-sonarr-apikey": read_secret("sonarr.api-key"),
      "settings-sonarr-only_monitored": "true",
      "settings-sonarr-series_sync": "60",
      "settings-sonarr-episodes_sync": "60",
      "settings-general-use_radarr": "true",
      "settings-radarr-ip": "127.0.0.1",
      "settings-radarr-port": "${toString radarrPort}",
      "settings-radarr-base_url": "",
      "settings-radarr-ssl": "false",
      "settings-radarr-apikey": read_secret("radarr.api-key"),
      "settings-radarr-only_monitored": "true",
      "settings-radarr-movies_sync": "60",
  }
  request = urllib.request.Request(
      "http://127.0.0.1:${toString bazarrPort}/api/system/settings",
      data=urllib.parse.urlencode(settings).encode("utf-8"),
      headers={
          "Content-Type": "application/x-www-form-urlencoded",
          "X-API-KEY": read_secret("bazarr.api-key"),
      },
      method="POST",
  )
  with urllib.request.urlopen(request, timeout=30) as response:
      if not 200 <= response.status < 300:
          message = f"Bazarr settings sync failed: HTTP {response.status}"
          raise RuntimeError(message)
''
