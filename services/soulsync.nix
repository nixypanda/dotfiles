{
  homelab,
  paths,
  pkgs,
  ...
}:

let
  ports = homelab.services;

  # The package is owned by pkgs/soulsync. This service intentionally does not
  # build or modify it; it only consumes the contract documented there:
  #   - bin/soulsync launches Gunicorn
  #   - Gunicorn accepts the standard --bind option
  package = pkgs.callPackage ../pkgs/soulsync { };

  stateDir = paths.state.soulsync;
  outputDir = paths.library.musicSoulSync;
  downloadDir = paths.downloads.soulsync;
  stagingDir = "${stateDir}/staging";
  musicVideosDir = "${stateDir}/music-videos";
  streamDir = "${stateDir}/Stream";
  playlistsDir = "${stateDir}/playlists";
  samplesDir = "${stateDir}/samples";
  imageCacheDir = "${stateDir}/image-cache";
  numbaCacheDir = "${stateDir}/numba-cache";
  playlistCoverDir = "${stateDir}/playlist-covers";
  avatarDir = "${stateDir}/avatars";
  podcastsDir = "${stateDir}/podcasts";
  audiobooksDir = "${stateDir}/audiobooks";

  localPort = ports.soulsync.local;

  seedConfig = pkgs.writeShellScript "soulsync-seed-config" ''
        set -eu

        install -d -m 0700 "${stateDir}" "${stateDir}/config" "${stateDir}/data" \
          "${stateDir}/database" "${stateDir}/logs" "${stateDir}/storage" "${imageCacheDir}" \
          "${numbaCacheDir}" "${playlistCoverDir}" "${avatarDir}" "${stagingDir}" \
          "${playlistsDir}" "${samplesDir}" "${streamDir}" "${musicVideosDir}" \
          "${podcastsDir}" "${audiobooksDir}"

        config_file="${stateDir}/config/config.json"

        # Seed from the upstream example exactly once. Never overwrite a config
        # that already exists, including one created by a previous run. The
        # package is expected to ship config/config.example.json (or the
        # share/soulsync path); fail loudly instead of starting with unsafe
        # defaults if none of those templates exists.
        if [ ! -f "$config_file" ]; then
          seeded=0
          for candidate in \
            "${package}/config/config.example.json" \
            "${package}/share/soulsync/config/config.example.json" \
            "${package}/share/soulsync/config.example.json"; do
            if [ -f "$candidate" ]; then
              # The store template is read-only; install with a writable mode
              # because the patch step below rewrites this file.
              install -m 0600 "$candidate" "$config_file"
              seeded=1
              break
            fi
          done
          if [ "$seeded" -ne 1 ]; then
            echo "soulsync: no config template found in ${package}; refusing to start with defaults" >&2
            exit 1
          fi
        fi

        # Patch the filesystem locations used by this deployment in the seed
        # config. Upstream uses its database as the primary configuration store
        # after first boot, so these values seed the initial migration and the
        # JSON fallback; UI edits may persist in the database and are not
        # forcibly reset on restart. The systemd sandbox confines persistent
        # writes to the state, download, and isolated output directories.
        if [ -f "$config_file" ]; then
          ${pkgs.python3}/bin/python3 - "$config_file" <<'PY'
    import json
    import sys

    path = sys.argv[1]
    with open(path, encoding="utf-8") as handle:
        config = json.load(handle)

    config.setdefault("soulseek", {})
    config["soulseek"]["download_path"] = "${downloadDir}"
    config["soulseek"]["transfer_path"] = "${outputDir}"

    config.setdefault("import", {})
    config["import"]["staging_path"] = "${stagingDir}"

    config.setdefault("library", {})
    config["library"]["music_paths"] = ["${outputDir}"]
    config["library"]["music_videos_path"] = "${musicVideosDir}"
    config["library"]["podcasts_path"] = "${podcastsDir}"
    config["library"]["audiobooks_path"] = "${audiobooksDir}"
    config["library"]["sample_paths"] = ["${samplesDir}"]

    config.setdefault("playlists", {})
    config["playlists"]["materialize_path"] = "${playlistsDir}"

    config.setdefault("image_cache", {})
    config["image_cache"]["path"] = "${imageCacheDir}"

    config.setdefault("logging", {})
    config["logging"]["path"] = "${stateDir}/logs/app.log"

    with open(path, "w", encoding="utf-8") as handle:
        json.dump(config, handle, indent=2)
        handle.write("\n")
    PY
        fi
  '';
in
{
  users.users.soulsync = {
    isSystemUser = true;
    group = "media";
    home = stateDir;
    createHome = false;
    shell = "${pkgs.shadow}/bin/nologin";
  };

  systemd.tmpfiles.rules = [
    "d ${stateDir} 0700 soulsync media - -"
    "d ${stateDir}/config 0700 soulsync media - -"
    "d ${stateDir}/data 0700 soulsync media - -"
    "d ${stateDir}/database 0700 soulsync media - -"
    "d ${stateDir}/logs 0700 soulsync media - -"
    "d ${stateDir}/storage 0700 soulsync media - -"
    "d ${imageCacheDir} 0700 soulsync media - -"
    "d ${numbaCacheDir} 0700 soulsync media - -"
    "d ${playlistCoverDir} 0700 soulsync media - -"
    "d ${avatarDir} 0700 soulsync media - -"
    "d ${stagingDir} 0700 soulsync media - -"
    "d ${playlistsDir} 0700 soulsync media - -"
    "d ${samplesDir} 0700 soulsync media - -"
    "d ${streamDir} 0700 soulsync media - -"
    "d ${musicVideosDir} 0700 soulsync media - -"
    "d ${podcastsDir} 0700 soulsync media - -"
    "d ${audiobooksDir} 0700 soulsync media - -"
    "d ${outputDir} 2775 soulsync media - -"
    "d ${downloadDir} 2775 soulsync media - -"
  ];

  systemd.services.soulsync = {
    description = "SoulSync music discovery and collection manager";
    wantedBy = [ "multi-user.target" ];
    wants = [
      "network-online.target"
      "systemd-tmpfiles-setup.service"
    ];
    after = [
      "network-online.target"
      "systemd-tmpfiles-setup.service"
    ];

    environment = {
      PYTHONUNBUFFERED = "1";
      HOME = stateDir;
      NUMBA_CACHE_DIR = numbaCacheDir;
      SOULSYNC_NO_FFMPEG_DOWNLOAD = "1";
      SOULSYNC_STATE_DIR = stateDir;
      SOULSYNC_PRESERVE_CWD = "1";
      # Package patch replaces base_root with this value and upstream then
      # appends "Stream"; point it at stateDir so the actual folder is
      # stateDir/Stream, not stateDir/Stream/Stream.
      SOULSYNC_STREAM_DIR = stateDir;
      SOULSYNC_SCRIPTS_DIR = "${stateDir}/scripts";
      SOULSYNC_CONFIG_PATH = "${stateDir}/config/config.json";
      DATABASE_PATH = "${stateDir}/data/music_library.db";
      VIDEO_DATABASE_PATH = "${stateDir}/data/video_library.db";
      AUDIOBOOK_DATABASE_PATH = "${stateDir}/data/audiobooks.db";
      SOULSYNC_LOG_FILE = "${stateDir}/logs/app.log";
      SOULSYNC_IMAGE_CACHE_DIR = imageCacheDir;
      SOULSYNC_PLAYLIST_COVER_DIR = playlistCoverDir;
      SOULSYNC_AVATAR_DIR = avatarDir;
      SOULSYNC_WEB_BIND_PORT = toString localPort;
      TZ = "UTC";
    };

    serviceConfig = {
      Type = "simple";
      User = "soulsync";
      Group = "media";
      WorkingDirectory = stateDir;
      UMask = "0007";
      ExecStartPre = seedConfig;
      ExecStart = "${package}/bin/soulsync --bind 127.0.0.1:${toString localPort}";
      Restart = "always";
      RestartSec = "10s";

      ReadWritePaths = [
        stateDir
        outputDir
        downloadDir
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
