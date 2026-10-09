{
  config,
  homelab,
  lib,
  paths,
  pkgs,
  ...
}:

let
  onepacerr = pkgs.callPackage ../../pkgs/onepacerr { };
  ports = homelab.services;

  startOnepacerr = import ./start-onepacerr.nix {
    inherit pkgs onepacerr;
    torrentPassword = config.age.secrets.qbittorrentPassword.path;
    jellyfinPassword = config.age.secrets.onepacerrJellyfinPassword.path;
  };
in
{
  systemd.services = {
    onepacerr-media-permissions = {
      description = "Normalize One Pace shared media permissions";
      after = [ "media-directories.service" ];
      requires = [ "media-directories.service" ];
      before = [ "onepacerr.service" ];
      requiredBy = [ "onepacerr.service" ];
      unitConfig = {
        RequiresMountsFor = paths.mediaRoot;
        ConditionPathIsMountPoint = paths.mediaRoot;
      };
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
      script = ''
        one_pace_dir="${paths.library.shows}/One Pace"
        if [ -d "$one_pace_dir" ]; then
          ${lib.getExe' pkgs.coreutils "chgrp"} -R media "$one_pace_dir"
          ${lib.getExe' pkgs.coreutils "chmod"} -R g+rwX "$one_pace_dir"
          ${lib.getExe pkgs.findutils} "$one_pace_dir" -type d -exec ${lib.getExe' pkgs.coreutils "chmod"} g+s {} +
        fi
      '';
    };

    onepacerr = {
      description = "One Pace automatic downloader and Jellyfin organizer";
      wantedBy = [ "multi-user.target" ];
      after = [
        "jellyfin.service"
        "network-online.target"
        "qbittorrent.service"
      ];
      wants = [
        "jellyfin.service"
        "network-online.target"
        "qbittorrent.service"
      ];

      environment = {
        TZ = "Asia/Kolkata";
        PORT = toString ports.onepacerr.local;
        npm_package_version = onepacerr.version;

        PIPELINE_SKIP_VERIFY_PRESENT_FILES = "true";
        PIPELINE_SKIP_ORGANIZE_PRESENT_FILES = "true";
        PIPELINE_SKIP_UPDATE_METADATA_PRESENT_FILES = "true";
        PIPELINE_SKIP_DOWNLOADS = "false";
        PIPELINE_SKIP_DOWNLOADS_IMPORTS = "false";
        PIPELINE_INCLUDE_SPECIALS = "true";
        PIPELINE_PREFER_EXTENDED = "true";
        PIPELINE_PREFER_ALTERNATE = "true";

        # Poll for metadata changes at most once a day instead of holding a
        # websocket open and reacting to every upstream push. Each poll only
        # runs the pipeline when the metadata's lastUpdate is newer.
        METADATA_DISABLE_WEBSOCKET = "true";
        METADATA_CHECK_INTERVAL = "86400";

        LIBRARY_MEDIA_SERVER = "jellyfin";
        LIBRARY_SERIES_NAME = "One Pace";
        LIBRARY_CREATE_SHOW_IF_NOT_FOUND = "true";
        LIBRARY_USE_HARDLINKS = "true";

        JELLYFIN_URL = "http://127.0.0.1:${toString ports.jellyfin.local}";
        JELLYFIN_USERNAME = "onepacerr";
        JELLYFIN_LIBRARY_NAME = "Shows";

        TORRENT_CLIENT = "qbittorrent";
        TORRENT_URL = "http://127.0.0.1:${toString ports.qbittorrent.syncApi}";
        TORRENT_USER = "admin";
        TORRENT_CATEGORY = "onepacerr";
        TORRENT_CATEGORY_ONCE_COMPLETED = "completed";
        TORRENT_CHECK_INTERVAL = "60";
      };

      serviceConfig = {
        Type = "simple";
        User = "qbittorrent";
        Group = "media";
        SupplementaryGroups = [ "arr-secrets" ];
        ExecStart = startOnepacerr;
        Restart = "always";
        RestartSec = "15s";
        UMask = "0002";

        CapabilityBoundingSet = "";
        LockPersonality = true;
        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateTmp = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHome = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectSystem = "strict";
        ReadWritePaths = [ paths.mediaRoot ];
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_NETLINK"
          "AF_UNIX"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        SystemCallArchitectures = "native";
      };
    };
  };
}
