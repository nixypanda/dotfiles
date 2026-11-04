{
  homelab,
  lib,
  paths,
  pkgs,
  ...
}:

let
  ports = homelab.services;
  package = pkgs.callPackage ../pkgs/metube { };

  stateDir = paths.state.metube;
  downloadDir = paths.downloads.metube;
  audioDir = "${downloadDir}/audio";
  tempDir = "${downloadDir}/.tmp";
in
{
  users.users.metube = {
    isSystemUser = true;
    group = "media";
    home = stateDir;
    createHome = false;
    shell = "${pkgs.shadow}/bin/nologin";
  };

  systemd.tmpfiles.rules = [
    "d ${stateDir} 0700 metube media - -"
    "d ${downloadDir} 2775 metube media - -"
    "d ${audioDir} 2775 metube media - -"
    "d ${tempDir} 0700 metube media - -"
  ];

  systemd.services.metube = {
    description = "MeTube yt-dlp web interface";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    # MeTube already adds the audio extraction, metadata, and embedded-cover
    # postprocessors for its audio modes, so do not set YTDL_OPTIONS or add a
    # global writethumbnail (that would disable MeTube's own chain). These
    # templates group playlist downloads into album directories while keeping
    # loose singles unique with the yt-dlp video id.
    environment = {
      DOWNLOAD_DIR = downloadDir;
      AUDIO_DOWNLOAD_DIR = audioDir;
      STATE_DIR = stateDir;
      TEMP_DIR = tempDir;
      HOST = "127.0.0.1";
      PORT = toString ports.metube.local;
      MAX_CONCURRENT_DOWNLOADS = "2";
      OUTPUT_TEMPLATE = "%(title)s [%(id)s].%(ext)s";
      OUTPUT_TEMPLATE_CHANNEL = "%(title)s [%(id)s].%(ext)s";
      OUTPUT_TEMPLATE_PLAYLIST = "%(playlist_title)s [%(playlist_id)s]/%(playlist_index)02d - %(title)s.%(ext)s";
    };
    serviceConfig = {
      Type = "simple";
      User = "metube";
      Group = "media";
      ExecStart = lib.getExe package;
      Restart = "always";
      RestartSec = "10s";
      UMask = "0007";
      ReadWritePaths = [
        stateDir
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
