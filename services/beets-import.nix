{
  lib,
  paths,
  pkgs,
  ...
}:

let
  audioInbox = "${paths.downloads.metube}/audio";
  reviewDir = paths.downloads.metubeReview;
  beetsLibrary = paths.library.musicBeets;
  beetsState = paths.state.beets;
  beetsConfig = "/etc/beets/config.yaml";

  beetsImport = pkgs.writeShellApplication {
    name = "beets-import";
    runtimeInputs = with pkgs; [
      beets
      coreutils
      ffmpeg
      findutils
      util-linux
    ];
    text = builtins.readFile ./beets-import.sh;
    meta.description = "Import finished MeTube audio downloads with Beets";
  };
in
{
  users.users.beets = {
    isSystemUser = true;
    group = "media";
    home = beetsState;
    createHome = false;
    shell = "${pkgs.shadow}/bin/nologin";
  };

  # Beets 2.14's rewrite plugin keys are "<field> <pattern>" with a literal
  # replacement and no capture expansion, so the requested
  # "artist: '^(.+?) - Topic$: $1'" rule is invalid and cannot strip a
  # captured " - Topic" suffix. The youtube_clean plugin handles those
  # YouTube-specific fields instead.
  environment.etc = {
    "beets/config.yaml".text = ''
      library: ${beetsState}/library.db
      directory: ${beetsLibrary}
      pluginpath:
        - /etc/beets/plugins
      plugins:
        - youtube_clean
        - musicbrainz
      import:
        write: yes
        move: yes
        copy: no
        quiet_fallback: asis
        none_rec_action: asis
        autotag: yes
        timid: no
        resume: no
        incremental: no
        duplicate_action: skip
        duplicate_verbose_prompt: no
        log: ${beetsState}/import.log
      paths:
        default: $albumartist/$album%aunique{}/$track $title
        singleton: '%if{$album,%if{$albumartist,$albumartist,%if{$artist,$artist,Unknown Artist}}/$album/%if{$track,$track,00} $title,Singles/$artist/$title}'
        comp: Compilations/$album%aunique{}/$track $title
    '';

    "beets/plugins/youtube_clean.py".text = builtins.readFile ./beets-youtube-clean.py;
  };

  systemd = {
    tmpfiles.rules = [
      "d ${beetsState} 0700 beets media - -"
      "d ${beetsLibrary} 2775 beets media - -"
      "d ${reviewDir} 2775 beets media - -"
    ];

    services.beets-import = {
      description = "Import MeTube audio downloads into the Beets library";
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      environment = {
        BEETS_INBOX = audioInbox;
        BEETS_REVIEW = reviewDir;
        BEETS_STATE = beetsState;
        BEETS_CONFIG = beetsConfig;
        HOME = beetsState;
      };
      serviceConfig = {
        Type = "oneshot";
        User = "beets";
        Group = "media";
        UMask = "0002";
        ExecStart = lib.getExe beetsImport;
        TimeoutStartSec = "2h";
        Nice = 10;
        IOSchedulingClass = "idle";
        Restart = "no";
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        NoNewPrivileges = true;
        CapabilityBoundingSet = "";
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_UNIX"
          "AF_NETLINK"
        ];
        SystemCallArchitectures = "native";
        ReadWritePaths = [
          beetsState
          beetsLibrary
          audioInbox
          reviewDir
        ];
        ReadOnlyPaths = [ paths.library.music ];
      };
    };

    timers.beets-import = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "2min";
        OnUnitInactiveSec = "2min";
        AccuracySec = "15s";
        Unit = "beets-import.service";
      };
    };

    paths.beets-import = {
      wantedBy = [ "paths.target" ];
      unitConfig.RequiresMountsFor = paths.mediaRoot;
      pathConfig = {
        PathChanged = audioInbox;
        Unit = "beets-import.service";
        TriggerLimitIntervalSec = "60s";
        TriggerLimitBurst = 200;
      };
    };
  };
}
