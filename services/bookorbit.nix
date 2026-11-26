{
  bookorbit,
  config,
  homelab,
  lib,
  paths,
  pkgs,
  ...
}:

let
  inherit (homelab) tailnetUrl services;
  bookorbitPort = services.bookorbit.local;
  bookorbitUrl = tailnetUrl services.bookorbit.tailnet;

  bookorbitPkg = bookorbit;
  appUser = "bookorbit";
  appGroup = "bookorbit";
  appState = "${paths.state.bookorbit}/app";
  dataDir = "${paths.state.bookorbit}/data";
  secretsDir = "/var/lib/bookorbit/secrets";
  dockPath = "${paths.mediaRoot}/.bookorbit";
  pgDb = "bookorbit";
  pgUser = "bookorbit";

  appUid = toString config.users.users.${appUser}.uid;
in
{
  users.users.${appUser} = {
    isSystemUser = true;
    group = appGroup;
    home = appState;
    createHome = true;
    extraGroups = [ "media" ];
  };
  users.groups.${appGroup} = { };

  services.postgresql = {
    enable = true;
    package = lib.mkForce pkgs.postgresql_16;
    extensions = ps: [ ps.pgvector ];
    ensureDatabases = [ pgDb ];
    ensureUsers = [
      {
        name = pgUser;
        ensureDBOwnership = true;
      }
    ];
    settings = {
      listen_addresses = lib.mkForce "127.0.0.1";
      port = 5432;
    };
  };

  systemd = {
    services = {
      bookorbit-db-setup = {
        description = "Prepare BookOrbit PostgreSQL extensions";
        after = [
          "postgresql-setup.service"
          "postgresql.service"
        ];
        requires = [
          "postgresql-setup.service"
          "postgresql.service"
        ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          User = "postgres";
        };
        script = ''
          ${pkgs.postgresql_16}/bin/psql -d ${pgDb} -v ON_ERROR_STOP=1 <<'SQL'
          CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
          CREATE EXTENSION IF NOT EXISTS pg_trgm;
          CREATE EXTENSION IF NOT EXISTS unaccent;
          CREATE EXTENSION IF NOT EXISTS vector;
          SQL
        '';
      };

      bookorbit-secrets = {
        description = "Generate BookOrbit runtime secrets";
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          StateDirectory = "bookorbit/secrets";
          StateDirectoryMode = "0700";
          User = appUser;
          Group = appGroup;
          UMask = "0077";
        };
        script = ''
          set -eu
          dir=${secretsDir}
          ${pkgs.coreutils}/bin/install -d -m 0700 -o ${appUser} -g ${appGroup} "$dir"
          for name in jwt_secret setup_bootstrap_token; do
            file="$dir/$name"
            if [ ! -s "$file" ]; then
              umask 077
              ${pkgs.openssl}/bin/openssl rand -hex 32 > "$file"
            fi
            ${pkgs.coreutils}/bin/chown ${appUser}:${appGroup} "$file"
            ${pkgs.coreutils}/bin/chmod 0600 "$file"
          done
          # BookOrbit reads BOOK_REQUEST_ENCRYPTION_KEY from the environment, so
          # store this secret as an EnvironmentFile entry rather than a raw value.
          file="$dir/book_request_encryption_key"
          if [ ! -s "$file" ]; then
            umask 077
            printf 'BOOK_REQUEST_ENCRYPTION_KEY=%s\n' "$(${pkgs.openssl}/bin/openssl rand -hex 32)" > "$file"
          fi
          ${pkgs.coreutils}/bin/chown ${appUser}:${appGroup} "$file"
          ${pkgs.coreutils}/bin/chmod 0600 "$file"
        '';
      };

      bookorbit-media-directories = {
        description = "Create BookOrbit directories on the mounted media disk";
        bindsTo = [ "srv-media.mount" ];
        after = [ "media-directories.service" ];
        requires = [ "media-directories.service" ];
        wantedBy = [ "multi-user.target" ];
        unitConfig = {
          RequiresMountsFor = paths.mediaRoot;
          ConditionPathIsMountPoint = paths.mediaRoot;
        };
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        script = ''
          ${pkgs.coreutils}/bin/install -d -m 0755 -o root -g root /books
          ${pkgs.coreutils}/bin/install -d -m 2775 -o ${appUser} -g media ${dockPath}
        '';
      };

      bookorbit = {
        description = "BookOrbit native server";
        after = [
          "bookorbit-db-setup.service"
          "bookorbit-media-directories.service"
          "bookorbit-secrets.service"
          "postgresql.service"
        ];
        requires = [
          "bookorbit-db-setup.service"
          "bookorbit-media-directories.service"
          "bookorbit-secrets.service"
          "postgresql.service"
        ];
        bindsTo = [ "srv-media.mount" ];
        wantedBy = [ "multi-user.target" ];
        unitConfig = {
          RequiresMountsFor = paths.mediaRoot;
          ConditionPathIsMountPoint = paths.mediaRoot;
        };
        path = [
          pkgs.coreutils
          pkgs.ffmpeg
          pkgs.nodejs_24
        ];
        serviceConfig = {
          Type = "simple";
          User = appUser;
          Group = appGroup;
          SupplementaryGroups = [ "media" ];
          WorkingDirectory = bookorbitPkg;
          Environment = [
            "NODE_ENV=production"
            "HOST=127.0.0.1"
            "PORT=${toString bookorbitPort}"
            "APP_URL=${bookorbitUrl}"
            "CLIENT_URL=${bookorbitUrl}"
            "APP_DATA_PATH=${dataDir}"
            "DATABASE_URL=postgres:///bookorbit?host=/run/postgresql"
            "JWT_SECRET_FILE=${secretsDir}/jwt_secret"
            "SETUP_BOOTSTRAP_TOKEN_FILE=${secretsDir}/setup_bootstrap_token"
            "BOOKS_HOST_PATH=/books"
            "PUID=${appUid}"
            "PGID=${toString config.users.groups.media.gid}"
            "NODE_MAX_OLD_SPACE_SIZE=2048"
            "BOOK_DOCK_PATH=/books/dock"
            "LIBRARY_BROWSE_ROOT=/books"
            "FFMPEG_PATH=${pkgs.ffmpeg}/bin/ffmpeg"
            "FFPROBE_PATH=${pkgs.ffmpeg}/bin/ffprobe"
            "KOREADER_PLUGIN_PATH=${bookorbitPkg}/koreader-plugin"
          ];
          BindReadOnlyPaths = [
            "${paths.library.manga}:/books/manga"
            "/srv/media/downloads/torrents/complete:/srv/media/downloads/torrents/complete"
          ];
          BindPaths = [
            "${dockPath}:/books/dock"
            "${paths.library.books}:/books/books"
            "${paths.library.audiobooks}:/books/audiobooks"
          ];
          ReadWritePaths = [
            appState
            dataDir
            dockPath
            paths.library.books
            paths.library.audiobooks
          ];
          StateDirectory = "bookorbit";
          EnvironmentFile = "${secretsDir}/book_request_encryption_key";
          ExecStart = "${pkgs.bash}/bin/sh ${bookorbitPkg}/entrypoint.sh";
          Restart = "on-failure";
          RestartSec = "5s";
        };
      };
    };

    tmpfiles.rules = [
      "d ${appState} 0750 ${appUser} ${appGroup} - -"
      "d ${dataDir} 0750 ${appUser} ${appGroup} - -"
      "d /books 0755 root root - -"
      "d /books/books 0755 root root - -"
      "d /books/manga 0755 root root - -"
      "d /books/audiobooks 0755 root root - -"
    ];
  };

  environment.etc."homelab/bookorbit-paths".text = ''
    app_state=${appState}
    data_dir=${dataDir}
    dock_path=${dockPath}
    books_library=${paths.library.books}
    manga_library=${paths.library.manga}
    audiobooks_library=${paths.library.audiobooks}
  '';
}
