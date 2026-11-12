{
  lib,
  stdenv,
  fetchFromGitHub,
  buildNpmPackage,
  python313,
  makeWrapper,
  nodejs_24,
  ffmpeg,
  flac,
  chromaprint,
  rubberband,
  deno,
  libarchive,
}:

let
  version = "3.6.4";

  src = fetchFromGitHub {
    owner = "Nezreka";
    repo = "SoulSync";
    tag = version;
    hash = "sha256-YMfckb5Fx29B5WHRdeUTg1ZsG1UBdATa92BeofTLMjs=";
  };

  # Use the cached Python 3.13 package set without test overrides that would
  # invalidate upstream binary-cache paths for its dependencies.
  python = python313.withPackages (ps: [
    ps.gunicorn
    ps.flask
    ps.flask-limiter
    # SpotipyFree (GPL-3.0) is an optional upstream dependency for full
    # Spotify link imports. It is intentionally omitted; upstream soft-imports
    # it and falls back to Spotify's embed scraper (~100 tracks) when absent.
    ps.spotipy
    ps.plexapi
    ps.requests
    ps.aiohttp
    ps.cryptography
    ps.mutagen
    ps.pillow
    ps.librosa
    ps.soundfile
    ps.onnxruntime
    ps.unidecode
    ps.beautifulsoup4
    ps.psutil
    ps.tzdata
    ps.tzlocal
    ps.yt-dlp
    ps.ytmusicapi
    ps.lrclibapi
    ps.pyacoustid
    ps.websocket-client
    ps.tidalapi
    ps.flask-socketio
    ps.simple-websocket
  ]);

  ui = buildNpmPackage {
    pname = "soulsync-ui";
    inherit version src;
    sourceRoot = "${src.name}/webui";
    nodejs = nodejs_24;
    npmDepsHash = "sha256-qm4+mHV/Lw4nKLP/cxB82VvEek361qUpzgPSHFYpeoY=";
    npmBuildScript = "build";
    # The package's build produces webui/static/dist.
    dontNpmInstall = false;
    installPhase = ''
      runHook preInstall
      mkdir -p $out
      cp -r static/dist $out/dist
      runHook postInstall
    '';
  };
in
stdenv.mkDerivation {
  pname = "soulsync";
  inherit version src;

  nativeBuildInputs = [ makeWrapper ];

  # The Python environment is the nixpkgs Python 3.13 interpreter; all
  # requirements are available there (see the build check).
  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/soulsync
    # Copy the application tree, excluding the (source) webui build output
    # which is replaced by the Nix-built bundle below.
    cp -r . $out/share/soulsync/
    rm -rf $out/share/soulsync/webui/static/dist
    mkdir -p $out/share/soulsync/webui/static
    cp -r ${ui}/dist $out/share/soulsync/webui/static/dist

    # Config template at the agreed location. Adjust Docker-oriented defaults
    # to bare-metal relative paths so a direct invocation has writable targets.
    mkdir -p $out/share/soulsync/config
    cp config/config.example.json $out/share/soulsync/config/config.example.json
    sed -i \
      -e 's#/app/downloads#./downloads#g' \
      -e 's#/app/Transfer#./Transfer#g' \
      -e 's#/app/Staging#./Staging#g' \
      -e 's#/app/podcasts#./podcasts#g' \
      -e 's#/app/audiobooks#./audiobooks#g' \
      $out/share/soulsync/config/config.example.json

    # Keep streaming's staging directory out of the read-only store.
    substituteInPlace $out/share/soulsync/web_server.py \
      --replace-fail \
        'base_root = os.path.dirname(os.path.abspath(__file__))' \
        'base_root = os.environ.get("SOULSYNC_STREAM_DIR", os.path.dirname(os.path.abspath(__file__)))'

    # Default bare invocations to loopback; the service and direct callers can
    # still override with Gunicorn's standard --bind option.
    substituteInPlace $out/share/soulsync/gunicorn.conf.py \
      --replace-fail 'bind = "0.0.0.0:8008"' 'bind = "127.0.0.1:8008"'

    makeWrapper ${python}/bin/gunicorn $out/bin/soulsync \
      --set SOULSYNC_APP_DIR "$out/share/soulsync" \
      --prefix PYTHONPATH : "$out/share/soulsync" \
      --set-default SOULSYNC_NO_FFMPEG_DOWNLOAD "1" \
      --prefix PATH : ${
        lib.makeBinPath [
          ffmpeg
          flac
          chromaprint
          rubberband
          deno
          libarchive
        ]
      } \
      --run 'state_dir="''${SOULSYNC_STATE_DIR:-''${XDG_STATE_HOME:-$HOME/.local/state}/soulsync}"; mkdir -p "$state_dir/config" "$state_dir/data" "$state_dir/database" "$state_dir/logs" "$state_dir/downloads" "$state_dir/Transfer" "$state_dir/Staging" "$state_dir/Stream" "$state_dir/storage" "$state_dir/scripts" "$state_dir/samples" "$state_dir/avatars" "$state_dir/image_cache" "$state_dir/playlist_covers"; if [ -z "''${SOULSYNC_CONFIG_PATH:-}" ]; then config_file="$state_dir/config/config.json"; if [ ! -f "$config_file" ]; then install -m 0600 "$SOULSYNC_APP_DIR/config/config.example.json" "$config_file"; fi; export SOULSYNC_CONFIG_PATH="$config_file"; fi; export DATABASE_PATH="''${DATABASE_PATH:-$state_dir/data/music_library.db}"; export VIDEO_DATABASE_PATH="''${VIDEO_DATABASE_PATH:-$state_dir/data/video_library.db}"; export AUDIOBOOK_DATABASE_PATH="''${AUDIOBOOK_DATABASE_PATH:-$state_dir/data/audiobooks.db}"; export SOULSYNC_LOG_FILE="''${SOULSYNC_LOG_FILE:-$state_dir/logs/app.log}"; export SOULSYNC_SCRIPTS_DIR="''${SOULSYNC_SCRIPTS_DIR:-$state_dir/scripts}"; export SOULSYNC_AVATAR_DIR="''${SOULSYNC_AVATAR_DIR:-$state_dir/avatars}"; export SOULSYNC_IMAGE_CACHE_DIR="''${SOULSYNC_IMAGE_CACHE_DIR:-$state_dir/image_cache}"; export SOULSYNC_PLAYLIST_COVER_DIR="''${SOULSYNC_PLAYLIST_COVER_DIR:-$state_dir/playlist_covers}"; export SOULSYNC_STREAM_DIR="''${SOULSYNC_STREAM_DIR:-$state_dir}"; if [ "''${SOULSYNC_PRESERVE_CWD:-0}" != "1" ]; then cd "$state_dir"; fi' \
      --add-flags "-c" \
      --add-flags "$out/share/soulsync/gunicorn.conf.py" \
      --add-flags "wsgi:application"

    runHook postInstall
  '';

  meta = {
    description = "SoulSync music discovery and automation platform";
    homepage = "https://github.com/Nezreka/SoulSync";
    license = lib.licenses.mit;
    mainProgram = "soulsync";
    platforms = lib.platforms.linux;
  };
}
