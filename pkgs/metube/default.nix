{
  lib,
  stdenv,
  autoPatchelfHook,
  fetchFromGitHub,
  fetchPnpmDeps,
  ffmpeg,
  makeWrapper,
  nix-update-script,
  nodejs_24,
  pnpm_10,
  pnpmConfigHook,
  python3,
}:

let
  version = "2026.11.01";

  src = fetchFromGitHub {
    owner = "alexta69";
    repo = "metube";
    tag = version;
    hash = "sha256-vp5ZbihV3nbI0RJyfaW6qmoKd28NcCi9xqqaS/zRL4c=";
  };

  python = python3.withPackages (ps: [
    ps.aiohttp
    ps.python-socketio
    ps.watchfiles
    ps.yt-dlp
  ]);

  ui = stdenv.mkDerivation (finalAttrs: {
    pname = "metube-ui";
    inherit version src;

    nativeBuildInputs = [
      nodejs_24
      pnpmConfigHook
      pnpm_10
    ]
    ++ lib.optionals stdenv.hostPlatform.isLinux [ autoPatchelfHook ];

    buildInputs = lib.optionals stdenv.hostPlatform.isLinux [ stdenv.cc.cc.lib ];

    # sass-embedded ships a prebuilt Dart binary. Its ELF interpreter points
    # outside the Nix store, so patch it before Angular invokes it.
    preBuild = lib.optionalString stdenv.hostPlatform.isLinux ''
      for dir in ui/node_modules/.pnpm/sass-embedded-linux-*/node_modules/sass-embedded-linux-*/dart-sass/src; do
        if [ -e "$dir/dart" ]; then
          autoPatchelf "$dir"
        fi
      done
    '';

    pnpmRoot = "ui";
    pnpmDeps = fetchPnpmDeps {
      inherit (finalAttrs) pname version src;
      pnpm = pnpm_10;
      fetcherVersion = 3;
      sourceRoot = "${src.name}/ui";
      hash = "sha256-jBOvI1uY5dFw7lNnAVE3P6KWP/DV6SDLT4/djEcVZDw=";
    };

    # pnpm tries to switch itself to the version in package.json's
    # packageManager field, which is not available inside the sandbox.
    pnpmInstallFlags = [ "--config.manage-package-manager-versions=false" ];

    buildPhase = ''
      runHook preBuild
      cd ui
      pnpm run build
      cd ..
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p $out
      cp -r ui/dist/metube/browser $out/browser
      runHook postInstall
    '';
  });
in
stdenv.mkDerivation {
  pname = "metube";
  inherit version src;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/metube/ui/dist/metube
    cp -r app $out/share/metube/app
    cp -r ${ui}/browser $out/share/metube/ui/dist/metube/browser

    makeWrapper ${python}/bin/python3 $out/bin/metube \
      --add-flags "$out/share/metube/app/main.py" \
      --prefix PATH : ${lib.makeBinPath [ ffmpeg ]} \
      --set PYTHONDONTWRITEBYTECODE 1 \
      --set-default BASE_DIR "$out/share/metube"

    runHook postInstall
  '';

  passthru.updateScript = nix-update-script { };

  meta = {
    description = "Self-hosted web UI for yt-dlp";
    homepage = "https://github.com/alexta69/metube";
    license = lib.licenses.agpl3Only;
    mainProgram = "metube";
    platforms = lib.platforms.linux;
  };
}
