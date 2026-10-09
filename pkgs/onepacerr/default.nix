{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
  makeWrapper,
  nodejs_24,
}:

buildNpmPackage rec {
  pname = "onepacerr";
  version = "1.7.20";

  src = fetchFromGitHub {
    owner = "eltharynd";
    repo = "OnePacerr";
    tag = "v${version}";
    hash = "sha256-vDW6uaj2gMHHLnCJVPaoxS7clycedso812TPDrcqxCc=";
  };

  nodejs = nodejs_24;
  npmDepsHash = "sha256-Sbw+qYt8ZLoi7EIeqtZmNostvw+Y4PNUM/0s/BsuKRA=";

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
    chmod u+w package-lock.json

    # OnePacerr's unauthenticated status API is only needed for local health
    # checks. Do not expose it on Rivendell's trusted tailnet interface.
    substituteInPlace src/api/express.ts \
      --replace-fail \
        "this.server.listen(portOverride || environment.PORT)" \
        "this.server.listen(portOverride || environment.PORT, '127.0.0.1')"

    # Poster assets in the Nix store are read-only. Remove an existing target
    # before copying so repeated metadata refreshes do not try to overwrite a
    # destination that inherited the source's read-only mode.
    substituteInPlace src/util/safe-copy-file.ts \
      --replace-fail \
        $'\t\tcopyFile(source, destination)' \
        $'\t\tif (existsSync(destination)) unlinkSync(destination)\n\t\tcopyFile(source, destination)'

    # Importing is not idempotent: every run deletes the existing library file
    # and recreates the hardlink/copy, which touches the file and makes the
    # media server rescan and regenerate trickplay/chapter images on each pass.
    # Skip the delete+relink when the destination is already the same hardlink
    # (or an identical-size copy when hardlinks are disabled).
    substituteInPlace src/torrent/torrent.controller.ts \
      --replace-fail \
        "import { existsSync, mkdirSync, readdirSync, unlinkSync } from 'node:fs'" \
        "import { existsSync, mkdirSync, readdirSync, statSync, unlinkSync } from 'node:fs'" \
      --replace-fail \
        "if (!environment.PIPELINE_SKIP_DOWNLOADS_IMPORTS) {" \
        "if (existsSync(destination) && (() => { try { const sourceStat = statSync(source); const destinationStat = statSync(destination); return environment.LIBRARY_USE_HARDLINKS ? (sourceStat.ino === destinationStat.ino && sourceStat.dev === destinationStat.dev) : (sourceStat.size === destinationStat.size); } catch { return false; } })()) { Logger.debug('File already imported, skipping re-import'); processed = true; continue; } if (!environment.PIPELINE_SKIP_DOWNLOADS_IMPORTS) {"
  '';

  nativeBuildInputs = [ makeWrapper ];

  npmBuildScript = "build";

  installPhase = ''
    runHook preInstall

    npm prune --omit=dev --ignore-scripts

    mkdir -p $out/bin $out/lib/onepacerr
    cp -r dist node_modules package.json posters $out/lib/onepacerr/
    touch $out/lib/onepacerr/.env

    makeWrapper ${nodejs_24}/bin/node $out/bin/onepacerr \
      --chdir $out/lib/onepacerr \
      --add-flags "--enable-source-maps $out/lib/onepacerr/dist/index.js"

    runHook postInstall
  '';

  meta = {
    description = "Automated One Pace downloader and media-library organizer";
    homepage = "https://github.com/eltharynd/OnePacerr";
    license = lib.licenses.mit;
    mainProgram = "onepacerr";
    platforms = lib.platforms.linux;
  };
}
