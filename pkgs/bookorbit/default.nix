{
  stdenv,
  fetchFromGitHub,
  fetchPnpmDeps,
  pnpm,
  pnpmConfigHook,
  nodejs_24,
  ...
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "bookorbit";
  version = "3.8.0";

  src = fetchFromGitHub {
    owner = "bookorbit";
    repo = "bookorbit";
    rev = "v${finalAttrs.version}";
    hash = "sha256-dz/ctTLvI99mi9O9ABKFjDU4xF/Q0CiUAPecw+oz6zc=";
  };

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    fetcherVersion = 4;
    hash = "sha256-VgvAG76NwTJXH9yRsFagnRKnyJ+6LBW6jMn/hq5ZoE0=";
  };

  nativeBuildInputs = [
    nodejs_24
    pnpm
    pnpmConfigHook
  ];

  # Upstream's SWC builder enables typeCheck, which turns `nest build` into a
  # full TypeScript type-check and can run for tens of minutes in this monorepo.
  # The release package only needs compiled JavaScript, so disable typeCheck
  # while retaining SWC compilation and the Swagger plugin.
  postPatch = ''
    substituteInPlace server/nest-cli.json \
      --replace '"typeCheck": true' '"typeCheck": false'
  '';

  buildPhase = ''
    runHook preBuild
    pnpm --config.verify-deps-before-run=false --filter client run build-only
    pnpm --config.verify-deps-before-run=false --filter server run build
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    # Avoid `pnpm deploy`, which tries to re-resolve metadata even in offline mode.
    # The build already installed the complete workspace node_modules, so stage it
    # directly. This keeps dev dependencies too, but avoids network access.
    mkdir -p $out
    # Dereference pnpm symlinks so the immutable Nix output has no store-external
    # or dangling links. This duplicates dependency files but keeps runtime loading
    # self-contained.
    # Use pnpm deploy to materialise a self-contained node_modules with the
    # exact dependency versions selected for the server package.
    pnpm deploy --filter server --prod "$TMPDIR/bookorbit-deploy"
    cp -r "$TMPDIR/bookorbit-deploy/node_modules" $out/node_modules
    cp -r server/dist $out/dist
    mkdir -p $out/migrations
    cp -r server/src/db/migrations/. $out/migrations/
    mkdir -p $out/public $out/locales
    cp -r client/dist/. $out/public/
    cp -r client/src/locales/. $out/locales/
    mkdir -p $out/bin/kepubify $out/vendor $out/koreader-plugin
    cp -r server/bin/kepubify/. $out/bin/kepubify/
    if [ -d server/vendor/foliate ]; then cp -r server/vendor/foliate $out/vendor/foliate; fi
    if [ -d koreader-plugin/bookorbit.koplugin ]; then cp -r koreader-plugin/bookorbit.koplugin/. $out/koreader-plugin/; fi
    cp server/entrypoint.sh server/file-env.sh $out/
    cp LICENSE NOTICE ADDITIONAL_TERMS.md $out/
    runHook postInstall
  '';
})
