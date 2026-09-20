{
  lib,
  stdenvNoCC,
  fetchurl,
  makeWrapper,
  unzip,
  ripgrep,
  sysctl,
  version ? "2.0.11",
}:

let
  system = stdenvNoCC.hostPlatform.system;

  # OpenCode 2 is not published on GitHub releases (only tags exist); the
  # standalone binaries are served from opencode.ai, keyed by version.
  #
  # To bump, pick a version and refresh the hash:
  #   nix store prefetch-file --json \
  #     "https://opencode.ai/files/bin/<version>/opencode-darwin-x64.zip"
  #
  # Keep this in step with the desktop app when convenient: it bundles its own
  # CLI under ~/Library/Application Support/ai.opencode.desktop/cli/<version>/.
  # A version gap is tolerable (V2 clients accept a service from another 2.x
  # release), but an exact match is what the desktop app runs.
  src =
    if system == "x86_64-darwin" then
      fetchurl {
        url = "https://opencode.ai/files/bin/${version}/opencode-darwin-x64.zip";
        hash = "sha256-e3dP79/ABX1iu6t7kAb+CReGN68p9YNdGjIy5qvZjAk=";
      }
    else
      throw "opencode.nix: unsupported system ${system}; use pkgs.opencode instead";
in
stdenvNoCC.mkDerivation {
  pname = "opencode";
  inherit version src;

  nativeBuildInputs = [
    makeWrapper
    unzip
  ];

  dontConfigure = true;
  dontBuild = true;
  dontUnpack = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $TMPDIR
    unzip $src -d $TMPDIR
    cp $TMPDIR/opencode $out/bin/
    chmod +x $out/bin/opencode

    wrapProgram $out/bin/opencode \
      --prefix PATH : ${
        lib.makeBinPath ([ ripgrep ] ++ lib.optionals stdenvNoCC.hostPlatform.isDarwin [ sysctl ])
      }

    runHook postInstall
  '';

  meta = {
    description = "AI coding agent for the terminal";
    homepage = "https://opencode.ai";
    license = lib.licenses.mit;
    mainProgram = "opencode";
    platforms = [ "x86_64-darwin" ];
  };
}
