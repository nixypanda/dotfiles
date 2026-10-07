{
  lib,
  stdenvNoCC,
  fetchurl,
  makeWrapper,
  unzip,
  ripgrep,
  sysctl,
  version ? "2.0.14",
}:

let
  system = stdenvNoCC.hostPlatform.system;

  # OpenCode 2 is not published on GitHub releases (only tags exist); the
  # standalone binaries are served from opencode.ai, keyed by version.
  # nixpkgs still only packages the V1 CLI (`pkgs.opencode`), so both Apple
  # Silicon and Intel Darwin build from these binaries here.
  #
  # To bump, pick a version and refresh the hashes:
  #   nix store prefetch-file --json \
  #     "https://opencode.ai/files/bin/<version>/opencode-darwin-<arch>.zip"
  #
  # Keep this in step with the desktop app when convenient: it bundles its own
  # CLI under ~/Library/Application Support/ai.opencode.desktop/cli/<version>/.
  # A version gap is tolerable (V2 clients accept a service from another 2.x
  # release), but an exact match is what the desktop app runs.
  sources = {
    x86_64-darwin = {
      file = "opencode-darwin-x64.zip";
      hash = "sha256-d8r0uahUd3AgJDnAsGIFtQeOsZOb1sBajqYoCQJrKJI=";
    };
    aarch64-darwin = {
      file = "opencode-darwin-arm64.zip";
      hash = "sha256-RAwK6SOvQFGN006ZMVr4yFZzIJTrSiE3p6lKagDxqkU=";
    };
  };

  source =
    sources.${system}
      or (throw "opencode.nix: unsupported system ${system}; use pkgs.opencode instead");

  src = fetchurl {
    url = "https://opencode.ai/files/bin/${version}/${source.file}";
    inherit (source) hash;
  };
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
    platforms = [
      "x86_64-darwin"
      "aarch64-darwin"
    ];
  };
}
