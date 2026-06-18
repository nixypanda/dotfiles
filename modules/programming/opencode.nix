{
  lib,
  stdenvNoCC,
  fetchurl,
  makeWrapper,
  unzip,
  ripgrep,
  sysctl,
  version ? "1.17.8",
}:

let
  system = stdenvNoCC.hostPlatform.system;
  src =
    if system == "x86_64-darwin" then
      fetchurl {
        url = "https://github.com/anomalyco/opencode/releases/download/v${version}/opencode-darwin-x64.zip";
        hash = "sha256-uVKlICWqtSH6BAQOXdFKQBzIsO+jEanWBz4vQN3x0qE=";
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
    description = "AI coding agent built for the terminal";
    homepage = "https://github.com/anomalyco/opencode";
    license = lib.licenses.mit;
    mainProgram = "opencode";
    platforms = [ "x86_64-darwin" ];
  };
}
