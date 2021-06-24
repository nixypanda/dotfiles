{ pkgs }:

pkgs.stdenv.mkDerivation {
  name = "custom-browsermediacontrol";
  buildInputs = with pkgs; [
    pkg-config
    cairo
    gobject-introspection
    (
      python3.withPackages (
        python3Packages: with python3Packages; [
          pydbus
          pygobject3
        ]
      )
    )
  ];
  unpackPhase = ":";
  installPhase = ''
    mkdir -p $out/bin
    cp ${./bmc.py} $out/bin/custom-browsermediacontrol
    chmod +x $out/bin/custom-browsermediacontrol
  '';
}
