{ pkgs }:

pkgs.stdenv.mkDerivation {
  name = "custom-weather-cli";
  buildInputs = with pkgs; [
    pkg-config
    (python3.withPackages (python3Packages: with python3Packages; [ requests ]))
  ];
  unpackPhase = ":";
  installPhase = ''
    mkdir -p $out/bin
    cp ${./weather.py} $out/bin/custom-weather-cli
    chmod +x $out/bin/custom-weather-cli
  '';
}
