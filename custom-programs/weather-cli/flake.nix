{
  description = "Weather CLI";
  inputs = {
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs:
    let
      system = "x86_64-linux";
      pkgs = inputs.unstable.legacyPackages.${system};
      pythonDeps = ps: with ps; [ requests ];
      deps = with pkgs; [
        pkg-config
        (python3.withPackages pythonDeps)
      ];
    in
    {
      devShell."${system}" = pkgs.mkShell {
        buildInputs = deps;
      };
      defaultPackage."${system}" = pkgs.stdenv.mkDerivation {
        name = "custom-weather-cli";
        buildInputs = deps;
        unpackPhase = ":";
        installPhase = ''
          mkdir -p $out/bin
          cp ${./weather.py} $out/bin/custom-weather-cli
          chmod +x $out/bin/custom-weather-cli
        '';
      };
    };
}
