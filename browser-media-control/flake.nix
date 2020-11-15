{
  description = "Browser Media Controls";
  inputs = {
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs:
    let
      system = "x86_64-linux";
      pkgs = inputs.unstable.legacyPackages.${system};
      pythonDeps = ps: with ps; [ pydbus pygobject3 ];
      deps = with pkgs; [
        pkg-config
        cairo gobject-introspection
        (python3.withPackages pythonDeps)
      ];
    in
    {
      devShell."${system}" = pkgs.mkShell {
        buildInputs = deps;
      };
      defaultPackage."${system}" = pkgs.stdenv.mkDerivation {
        name = "custom-browsermediacontrol";
        buildInputs = deps;
        unpackPhase = ":";
        installPhase = ''
          mkdir -p $out/bin
          cp ${./bmc.py} $out/bin/custom-browsermediacontrol
          chmod +x $out/bin/custom-browsermediacontrol
        '';
      };
    };
}
