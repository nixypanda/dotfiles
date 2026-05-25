{ kitty-upstream }:
final: prev: {
  kitty-dev = prev.kitty.overrideAttrs (
    old:
    let
      version = "floating-pane-experiment-${kitty-upstream.shortRev or kitty-upstream.rev}";
      src = kitty-upstream;
    in
    {
      inherit src version;
      inherit
        (final.buildGo126Module {
          pname = "kitty-go-modules";
          inherit src version;
          vendorHash = "sha256-FaSWBeQJlvw9vXcHJ/OaFd48K8d7X86X8w7wpG84Ltw=";
        })
        goModules
        ;
      nativeBuildInputs = map (
        pkg: if (pkg.pname or "") == "go" then final.go_1_26 else pkg
      ) old.nativeBuildInputs;
      env = old.env // {
        GOTOOLCHAIN = "local";
      };
    }
  );
}
