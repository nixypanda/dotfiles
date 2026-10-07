{
  pkgs,
  lib,
  ...
}:
let
  inherit (import ./kitty-dev.nix { inherit pkgs; })
    kittyDevApp
    kittyDevBin
    ;
in
{
  home = {
    # The copied app bundle is patched after leaving its original derivation, so
    # sign only kitty-dev.app after Home Manager has copied it into place.
    activation.kittyDevCodesign = lib.hm.dag.entryAfter [ "copyApps" ] (
      lib.optionalString pkgs.stdenv.hostPlatform.isDarwin ''
        app="$HOME/Applications/Home Manager Apps/kitty-dev.app"
        if [ -d "$app" ]; then
          /usr/bin/codesign --force --deep --sign - "$app"
          /usr/bin/codesign --verify --deep --strict --verbose=2 "$app"
        fi
      ''
    );

    packages = [
      kittyDevApp
      kittyDevBin
    ];
  };
}
