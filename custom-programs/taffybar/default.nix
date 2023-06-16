{ pkgs }: pkgs.haskellPackages.callCabal2nix "my-taffybar" ./. { }
