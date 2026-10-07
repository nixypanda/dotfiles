{ lib, pkgs, ... }:
let
  # hledger-lsp is not packaged in nixpkgs yet.
  hledger-lsp = pkgs.callPackage ../pkgs/hledger-lsp.nix { };
in
{
  home.packages = with pkgs; [
    hledger
    hledger-ui
    # `hledger-web` and `haskell-language-server` both install
    # `lib/links/libHSbase64-*.dylib`, which collides in the home-manager
    # buildEnv. Give hledger-web priority so buildEnv resolves it.
    (lib.hiPrio hledger-web)
    paisa

    hledger-fmt
    hledger-lsp
  ];
}
