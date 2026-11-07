{ pkgs, ... }:
{
  # Kept as an installed command rather than a just recipe because it is
  # routinely run outside the dotfiles repo.
  home.packages = [
    (pkgs.writeScriptBin "sync-hledger" (builtins.readFile ./sync-hledger.sh))
  ];
}
