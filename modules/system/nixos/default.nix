# Baseline NixOS system configuration shared by fleet hosts.
{ ... }:
{
  imports = [
    ./locale.nix
    ./nix.nix
    ./openssh.nix
    ./tailscale.nix
  ];
}
