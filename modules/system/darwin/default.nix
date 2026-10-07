# Shared nix-darwin configuration for the Macs.
{ ... }:
{
  imports = [
    ./system.nix
    ./homebrew.nix
  ];
}
