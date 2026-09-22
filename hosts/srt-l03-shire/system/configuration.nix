# Only what is specific to this host. Shared Mac config lives in
# modules/mac/.
{ ... }:
{
  imports = [
    ../../../modules/mac/system.nix
    ../../../modules/mac/homebrew.nix
  ];

  networking.hostName = "srt-l03-shire";

  # This machine runs Determinate Nix, which manages the Nix installation
  # (daemon, /etc/nix/nix.conf) itself. nix-darwin's native Nix management
  # conflicts with it, so disable it here.
  nix.enable = false;
}
