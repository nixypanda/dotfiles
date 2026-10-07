# Only what is specific to this host. Shared macOS system config lives in
# modules/system/darwin/.
{ ... }:
{
  imports = [
    ../../modules/system/darwin
  ];

  networking.hostName = "srt-l03-shire";

  # This machine runs Determinate Nix, which manages the Nix installation
  # (daemon, /etc/nix/nix.conf) itself. nix-darwin's native Nix management
  # conflicts with it, so disable it here.
  nix.enable = false;
}
