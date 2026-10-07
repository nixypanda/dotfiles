# Only what is specific to this host. The shared workstation role lives in
# profiles/workstation/.
{ ... }:
{
  imports = [
    ../../profiles/workstation/system.nix
  ];

  networking.hostName = "srt-l03-shire";

  # This machine runs Determinate Nix, which manages the Nix installation
  # (daemon, /etc/nix/nix.conf) itself. nix-darwin's native Nix management
  # conflicts with it, so disable it here.
  nix.enable = false;
}
