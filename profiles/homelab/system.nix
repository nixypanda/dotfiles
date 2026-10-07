# Role: the system half of a headless homelab service host (NixOS).
{ ... }:
{
  imports = [
    ../../modules/system/nixos
    ../../services
  ];
}
