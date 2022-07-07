{ pkgs, ... }:
{
  home.packages = with pkgs; [
    openvpn
    nomad
    vault
    consul
    terraform
    packer
    vagrant
  ] ++ (if pkgs.stdenv.isLinux then [
    # Unavailable for darwin
    envoy
    # Sha mismatch on darwin
    zoom-us
    # Build error on darwin
    slack
  ] else [

  ]);
}
