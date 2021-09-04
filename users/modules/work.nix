{ config, pkgs, libs, ... }:
{
  home.packages = with pkgs; [
    slack
    openvpn
    nomad
    vault
    consul
    terraform
    packer
  ];
}
