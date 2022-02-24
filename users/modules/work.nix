{ config, pkgs, libs, ... }:
{
  home.packages = with pkgs; [
    slack
    openvpn
    nomad
    vault
    consul
    envoy
    terraform
    packer
    vagrant
    zoom-us
  ];
}
