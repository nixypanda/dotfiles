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
    zoom-us
    # Build error on darwin
    slack
  ] else [

  ]);
}
