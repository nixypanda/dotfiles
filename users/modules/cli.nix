{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    # CLI tools / Terminal facification
    arandr
    awscli
    bashmount
    dig
    docker-compose
    dua
    fx
    gnumake
    graphviz
    hexyl
    jq
    ngrok
    nix-du
    openvpn
    tokei
    unrar
    unzip
    wireshark

    # Moar colors
    starship
    zsh-syntax-highlighting

    # Searching/Movement helpers
    ripgrep
    zoxide

    # system info
    bottom
    neofetch
  ];
}
