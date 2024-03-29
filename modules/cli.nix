{ pkgs, ... }: {
  home.packages = with pkgs; [
    # CLI tools / Terminal facification
    arandr
    bashmount
    dig
    graphviz
    unrar
    unzip
    # Build failing on MacOS
    # wireshark
    pgcli

    ngrok
    openvpn
    drone-cli
    # Broken for macos (from the looks of it)
    # mitmproxy

    # Better alternatives
    bottom # top
    dogdns # dig
    dua # disk usage analyzer
    fd # find
    hexyl # hex viewer
    lsd # ls
    ripgrep # grep
    tokei # cloc, sloc, etc
    zoxide # cd with jumping
    hyperfine # benchmarking (time)

    # Structured data
    fx
    jc
    jq

    neofetch
    carapace

    # Nix itself
    nixVersions.nix_2_21
  ];

  programs.starship = { enable = true; };
}
