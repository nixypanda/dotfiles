{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # CLI tools / Terminal facification
    dig
    unrar
    unzip
    # Build failing on MacOS
    # wireshark

    # Broken for macos (from the looks of it)
    # mitmproxy

    # Better alternatives
    bottom # top
    dua # disk usage analyzer
    fd # find
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

    terraform

    # Nix itself
    nixVersions.nix_2_21
  ];

  programs.starship = {
    enable = true;
  };
}
