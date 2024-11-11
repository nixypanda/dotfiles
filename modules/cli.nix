{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # CLI tools / Terminal facification
    dig
    unrar
    unzip

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

    carapace
    vim-startuptime

    # Nix itself
    nixVersions.latest
  ];

  programs.starship = {
    enable = true;
  };
}
