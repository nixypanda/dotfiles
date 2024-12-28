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
    hyperfine # benchmarking (time)

    # Structured data
    fx
    jc
    jq

    vim-startuptime

    # Nix itself
    nixVersions.latest
  ];

  programs = {
    carapace = {
      enable = true;
      enableNushellIntegration = true;
    };
    zoxide = {
      enable = true;
      enableNushellIntegration = true;
    };
    starship = {
      enable = true;
      enableNushellIntegration = true;
    };
    atuin = {
      enable = true;
      enableNushellIntegration = true;
      settings = {
        auto_sync = false;
        update_check = false;
        search_mode = "fuzzy";
        cwd_filter = [ "shit" ];
      };
    };
  };
}
