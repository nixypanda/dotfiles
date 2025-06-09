{ pkgs, colorscheme, ... }:
{
  home.packages = with pkgs; [
    # CLI tools / Terminal facification
    dig
    unzip

    # Better alternatives
    bottom # top
    fd # find
    ripgrep # grep
    tokei # cloc, sloc, etc
    hyperfine # benchmarking (time)

    # Structured data
    fx
    jc
    jq

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
        history_filter = [ "shit" ];
      };
    };
    bat = {
      enable = true;
      config.theme = colorscheme.bat-theme-name;
    };
    direnv = {
      enable = true;
      enableNushellIntegration = true;
      nix-direnv.enable = true;
    };
  };
}
