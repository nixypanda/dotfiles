{
  lib,
  pkgs,
  colorscheme,
  ...
}:
{
  home.packages =
    with pkgs;
    [
      # CLI tools / Terminal facification
      dig
      unzip
      py7zr
      qpdf
      hledger
      hledger-ui
      hledger-web
      paisa

      # Better alternatives
      bottom # top
      fd # find
      ripgrep # grep
      tokei # cloc, sloc, etc
      hyperfine # benchmarking (time)
      gh

      # Structured data
      jc
      jq

      # Nix related
      nixVersions.latest
      nix-forecast
      age

      # Document conversion
      pandoc
    ]
    ++ lib.optionals stdenv.hostPlatform.isLinux [
      # USB tooling
      usbutils
      usb-modeswitch
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
      config = {
        theme-dark = "${colorscheme.bat-name-dark}";
        theme-light = "${colorscheme.bat-name-light}";
      };
    };
    direnv = {
      enable = true;
      enableNushellIntegration = true;
      nix-direnv.enable = true;
    };
  };
}
