{
  programs.nushell = { enable = true; };
  home.file."Library/Application Support/nushell/env.nu".source = ./env.nu;
  home.file."Library/Application Support/nushell/config.nu".source =
    ./config.nu;
}

