{
  programs.nushell = { enable = true; };
  home.file.".config/nushell/env.nu".source = ./env.nu;
  home.file.".config/nushell/config.nu".source = ./config.nu;
}

