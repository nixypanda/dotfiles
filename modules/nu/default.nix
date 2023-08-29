{
  programs.nushell = {
    enable = true;
    extraConfig = ''
      ${builtins.readFile ./config.nu}
      ${builtins.readFile ./aliases.nu}
    '';
    extraEnv = ''
      ${builtins.readFile ./env.nu}
    '';
  };
}
