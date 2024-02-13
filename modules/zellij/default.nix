{
  programs.zellij = {
    enable = true;
    enableZshIntegration = true;
  };
  xdg.configFile."zellij/config.kdl".text = ''
    default_shell "nu"
    pane_frames false
    theme "catppuccin-macchiato"
    ${builtins.readFile ./keybinds.kdl}
  '';
}
