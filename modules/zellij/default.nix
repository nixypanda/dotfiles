{
  programs.zellij = {
    enable = true;
    enableZshIntegration = true;
  };
  xdg.configFile."zellij/config.kdl".text = # kdl
    ''
      default_shell "nu"
      pane_frames false
      theme "catppuccin-macchiato"
      default_layout "compact"
      ${builtins.readFile ./keybinds.kdl}
    '';
}
