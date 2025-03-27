{
  programs.zellij = {
    enable = true;
  };
  xdg.configFile = {
    "zellij/config.kdl".text = # kdl
      ''
        default_shell "nu"
        pane_frames false
        theme "catppuccin-macchiato"
        simplified_ui true

        keybinds clear-defaults=true {
          ${builtins.readFile ./keybinds.kdl}
        }
      '';
  };
}
