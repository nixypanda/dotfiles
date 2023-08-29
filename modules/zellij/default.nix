{
  programs.zellij = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      theme = "catppuccin-macchiato";
      default_layout = "compact";
      default_shell = "nu";
      pane_frames = false;
    };
  };
}
