{ pkgs, colorscheme, ... }: {
  programs.rofi = {
    enable = true;
    package = pkgs.rofi.override {
      plugins = [ pkgs.rofi-emoji pkgs.rofi-calc pkgs.rofi-file-browser ];
    };
    font = "hack 10";
  };
  home = {
    file = {
      ".config/rofi/colors.rasi".text = ''
        * {
          accent: ${colorscheme.accent-primary};
          accent-secondary: ${colorscheme.accent-secondary};
          background: ${colorscheme.bg-primary};
          foreground: ${colorscheme.fg-primary};
        }
      '';
      ".config/rofi/grid.rasi".source = ./grid.rasi;
      ".config/rofi/launcher.rasi".source = ./launcher.rasi;
    };
  };
}
