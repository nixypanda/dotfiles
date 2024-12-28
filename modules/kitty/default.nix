{ pkgs, colorscheme, ... }:
{
  programs.kitty = {
    enable = true;
    # temp: kitty 0.38 is broken on nixpkgs
    package = (
      pkgs.kitty.overrideDerivation (o: rec {
        version = "0.38.1";
        src = pkgs.fetchFromGitHub {
          owner = "kovidgoyal";
          repo = "kitty";
          rev = "refs/tags/v${version}";
          hash = "sha256-0M4Bvhh3j9vPedE/d+8zaiZdET4mXcrSNUgLllhaPJw=";
        };
      })
    );
    settings = {
      font_size = 10;
      shell = "zsh";
      scrollback_lines = 10000;
      input_delay = 1;

      foreground = "${colorscheme.fg-primary}";
      background = "${colorscheme.bg-primary}";

      macos_option_as_alt = "left";
      macos_show_window_title_in = "none";

      color0 = "${colorscheme.black}";
      color1 = "${colorscheme.red}";
      color2 = "${colorscheme.green}";
      color3 = "${colorscheme.yellow}";
      color4 = "${colorscheme.blue}";
      color5 = "${colorscheme.magenta}";
      color6 = "${colorscheme.cyan}";
      color7 = "${colorscheme.white}";
      color8 = "${colorscheme.bright-black}";
      color9 = "${colorscheme.bright-red}";
      color10 = "${colorscheme.bright-green}";
      color11 = "${colorscheme.bright-yellow}";
      color12 = "${colorscheme.bright-blue}";
      color13 = "${colorscheme.bright-magenta}";
      color14 = "${colorscheme.bright-cyan}";
      color15 = "${colorscheme.bright-white}";
    };
  };
}
