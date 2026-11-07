# Homebrew casks shared by the Macs.
{
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "zap";
      upgrade = true;
    };
    casks = [
      "docker-desktop"
      "zoom"
      "protonvpn"
      "stremio"
      "steipete/tap/codexbar"
      "colemak-dh"
      "obsidian"
      "vorssaint"
    ];
    # Mac App Store-only applications.
    masApps = {
      Primuse = 6761675450;
      Xcode = 497799835;
    };
  };
}
