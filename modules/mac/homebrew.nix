# Homebrew casks shared by the Macs.
{
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "zap";
      upgrade = true;
    };
    taps = [
    ];
    casks = [
      "docker-desktop"
      "zoom"
      "protonvpn"
      "stremio"
      "steipete/tap/codexbar"
      "colemak-dh"
      "obsidian"
    ];
  };
}
