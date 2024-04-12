{
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "zap";
      upgrade = true;
    };
    global = {
      brewfile = true;
      lockfiles = false;
    };
    taps = [ "homebrew/cask-drivers" ];
    casks = [
      "google-chrome"
      "1password"
      "protonvpn"
      "docker"
      "microsoft-onenote"
      "wireshark"
      "signal"
      "zoom"
    ];
  };
}
