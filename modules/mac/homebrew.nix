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
    taps = [ ];
    casks = [
      "google-chrome"
      "1password"
      "docker"
      "microsoft-onenote"
      "zoom"
      "protonvpn"
    ];
  };
}
