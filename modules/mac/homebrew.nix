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
      "docker"
      "zoom"
      "protonvpn"
    ];
  };
}
