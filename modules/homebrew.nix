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
    taps = [ "homebrew/cask" "homebrew/cask-drivers" "homebrew/core" ];
    casks = [ "google-chrome" "vlc" ];
  };
}
