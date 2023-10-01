{
  homebrew.enable = true;
  homebrew.onActivation = {
    autoUpdate = true;
    cleanup = "zap";
    upgrade = true;
  };
  homebrew.global = {
    brewfile = true;
    lockfiles = false;
  };
  homebrew.taps = [ "homebrew/cask" "homebrew/cask-drivers" "homebrew/core" ];
  homebrew.casks = [ "google-chrome" "vlc" ];
}
