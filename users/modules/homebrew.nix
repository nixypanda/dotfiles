{ _, ... }: {
  homebrew.enable = true;
  homebrew.autoUpdate = true;
  homebrew.cleanup = "zap";
  homebrew.global.brewfile = true;
  homebrew.global.noLock = true;

  homebrew.taps = [ "homebrew/core" "homebrew/cask" "homebrew/cask-drivers" ];
  homebrew.casks = [ "google-chrome" "chromium" ];
  homebrew.brews = [ "ltex-ls" ];
}
