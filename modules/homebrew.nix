{ _, ... }: {
  homebrew.enable = true;
  homebrew.autoUpdate = true;
  homebrew.cleanup = "zap";
  homebrew.global.brewfile = true;
  homebrew.global.noLock = true;

  homebrew.taps = [
    "homebrew/cask"
    "homebrew/cask-drivers"
    "homebrew/core"
  ];
  homebrew.casks = [
    "google-chrome"
  ];
  homebrew.brews = [
    "ltex-ls"
  ];
}
