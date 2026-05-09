{
  system = {
    primaryUser = "nixypanda";
    defaults = {
      SoftwareUpdate.AutomaticallyInstallMacOSUpdates = false;
      NSGlobalDomain = {
        InitialKeyRepeat = 10;
        KeyRepeat = 1;
        AppleInterfaceStyle = "Dark";
        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticDashSubstitutionEnabled = false;
        NSAutomaticPeriodSubstitutionEnabled = false;
        NSAutomaticQuoteSubstitutionEnabled = false;
        NSAutomaticSpellingCorrectionEnabled = false;
        NSAutomaticWindowAnimationsEnabled = false;

      };
      dock = {
        autohide = true;
        mru-spaces = false;
        show-recents = false;
      };
    };
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToEscape = true;
    };
    stateVersion = 5;
  };
  networking.hostName = "srt-l02-sekhmet";
}
