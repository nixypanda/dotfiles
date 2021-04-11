{ config, pkgs, libs, ... }:
{
  home.packages = with pkgs; [
    google-chrome
  ];

  home.file.".config/google-chrome/NativeMessagingHosts".source = pkgs.symlinkJoin {
    name = "native-messaging-hosts";
    paths = [
      "${pkgs.plasma-browser-integration}/etc/var/empty/chrome/native-messaging-hosts"
    ];
  };
}
