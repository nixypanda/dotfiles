{ lib, ... }:

let
  mediaServices = [
    "jellyfin"
    "kavita"
    "radarr"
    "sonarr"
  ];

  requireMediaMount =
    name:
    lib.nameValuePair name {
      after = [ "srv-media.mount" ];
      requires = [ "srv-media.mount" ];
    };
in
{
  # The Realtek RTL9201DP enclosure reports I/O errors under UAS with the 4T
  # media HDD. Force usb-storage for stability.
  boot.kernelParams = [
    "usb-storage.quirks=0bda:9201:u"
  ];

  fileSystems."/srv/media" = {
    device = "/dev/disk/by-uuid/5c5bc7ed-677b-4f72-a1ce-e59d43af756a";
    fsType = "ext4";
    options = [
      "noatime"
      "nofail"
    ];
  };

  systemd.services = builtins.listToAttrs (map requireMediaMount mediaServices);
}
