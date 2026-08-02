{ lib, pkgs, ... }:

let
  mediaMount = "/srv/media";
  mediaDirectoriesUnit = "media-directories.service";
  mediaMountUnit = "srv-media.mount";
  mediaServices = [
    "audiobookshelf"
    "jellyfin"
    "kavita"
    "prowlarr-sync-config"
    "qbittorrent"
    "radarr"
    "radarr-sync-config"
    "shelfmark"
    "sonarr"
    "sonarr-sync-config"
  ];

  requireMediaMount =
    name:
    lib.nameValuePair name {
      after = [ mediaDirectoriesUnit ];
      requires = [ mediaDirectoriesUnit ];
      bindsTo = [ mediaMountUnit ];
      unitConfig = {
        RequiresMountsFor = mediaMount;
        ConditionPathIsMountPoint = mediaMount;
      };
    };
in
{
  # Both media enclosures report I/O errors under UAS. Force usb-storage for
  # stability; keep the Realtek quirk for switching back to the 4 TB disk.
  boot.kernelParams = [
    "usb-storage.quirks=0bda:9201:u,152d:0583:u"
  ];

  environment.systemPackages = [ pkgs.smartmontools ];

  fileSystems.${mediaMount} = {
    device = "/dev/disk/by-id/ata-geonix_gold_edition_2022092600288-part1";
    fsType = "ext4";
    options = [
      "noatime"
      "nofail"
    ];
  };

  systemd.services = builtins.listToAttrs (map requireMediaMount mediaServices) // {
    media-directories = {
      description = "Create directories on the mounted media disk";
      bindsTo = [ mediaMountUnit ];
      unitConfig = {
        RequiresMountsFor = mediaMount;
        ConditionPathIsMountPoint = mediaMount;
      };
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${lib.getExe' pkgs.systemd "systemd-tmpfiles"} --create --prefix=${mediaMount}";
      };
    };
  };
}
