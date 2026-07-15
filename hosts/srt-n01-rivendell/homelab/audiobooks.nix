{ homelab, lib, ... }:

let
  audiobookLibrary = "/srv/media/audiobooks";
  audiobookDownloads = "/srv/media/downloads/audiobooks";
  audiobookshelfState = "/srv/.state/audiobookshelf";
  shelfmarkState = "/srv/.state/shelfmark";
in
{
  services.audiobookshelf = {
    enable = true;
    host = "127.0.0.1";
    port = homelab.services.audiobookshelf.local;
    dataDir = "audiobookshelf";
    openFirewall = false;
  };

  # The native module assumes state below /var/lib. Keep its service and user
  # management while relocating the working directory to the homelab state
  # root used by the other services on this host.
  systemd.services.audiobookshelf.serviceConfig = {
    StateDirectory = lib.mkForce [ ];
    WorkingDirectory = lib.mkForce audiobookshelfState;
  };

  nixarr.shelfmark = {
    enable = true;
    host = "127.0.0.1";
    port = homelab.services.shelfmark.local;
    stateDir = shelfmarkState;
    openFirewall = false;
  };

  services.shelfmark.environment = {
    INGEST_DIR = audiobookDownloads;
    SEARCH_MODE = "universal";
  };

  systemd.tmpfiles.rules = [
    "d ${audiobookLibrary} 2775 shelfmark media - -"
    "d ${audiobookDownloads} 2775 shelfmark media - -"
    "d ${audiobookshelfState} 0750 audiobookshelf audiobookshelf - -"
  ];

  users.users.audiobookshelf.extraGroups = [ "media" ];

  environment.etc."homelab/audiobook-paths".text = ''
    audiobook_library=${audiobookLibrary}
    audiobook_downloads=${audiobookDownloads}
    audiobookshelf_state=${audiobookshelfState}
    shelfmark_state=${shelfmarkState}
  '';
}
