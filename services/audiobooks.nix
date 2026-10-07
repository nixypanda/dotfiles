{ homelab, ... }:

let
  audiobookLibrary = "/srv/media/library/audiobooks";
  audiobookDownloads = "/srv/media/downloads/audiobooks";
  audiobookshelfState = "/srv/.state/audiobookshelf";
  shelfmarkState = "/srv/.state/shelfmark";
in
{
  nixarr = {
    audiobookshelf = {
      enable = true;
      host = "127.0.0.1";
      port = homelab.services.audiobookshelf.local;
      stateDir = audiobookshelfState;
      openFirewall = false;
    };

    shelfmark = {
      enable = true;
      host = "127.0.0.1";
      port = homelab.services.shelfmark.local;
      stateDir = shelfmarkState;
      openFirewall = false;
    };
  };

  services.shelfmark.environment = {
    INGEST_DIR = audiobookDownloads;
    SEARCH_MODE = "universal";
  };

  systemd.tmpfiles.rules = [
    "d ${audiobookLibrary} 2775 shelfmark media - -"
    "d ${audiobookDownloads} 2775 shelfmark media - -"
  ];

  environment.etc."homelab/audiobook-paths".text = ''
    audiobook_library=${audiobookLibrary}
    audiobook_downloads=${audiobookDownloads}
    audiobookshelf_state=${audiobookshelfState}
    shelfmark_state=${shelfmarkState}
  '';
}
