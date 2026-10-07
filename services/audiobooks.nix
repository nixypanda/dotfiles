{ homelab, paths, ... }:

let
  audiobookLibrary = paths.library.audiobooks;
  audiobookDownloads = paths.downloads.audiobooks;
  audiobookshelfState = paths.state.audiobookshelf;
  shelfmarkState = paths.state.shelfmark;
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
