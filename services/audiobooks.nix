{
  homelab,
  paths,
  pkgs,
  ...
}:

let
  audiobookLibrary = paths.library.audiobooks;
  audiobookDownloads = paths.downloads.audiobooks;
  ebookLibrary = paths.library.books;
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
      package = pkgs.audiobookshelf.overrideAttrs (oldAttrs: {
        # Audiobookshelf hard-codes its socket-connection toasts to the
        # bottom-center of the screen. Move them to the top-right, matching
        # the rest of the in-app toasts.
        postInstall = (oldAttrs.postInstall or "") + ''
          if [ -d "$out/lib/node_modules/audiobookshelf/client/dist" ]; then
            find "$out/lib/node_modules/audiobookshelf/client/dist" -type f -name '*.js' -print0 \
              | xargs -0 sed -i "s/'bottom-center'/'top-right'/g"
          fi
        '';
      });
    };

    shelfmark = {
      enable = true;
      host = "127.0.0.1";
      port = homelab.services.shelfmark.local;
      stateDir = shelfmarkState;
      openFirewall = false;
    };
  };

  # Shelfmark uses INGEST_DIR as the book destination. Point ebooks at Kavita's
  # library and keep audiobooks in their own download directory for review.
  services.shelfmark.environment = {
    INGEST_DIR = ebookLibrary;
    DESTINATION_AUDIOBOOK = audiobookDownloads;
    SUPPORTED_FORMATS = "epub,pdf,cbz,cbr";
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
