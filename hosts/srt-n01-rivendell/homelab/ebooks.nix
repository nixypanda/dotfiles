{ config, ... }:

let
  booksLibrary = "/srv/media/books";
  mangaLibrary = "/srv/media/manga";
in
{
  age.secrets.kavitaTokenKey = {
    file = ./secrets/kavita-token-key.age;
    mode = "0400";
    owner = "root";
    group = "root";
  };

  services.kavita = {
    enable = true;
    dataDir = "/srv/.state/kavita";
    tokenKeyFile = config.age.secrets.kavitaTokenKey.path;
    settings = {
      Port = 5000;
      IpAddresses = "127.0.0.1";
    };
  };

  systemd.tmpfiles.rules = [
    "d ${booksLibrary} 2775 nixypanda media - -"
    "d ${mangaLibrary} 2775 nixypanda media - -"
  ];

  users.users.kavita.extraGroups = [ "media" ];

  environment.etc."homelab/ebook-paths".text = ''
    books_library=${booksLibrary}
    manga_library=${mangaLibrary}
  '';
}
