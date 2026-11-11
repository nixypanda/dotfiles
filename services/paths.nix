let
  mediaRoot = "/srv/media";
  stateRoot = "/srv/.state";
in
{
  inherit mediaRoot stateRoot;

  library = {
    movies = "${mediaRoot}/library/movies";
    shows = "${mediaRoot}/library/shows";
    music = "${mediaRoot}/library/music";
    musicSoulSync = "${mediaRoot}/library/music-soulsync";
    books = "${mediaRoot}/library/books";
    manga = "${mediaRoot}/library/manga";
    audiobooks = "${mediaRoot}/library/audiobooks";
  };

  downloads = {
    torrents = "${mediaRoot}/downloads/torrents";
    audiobooks = "${mediaRoot}/downloads/audiobooks";
    soulsync = "${mediaRoot}/downloads/soulsync";
  };

  cache = {
    stremioDebrid = "${mediaRoot}/cache/stremio-debrid";
  };

  state = {
    nixarr = "${stateRoot}/nixarr";
    audiobookshelf = "${stateRoot}/audiobookshelf";
    shelfmark = "${stateRoot}/shelfmark";
    soulsync = "${stateRoot}/soulsync";
    navidrome = "${stateRoot}/navidrome";
    kavita = "${stateRoot}/kavita";
    homeAssistant = "${stateRoot}/home-assistant";
  };
}
