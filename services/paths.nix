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
    books = "${mediaRoot}/library/books";
    manga = "${mediaRoot}/library/manga";
    audiobooks = "${mediaRoot}/library/audiobooks";
  };

  downloads = {
    torrents = "${mediaRoot}/downloads/torrents";
    audiobooks = "${mediaRoot}/downloads/audiobooks";
  };

  state = {
    nixarr = "${stateRoot}/nixarr";
    audiobookshelf = "${stateRoot}/audiobookshelf";
    shelfmark = "${stateRoot}/shelfmark";
    navidrome = "${stateRoot}/navidrome";
    kavita = "${stateRoot}/kavita";
  };
}
