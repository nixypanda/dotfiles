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
    musicBeets = "${mediaRoot}/library/music-beets";
    musicSoulSync = "${mediaRoot}/library/music-soulsync";
    books = "${mediaRoot}/library/books";
    manga = "${mediaRoot}/library/manga";
    audiobooks = "${mediaRoot}/library/audiobooks";
  };

  downloads = {
    torrents = "${mediaRoot}/downloads/torrents";
    audiobooks = "${mediaRoot}/downloads/audiobooks";
    metube = "${mediaRoot}/downloads/metube";
    metubeReview = "${mediaRoot}/downloads/metube/review";
    soulsync = "${mediaRoot}/downloads/soulsync";
  };

  cache = {
    stremioDebrid = "${mediaRoot}/cache/stremio-debrid";
  };

  state = {
    nixarr = "${stateRoot}/nixarr";
    audiobookshelf = "${stateRoot}/audiobookshelf";
    shelfmark = "${stateRoot}/shelfmark";
    metube = "${stateRoot}/metube";
    beets = "${stateRoot}/beets";
    soulsync = "${stateRoot}/soulsync";
    navidrome = "${stateRoot}/navidrome";
    kavita = "${stateRoot}/kavita";
  };
}
