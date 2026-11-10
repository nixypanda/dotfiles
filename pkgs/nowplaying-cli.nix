{
  lib,
  stdenv,
  fetchFromGitHub,
}:
stdenv.mkDerivation {
  pname = "nowplaying-cli";
  version = "1.2.1-unstable-2026-04-06";

  src = fetchFromGitHub {
    owner = "kirtan-shah";
    repo = "nowplaying-cli";
    rev = "8c8c1fa4820681fd4bbd6a17ce0a5655e1f4ebe7";
    hash = "sha256-jPW3WEq1ZhxBojMO+5WF8ohO1rLmlAJKGdh1HfSOR5s=";
  };

  postPatch = ''
    # Home Manager exposes the executable through profile symlinks.
    substituteInPlace src/nowplaying.mm \
      --replace-fail 'return [path stringByDeletingLastPathComponent];' \
      'return [[path stringByResolvingSymlinksInPath] stringByDeletingLastPathComponent];'
  '';

  makeFlags = [ "PREFIX=$(out)" ];

  meta = {
    description = "Control and inspect the active macOS media session";
    homepage = "https://github.com/kirtan-shah/nowplaying-cli";
    license = lib.licenses.gpl3Only;
    platforms = lib.platforms.darwin;
    mainProgram = "nowplaying-cli";
  };
}
