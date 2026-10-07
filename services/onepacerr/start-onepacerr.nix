{
  pkgs,
  onepacerr,
  torrentPassword,
  jellyfinPassword,
}:
pkgs.writeShellScript "start-onepacerr" ''
  export TORRENT_PASSWORD="$(${pkgs.lib.getExe' pkgs.coreutils "cat"} ${torrentPassword})"
  export JELLYFIN_PASSWORD="$(${pkgs.lib.getExe' pkgs.coreutils "cat"} ${jellyfinPassword})"

  exec ${pkgs.lib.getExe onepacerr}
''
