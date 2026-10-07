{ pkgs }:
pkgs.writeShellScript "kitty-session-picker" (
  pkgs.lib.replaceStrings
    [
      "@find@"
      "@sort@"
      "@basename@"
      "@cut@"
      "@fzf@"
      "@kitty@"
    ]
    [
      "${pkgs.findutils}/bin/find"
      "${pkgs.coreutils}/bin/sort"
      "${pkgs.coreutils}/bin/basename"
      "${pkgs.coreutils}/bin/cut"
      "${pkgs.fzf}/bin/fzf"
      "${pkgs.kitty}/bin/kitty"
    ]
    (builtins.readFile ./kitty-session-picker.sh)
)
