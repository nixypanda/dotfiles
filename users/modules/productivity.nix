{ pkgs, ... }:
{
  home.packages = with pkgs; [
  ] ++ (if pkgs.stdenv.isLinux then [
    p3x-onenote
  ] else [

  ]);
}
