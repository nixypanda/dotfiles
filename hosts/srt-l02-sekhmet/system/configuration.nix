# Only what is specific to this host. Shared Mac config lives in
# modules/mac/.
{ ... }:
{
  imports = [
    ../../../modules/mac/system.nix
    ../../../modules/mac/homebrew.nix
  ];

  networking.hostName = "srt-l02-sekhmet";
}
