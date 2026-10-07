# Role: a graphical development workstation (macOS).
# The reusable user configuration a workstation gets. Host-specific bits
# (home directory, extra GUI packages, DSH desktop patch) stay in the host dir.
{ ... }:
{
  imports = [
    ../../modules/home/claude
    ../../modules/home/cli.nix
    ../../modules/home/dsh
    ../../modules/home/env.nix
    ../../modules/home/firefox
    ../../modules/home/git.nix
    ../../modules/home/hledger.nix
    ../../modules/home/kitty
    ../../modules/home/kitty/dev.nix
    ../../modules/home/nu
    ../../modules/home/nvim
    ../../modules/home/programming.nix
    ../../modules/home/rumdl
    ../../modules/home/system-management
    ../../modules/home/theme.nix
    ../../modules/home/vale
  ];
}
