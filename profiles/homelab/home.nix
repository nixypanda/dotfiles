# Role: a headless homelab service host.
# The user configuration a homelab host gets. Kept deliberately small: no
# GUI programs or desktop integrations.
{ ... }:
{
  imports = [
    ../../modules/home/cli.nix
    ../../modules/home/env.nix
    ../../modules/home/git.nix
    ../../modules/home/hledger.nix
    ../../modules/home/nvim/minimal.nix
    ../../modules/home/theme.nix
  ];
}
