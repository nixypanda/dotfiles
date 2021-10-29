{
  description = "Home manager flake";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager.url = "github:nix-community/home-manager";
    nur.url = "github:nix-community/NUR";
  };
  outputs = { self, nur, ... }@inputs:
    let
      # Add beautifulsoup4 overlay
      # Until NixOS/nixpkgs#137870 fixing NixOS/nixpkgs#137678 lands in
      # nixpkgs-unstable.
      bs4 = self: super:
        let
          lib = super.lib;
        in
          rec {
            python39 = super.python39.override {
              packageOverrides = self: super: {
                beautifulsoup4 = super.beautifulsoup4.overrideAttrs (
                  old: {
                    propagatedBuildInputs = lib.remove super.lxml old.propagatedBuildInputs;
                  }
                );
              };
            };
            python39Packages = python39.pkgs;
          };
      overlays = [ nur.overlay bs4 ];
    in
      {
        homeConfigurations = {
          nixos = inputs.home-manager.lib.homeManagerConfiguration {
            system = "x86_64-linux";
            homeDirectory = "/home/sherub";
            username = "sherub";
            configuration = { config, lib, pkgs, ... }@configInput:
              {
                # NOTE: Here we are injecting colorscheme so that it is passed down all the imports
                _module.args = {
                  colorscheme = (import ./colorschemes/dracula.nix);
                };

                nixpkgs.config = { allowUnfree = true; };
                nixpkgs.overlays = overlays;

                # Let Home Manager install and manage itself.
                programs.home-manager.enable = true;

                imports = [
                  ./modules/browser.nix
                  ./modules/git.nix
                  ./modules/desktop-environment
                  ./modules/nvim
                  ./modules/cli
                  ./modules/fonts.nix
                  ./modules/kitty.nix
                  ./modules/alacritty.nix
                  ./modules/system-management
                  ./modules/work.nix
                ];

                # Packages that don't fit in the modules that we have
                home.packages = with pkgs; [
                  # GUI Apps
                  discord
                  p3x-onenote

                  # Busybox replacements: As the default ones give out very
                  # limited info which is extremely unhelpful when debugging
                  # something
                  pciutils
                  usbutils
                  less
                  stress

                  gnome3.networkmanager-openvpn
                ];
              };
          };

          macbook-pro = inputs.home-manager.lib.homeManagerConfiguration {
            system = "x86_64-darwin";
            homeDirectory = "/Users/sherubthakur";
            username = "sherubthakur";
            configuration = { config, lib, pkgs, ... }:
              {
                # NOTE: Here we are injecting colorscheme so that it is passed down all the imports
                _module.args = {
                  colorscheme = (import ./colorschemes/dracula.nix);
                };
                xdg.configFile."nix/nix.conf".text = ''
                  experimental-features = nix-command flakes ca-references
                '';
                nixpkgs.config = { allowUnfree = true; };
                nixpkgs.overlays = overlays;

                # Let Home Manager install and manage itself.
                programs.home-manager.enable = true;

                imports = [
                  ./modules/kitty.nix
                  ./modules/git.nix
                  ./modules/nvim
                  ./modules/cli
                  ./modules/fonts.nix
                  ./modules/kitty.nix
                  ./modules/alacritty.nix
                  ./modules/tmux.nix
                  ./modules/system-management
                ];
              };
          };
        };
      };
}
