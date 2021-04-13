{
  description = "Home manager flake";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    home-manager.url = "github:nix-community/home-manager";
  };
  outputs = {self, ... }@inputs:
  {
    homeConfigurations = {
      nixos = inputs.home-manager.lib.homeManagerConfiguration {
        system = "x86_64-linux";
        homeDirectory = "/home/sherub";
        username = "sherub";
        configuration = { config, lib, pkgs, ... }:
        {
          nixpkgs.config = { allowUnfree = true; };
          nixpkgs.overlays = [ inputs.neovim-nightly-overlay.overlay ];

          # This value determines the Home Manager release that your
          # configuration is compatible with. This helps avoid breakage
          # when a new Home Manager release introduces backwards
          # incompatible changes.
          #
          # You can update Home Manager without changing this value. See
          # the Home Manager release notes for a list of state version
          # changes in each release.
          home.stateVersion = "20.09";

          # Let Home Manager install and manage itself.
          programs.home-manager.enable = true;


          imports = [
            ./modules/browser.nix
            ./modules/git.nix
            ./modules/desktop-environment/index.nix
            ./modules/nvim/index.nix
            ./modules/programming.nix
            ./modules/cli/index.nix
            ./modules/fonts.nix
            ./modules/kitty.nix
            ./modules/alacritty.nix
            ./modules/system-management/index.nix
          ];

          home.packages = with pkgs; [
            # GUI Apps
            slack
            discord
            p3x-onenote

            # Busybox replacements
            pciutils
            usbutils
            less

            openvpn
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
          xdg.configFile."nix/nix.conf".text = ''
            experimental-features = nix-command flakes ca-references
          '';
          nixpkgs.config = { allowUnfree = true; };
          nixpkgs.overlays = [ inputs.neovim-nightly-overlay.overlay ];

          # This value determines the Home Manager release that your
          # configuration is compatible with. This helps avoid breakage
          # when a new Home Manager release introduces backwards
          # incompatible changes.
          #
          # You can update Home Manager without changing this value. See
          # the Home Manager release notes for a list of state version
          # changes in each release.
          home.stateVersion = "20.09";

          # Let Home Manager install and manage itself.
          programs.home-manager.enable = true;


          imports = [
            ./modules/git.nix
            ./modules/nvim/index.nix
            ./modules/programming.nix
            ./modules/cli/index.nix
            ./modules/fonts.nix
            ./modules/kitty.nix
            ./modules/alacritty.nix
            ./modules/tmux.nix
            ./modules/system-management/index.nix
          ];
        };
      };
    };
  };
}
