{
  description = "Home manager flake";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs-firefox-darwin = {
      url = "github:bandithedoge/nixpkgs-firefox-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    taffybar = {
      url = "github:sherubthakur/taffybar";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Applying the configuration happens from the .dotfiles directory so the
    # relative path is defined accordingly. This has potential of causing issues.
    vim-plugins = {
      url = "path:./modules/nvim/plugins";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nur, taffybar, vim-plugins, nixpkgs, home-manager, darwin, nixpkgs-firefox-darwin }:
    let
      home-common = { pkgs, lib, ... }:
        {
          # NOTE: Here we are injecting colorscheme so that it is passed down all the imports
          _module.args = {
            colorscheme = import ./colorschemes/tokyonight.nix;
          };

          nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
            "zoom"
            "slack"
            "ngrok"
            "discord"
            "unrar"
            # browser extensions
            "onepassword-password-manager"
            "grammarly"
            "okta-browser-plugin"
          ];

          nixpkgs.overlays = [
            nur.overlay
            taffybar.overlay
            vim-plugins.overlay
          ];

          # Let Home Manager install and manage itself.
          programs.home-manager.enable = true;
          home.stateVersion = "22.05";

          imports = [
            ./modules/alacritty
            ./modules/aws
            ./modules/bat
            ./modules/cli.nix
            ./modules/direnv
            ./modules/firefox
            ./modules/fonts.nix
            ./modules/git
            ./modules/hashistack.nix
            ./modules/kitty
            ./modules/nvim
            ./modules/programming.nix
            ./modules/system-management
            ./modules/zsh
          ];
        };

      home-macbook = { config, pkgs, lib, ... }:
        {
          # Hack: Firefox does not work on mac so we have to depend on an overlay.
          nixpkgs.overlays = [
            nixpkgs-firefox-darwin.overlay
          ];
          home.homeDirectory = "/Users/sherubthakur";
          home.username = "sherubthakur";
          imports = [
            ./modules/nu/default-mac.nix
            ./modules/tmux
            ./modules/mac-symlink-applications.nix
          ];
          xdg.configFile."nix/nix.conf".text = ''
            experimental-features = nix-command flakes ca-references
          '';
        };

      home-linux = { pkgs, ... }:
        {
          home.homeDirectory = "/home/sherub";
          home.username = "sherub";
          imports = [
            ./modules/discord
            ./modules/media.nix
            ./modules/nu/default-linux.nix
            ./modules/onenote
            ./modules/slack

            # Desktop Environment
            ./modules/desktop-environment.nix
            ./modules/betterlockscreen
            ./modules/colorscheme-based-background
            ./modules/deadd
            ./modules/eww
            ./modules/gtk
            ./modules/picom
            ./modules/plasma-browser-integration
            ./modules/rofi
            ./modules/taffybar
            ./modules/xidlehook
            ./modules/xmonad
          ];
        };

    in
    {

      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./system/configuration.nix ];
      };

      homeConfigurations = {
        nixos =
          let
            system = "x86_64-linux";
            pkgs = nixpkgs.legacyPackages.${system};
          in
          home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [
              home-common
              home-linux
            ];
          };

        macbook-pro =
          let
            system = "x86_64-darwin";
            pkgs = nixpkgs.legacyPackages.${system};
          in
          home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [
              home-common
              home-macbook
            ];
          };
      };
      # WARN: The intent here is to only use brew and nothing else.
      # I have no idea what I am doing here. This setup is making using of
      # nix-darwin which apperently has brew. Brew can be a potent practical
      # fallback when the nix world is not so great no macos.
      darwinConfigurations = {
        "Sherubs-MacBook-Pro-2" =
          let
            system = "x86_64-darwin";
            pkgs = nixpkgs.legacyPackages.${system};
          in
          darwin.lib.darwinSystem {
            inherit pkgs;
            modules = [
              ./modules/homebrew.nix
            ];
          };
      };
    };
}
