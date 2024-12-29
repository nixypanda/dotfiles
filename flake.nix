{
  description = "Home manager flake";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur = {
      url = "github:nix-community/NUR";
    };
    # Applying the configuration happens from the .dotfiles directory so the
    # relative path is defined accordingly. This has potential of causing issues.
    vim-plugins = {
      url = "path:/Users/nixypanda/.dotfiles/modules/nvim/plugins";
    };
    zjstatus = {
      url = "github:dj95/zjstatus";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # MacOS specific inputs
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs-firefox-darwin = {
      url = "github:bandithedoge/nixpkgs-firefox-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mac-app-util = {
      url = "github:hraban/mac-app-util";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };
  outputs =
    {
      self,
      nur,
      vim-plugins,
      nixpkgs,
      home-manager,
      zjstatus,

      darwin,
      nixpkgs-firefox-darwin,
      mac-app-util,
    }:
    let
      home-common =
        { lib, ... }:
        {
          # NOTE: Injecting colorscheme so that it is passed down all the imports
          _module.args = {
            colorscheme = import ./colorschemes/tokyonight.nix;
          };
          nixpkgs.config = {

            allowUnfreePredicate =
              pkg:
              builtins.elem (lib.getName pkg) [
                "zoom"
                "unrar"
                "codeium"
                "terraform"
                # browser extensions
                "onepassword-password-manager"
                "okta-browser-plugin"
              ];
          };

          nixpkgs.overlays = [
            nur.overlay
            vim-plugins.overlay
            (final: prev: { zjstatus = zjstatus.packages.${prev.system}.default; })
          ];

          # Let Home Manager install and manage itself.
          programs.home-manager.enable = true;
          home.stateVersion = "22.05";

          imports = [
            ./modules/cli.nix
            ./modules/firefox
            ./modules/fonts.nix
            ./modules/git
            ./modules/kitty
            ./modules/nu
            ./modules/nvim
            ./modules/programming.nix
            ./modules/system-management
            ./modules/zellij
            ./modules/zsh
          ];
        };

      home-macbook = {
        # Hack: Firefox does not work on mac so we have to depend on an overlay.
        nixpkgs.overlays = [ nixpkgs-firefox-darwin.overlay ];
        home.homeDirectory = "/Users/nixypanda";
        home.username = "nixypanda";
        imports = [ mac-app-util.homeManagerModules.default ];
        xdg.configFile."nix/nix.conf".text = ''
          experimental-features = nix-command flakes
        '';
      };

      home-linux = {
        home.homeDirectory = "/home/sherub";
        home.username = "sherub";
        imports = [
          # Desktop Environment
          ./modules/linux/desktop-environment.nix
          ./modules/linux/betterlockscreen
          ./modules/linux/colorscheme-based-background
          ./modules/linux/deadd
          ./modules/linux/eww
          ./modules/linux/gtk
          ./modules/linux/picom
          ./modules/linux/plasma-browser-integration
          ./modules/linux/rofi
          ./modules/linux/taffybar
          ./modules/linux/xidlehook
          ./modules/linux/xmonad
        ];
      };

    in
    {
      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./system/nixos/configuration.nix ];
      };

      homeConfigurations = {
        nixos = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages."x86_64-linux";
          modules = [
            home-common
            home-linux
          ];
        };

        srt-l02-sekhmet = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages."x86_64-darwin";
          modules = [
            home-common
            home-macbook
          ];
        };
      };

      darwinConfigurations."srt-l02-sekhmet" = darwin.lib.darwinSystem {
        pkgs = nixpkgs.legacyPackages."x86_64-darwin";
        modules = [
          ./modules/mac/configuration.nix
          ./modules/mac/yabai.nix
          ./modules/mac/skhd.nix
          ./modules/mac/homebrew.nix
        ];
      };
    };
}
