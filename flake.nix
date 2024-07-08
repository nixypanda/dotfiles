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
    };
    taffybar = {
      url = "github:nixypanda/taffybar";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # NOTE: https://github.com/NixOS/nixpkgs/pull/211321
    # Using alternate nixpkgs which has a fixed version of codelldb which works on Mac
    # Caveat: This requires Xcode.app installed on the system
    nixpkgs_codelldb_fixed = {
      url = "github:nixypanda/nixpkgs/171e2c61fb27dadc865a5fe623c5d2eca4cfd4b9";
    };

    # Applying the configuration happens from the .dotfiles directory so the
    # relative path is defined accordingly. This has potential of causing issues.
    vim-plugins = {
      url = "path:./modules/nvim/plugins";
    };
  };
  outputs =
    {
      self,
      nur,
      taffybar,
      vim-plugins,
      nixpkgs,
      home-manager,
      darwin,
      nixpkgs-firefox-darwin,
      nixpkgs_codelldb_fixed,
    }:
    let
      home-common =
        { lib, ... }:
        {
          # NOTE: Injecting colorscheme so that it is passed down all the imports
          _module.args = {
            colorscheme = import ./colorschemes/tokyonight.nix;
            codelldb_fixed_pkgs = nixpkgs_codelldb_fixed.legacyPackages."x86_64-darwin";
          };
          nixpkgs.config = {

            allowUnfreePredicate =
              pkg:
              builtins.elem (lib.getName pkg) [
                "zoom"
                "ngrok"
                "unrar"
                "vscode"
                "vscode-extension-MS-python-vscode-pylance"
                "codeium"
                # browser extensions
                "onepassword-password-manager"
                "okta-browser-plugin"
              ];
          };

          nixpkgs.overlays = [
            nur.overlay
            taffybar.overlay
            vim-plugins.overlay
          ];

          # Let Home Manager install and manage itself.
          programs.home-manager.enable = true;
          home.stateVersion = "22.05";

          imports = [
            ./modules/aws
            ./modules/bat
            ./modules/cli.nix
            ./modules/direnv
            ./modules/firefox
            ./modules/fonts.nix
            ./modules/git
            ./modules/helix
            ./modules/kitty
            ./modules/nu
            ./modules/nvim
            ./modules/programming.nix
            ./modules/system-management
            ./modules/zellij
            ./modules/zsh
            ./modules/vscode
          ];
        };

      home-macbook = {
        # Hack: Firefox does not work on mac so we have to depend on an overlay.
        nixpkgs.overlays = [ nixpkgs-firefox-darwin.overlay ];
        home.homeDirectory = "/Users/sherubthakur";
        home.username = "sherubthakur";
        imports = [
          ./modules/tmux
          ./modules/mac-symlink-applications.nix
        ];
        xdg.configFile."nix/nix.conf".text = ''
          experimental-features = nix-command flakes
        '';
      };

      home-linux = {
        home.homeDirectory = "/home/sherub";
        home.username = "sherub";
        imports = [
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
        nixos = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages."x86_64-linux";
          modules = [
            home-common
            home-linux
          ];
        };

        macbook-pro = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages."x86_64-darwin";
          modules = [
            home-common
            home-macbook
          ];
        };
      };

      darwinConfigurations."nixyMac" = darwin.lib.darwinSystem {
        pkgs = nixpkgs.legacyPackages."x86_64-darwin";
        modules = [
          ./modules/system-mac
          ./modules/homebrew.nix
        ];
      };
    };
}
