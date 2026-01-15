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
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Applying the configuration happens from the.dotfiles directory so the
    # relative path is defined accordingly. This has potential of causing issues.
    vim-plugins = {
      url = "path:/Users/nixypanda/.dotfiles/modules/nvim/plugins";
    };
    # MacOS specific inputs
    darwin = {
      url = "github:LnL7/nix-darwin";
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
      darwin,
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
                "codeium"
                "windsurf"
                "cursor"
                "cursor-cli"
                "google-chrome"
              ];
          };

          nixpkgs.overlays = [
            nur.overlays.default
            vim-plugins.overlay
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
          ];
        };

      home-macbook = {
        # Hack: Firefox does not work on mac so we have to depend on an overlay.
        nixpkgs.overlays = [ ];
        home.homeDirectory = "/Users/nixypanda";
        home.username = "nixypanda";

        targets.darwin.copyApps.enable = true;
        targets.darwin.linkApps.enable = false;

        imports = [
          ./modules/mac/gui-apps.nix
        ];
        xdg.configFile."nix/nix.conf".text = ''
          experimental-features = nix-command flakes
        '';
      };

    in
    {
      homeConfigurations = {
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
