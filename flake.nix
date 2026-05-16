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
    kitty-upstream = {
      url = "github:nixypanda/kitty/floating-pane-experiment";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      nur,
      vim-plugins,
      nixpkgs,
      home-manager,
      darwin,
      kitty-upstream,
      ...
    }:
    let
      kitty-dev-build-overlay = final: prev: {
        kitty-dev = prev.kitty.overrideAttrs (
          old:
          let
            version = "floating-pane-experiment-${kitty-upstream.shortRev or kitty-upstream.rev}";
            src = kitty-upstream;
          in
          {
            inherit src version;
            inherit
              (final.buildGo126Module {
                pname = "kitty-go-modules";
                inherit src version;
                vendorHash = "sha256-FaSWBeQJlvw9vXcHJ/OaFd48K8d7X86X8w7wpG84Ltw=";
              })
              goModules
              ;
            nativeBuildInputs = map (
              pkg: if (pkg.pname or "") == "go" then final.go_1_26 else pkg
            ) old.nativeBuildInputs;
            env = old.env // {
              GOTOOLCHAIN = "local";
            };
          }
        );
      };

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
                "claude-code"
                "google-chrome"
                "firefox-bin"
                "firefox-bin-unwrapped"
                "vim-table-mode"
              ];
          };

          nixpkgs.overlays = [
            kitty-dev-build-overlay
            nur.overlays.default
            vim-plugins.overlay
          ];

          # Let Home Manager install and manage itself.
          programs.home-manager.enable = true;
          home.stateVersion = "25.11";

          imports = [
            ./modules/cli.nix
            ./modules/env.nix
            ./modules/firefox
            ./modules/fonts.nix
            ./modules/git
            ./modules/kitty
            ./modules/nu
            ./modules/nvim
            ./modules/programming
            ./modules/system-management
          ];
        };

      home-macbook = {
        # Hack: Firefox does not work on mac so we have to depend on an overlay.
        nixpkgs.overlays = [ ];
        home.homeDirectory = "/Users/nixypanda";
        home.username = "nixypanda";

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
