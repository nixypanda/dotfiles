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
      url = "path:./users/modules/nvim/plugins";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nur, taffybar, vim-plugins, nixpkgs, home-manager, darwin }:
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
            ./modules/alacritty.nix
            ./modules/cli
            ./modules/fonts.nix
            ./modules/git.nix
            ./modules/kitty.nix
            ./modules/nvim
            ./modules/programming.nix
            ./modules/productivity.nix
            ./modules/social.nix
            ./modules/system-management
            ./modules/work.nix
          ];
        };

      home-macbook = { config, pkgs, lib, ... }:
        {
          home.homeDirectory = "/Users/sherubthakur";
          home.username = "sherubthakur";
          imports = [
            ./modules/tmux.nix
          ];
          xdg.configFile."nix/nix.conf".text = ''
            experimental-features = nix-command flakes ca-references
          '';
          # Symlink macos applications. This does not happen by default.
          # https://github.com/nix-community/home-manager/issues/1341
          home.activation = {
            copyApplications =
              let
                apps = pkgs.buildEnv {
                  name = "home-manager-applications";
                  paths = config.home.packages;
                  pathsToLink = "/Applications";
                };
              in
              lib.hm.dag.entryAfter [ "writeBoundary" ] ''
                baseDir="$HOME/Applications/Home Manager Apps"
                if [ -d "$baseDir" ]; then
                  rm -rf "$baseDir"
                fi
                mkdir -p "$baseDir"
                for appFile in ${apps}/Applications/*; do
                  target="$baseDir/$(basename "$appFile")"
                  $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -fHRL "$appFile" "$baseDir"
                  $DRY_RUN_CMD chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
                done
              '';
          };
        };

      home-linux = { pkgs, ... }:
        {
          home.homeDirectory = "/home/sherub";
          home.username = "sherub";
          imports = [
            ./modules/browser.nix
            ./modules/desktop-environment
            ./modules/media.nix
          ];
        };

    in
    {
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
