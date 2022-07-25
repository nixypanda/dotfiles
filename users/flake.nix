{
  description = "Home manager flake";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager.url = "github:nix-community/home-manager";
    nur.url = "github:nix-community/NUR";
    taffybar.url = "github:sherubthakur/taffybar";
    # Applying the configuration happens from the .dotfiles directory so the
    # relative path is defined accordingly. This has potential of causing issues.
    vim-plugins.url = "path:./users/modules/nvim/plugins";
  };
  outputs = { self, nur, taffybar, vim-plugins, nixpkgs, home-manager }:
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

      home-macbook = { pkgs, ... }:
        {
          xdg.configFile."nix/nix.conf".text = ''
            experimental-features = nix-command flakes ca-references
          '';
          home.homeDirectory = "/Users/sherubthakur";
          home.username = "sherubthakur";
          imports = [
            ./modules/tmux.nix
          ];
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
    };
}
