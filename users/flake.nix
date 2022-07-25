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
  outputs = { self, nur, taffybar, vim-plugins, ... }@inputs:
    let
      overlays = [
        nur.overlay
        taffybar.overlay
        vim-plugins.overlay
      ];

      unfreePredicate = lib: pkg: builtins.elem (lib.getName pkg) [
        "zoom"
        "slack"
        "ngrok"
        "discord"
        "unrar"
      ];

      common_modules = [
        ./modules/alacritty.nix
        ./modules/cli
        ./modules/fonts.nix
        ./modules/git.nix
        ./modules/kitty.nix
        ./modules/nvim
        ./modules/programming.nix
        ./modules/system-management
        ./modules/work.nix
      ];
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
                colorscheme = import ./colorschemes/tokyonight.nix;
              };

              nixpkgs.config.allowUnfreePredicate = unfreePredicate lib;
              nixpkgs.overlays = overlays;

              # Let Home Manager install and manage itself.
              programs.home-manager.enable = true;

              imports = common_modules ++ [
                ./modules/browser.nix
                ./modules/desktop-environment
                ./modules/media.nix
              ];

              # Packages that don't fit in the modules that we have
              home.packages = with pkgs; [
                discord
                p3x-onenote
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
                colorscheme = import ./colorschemes/tokyonight.nix;
              };
              xdg.configFile."nix/nix.conf".text = ''
                experimental-features = nix-command flakes ca-references
              '';
              nixpkgs.config.allowUnfreePredicate = unfreePredicate lib;
              nixpkgs.overlays = overlays;

              # Let Home Manager install and manage itself.
              programs.home-manager.enable = true;

              home.packages = with pkgs; [
                docker
              ];

              imports = common_modules ++ [
                ./modules/tmux.nix
              ];
            };
        };
      };
    };
}
