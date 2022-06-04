{
  description = "Home manager flake";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager.url = "github:nix-community/home-manager";
    nur.url = "github:nix-community/NUR";
    taffybar.url = "github:sherubthakur/taffybar";

    # Nvim plugins
    nvim-dap-python-src = {
      url = "github:mfussenegger/nvim-dap-python";
      flake = false;
    };
    nvim-alpha-src = {
      url = "github:goolord/alpha-nvim";
      flake = false;
    };
    nvim-sqls-src = {
      url = "github:nanotee/sqls.nvim";
      flake = false;
    };
    nvim-yuck-src = {
      url = "github:elkowar/yuck.vim";
      flake = false;
    };
    nvim-better-digraphs-src = {
      url = "github:protex/better-digraphs.nvim";
      flake = false;
    };
    nvim-nu-src = {
      url = "github:LhKipp/nvim-nu";
      flake = false;
    };

  };
  outputs = { self, nur, taffybar, ... }@inputs:
    let
      missingVimPluginsInNixpkgs = pkgs: {
        nvim-dap-python = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-dap-python";
          src = inputs.nvim-dap-python-src;
        };
        nvim-alpha = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-alpha";
          src = inputs.nvim-alpha-src;
        };
        nvim-sqls = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-sqls";
          src = inputs.nvim-sqls-src;
        };
        nvim-yuck = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-yuck";
          src = inputs.nvim-yuck-src;
        };
        nvim-better-digraphs = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-better-digraphs";
          src = inputs.nvim-better-digraphs-src;
        };
        nvim-nu = pkgs.vimUtils.buildVimPlugin {
          name = "nvim-nu";
          src = inputs.nvim-nu-src;
        };
      };

      overlays = [
        nur.overlay
        taffybar.overlay

        (final: prev: {
          vimPlugins = prev.vimPlugins // (missingVimPluginsInNixpkgs prev.pkgs);
        })
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
                colorscheme = (import ./colorschemes/tokyonight.nix);
              };

              nixpkgs.config.allowUnfreePredicate = (unfreePredicate lib);
              nixpkgs.overlays = overlays;

              # Let Home Manager install and manage itself.
              programs.home-manager.enable = true;

              imports = common_modules ++ [
                ./modules/browser.nix
                ./modules/desktop-environment
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
                colorscheme = (import ./colorschemes/tokyonight.nix);
              };
              xdg.configFile."nix/nix.conf".text = ''
                experimental-features = nix-command flakes ca-references
              '';
              nixpkgs.config.allowUnfreePredicate = (unfreePredicate lib);
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
