{
  description = "Home manager flake";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    home-manager.url = "github:nix-community/home-manager";
  };
  outputs = {self, ... }@inputs:
  let
    overlays = [inputs.neovim-nightly-overlay.overlay];
  in {
    homeConfigurations = {
      nixos = inputs.home-manager.lib.homeManagerConfiguration {
        system = "x86_64-linux";
        homeDirectory = "/home/sherub";
        username = "sherub";
        configuration = { config, lib, pkgs, colorscheme, ... }:

        let
          colorscheme = (import ./colorschemes/onedark.nix);
        in
        {
          nixpkgs.config = {
            allowUnfree = true;
          };
          nixpkgs.overlays = overlays;
          imports = [
            ./modules/browser.nix
            ./modules/git.nix
            ./modules/desktop-environment/index.nix
            ./modules/nvim/index.nix
            ./modules/programming.nix
            ./modules/cli/index.nix
          ];

          home.packages = with pkgs; [
            # GUI Apps
            slack
            discord
            sxiv
            exiv2
            imagemagick

            p3x-onenote
            # file browser
            ranger
            # screenshot utility
            scrot

            # Fonts
            (nerdfonts.override { fonts = [ "Hack" ]; })

            # Docker
            docker-compose

            openvpn
            gnome3.networkmanager-openvpn

          ];

          # Home Manager needs a bit of information about you and the
          # paths it should manage.
          # home.username = "sherub";
          # home.homeDirectory = "/home/sherub";

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

          programs.alacritty = {
            enable = true;
            settings = (import ./alacritty/config.nix) { colors = colorscheme; };
          };

          programs.kitty = {
            enable = true;
            settings = {
              font_size = 10;
              shell = "zsh";
              scrollback_lines = 10000;
              input_delay = 1;

              foreground = "${colorscheme.fg-primary}";
              background = "${colorscheme.bg-primary}";

              color0  = "${colorscheme.black}";
              color1  = "${colorscheme.red}";
              color2  = "${colorscheme.green}";
              color3  = "${colorscheme.yellow}";
              color4  = "${colorscheme.blue}";
              color5  = "${colorscheme.magenta}";
              color6  = "${colorscheme.cyan}";
              color7  = "${colorscheme.white}";
              color8  = "${colorscheme.bright-black}";
              color9  = "${colorscheme.bright-red}";
              color10 = "${colorscheme.bright-green}";
              color11 = "${colorscheme.bright-yellow}";
              color12 = "${colorscheme.bright-blue}";
              color13 = "${colorscheme.bright-magenta}";
              color14 = "${colorscheme.bright-cyan}";
              color15 = "${colorscheme.bright-white}";
            };
          };

          home.file.".config/sxiv/exec/image-info".text = ''
            ${builtins.readFile ./sxiv/image_info.sh}
          '';

          xresources = {
            properties = {
              "*.foreground" = colorscheme.fg-primary;
              "*.background" = colorscheme.bg-primary;

              "*.color0"  = colorscheme.black;
              "*.color1"  = colorscheme.red;
              "*.color2"  = colorscheme.green;
              "*.color3"  = colorscheme.yellow;
              "*.color4"  = colorscheme.blue;
              "*.color5"  = colorscheme.magenta;
              "*.color6"  = colorscheme.cyan;
              "*.color7"  = colorscheme.white;

              "*.color8"  = colorscheme.bright-black;
              "*.color9"  = colorscheme.bright-red;
              "*.color10" = colorscheme.bright-green;
              "*.color11" = colorscheme.bright-yellow;
              "*.color12" = colorscheme.bright-blue;
              "*.color13" = colorscheme.bright-magenta;
              "*.color14" = colorscheme.bright-cyan;
              "*.color15" = colorscheme.bright-white;

              "XTerm*font" = "xft:Hack Nerd Font Mono:pixelsize=12";
              "*.internalBorder" = 4;

              "Xft.dpi" = 96;
              "Xft.antialias" = true;
              "Xft.hinting" = true;
              "Xft.rgba" = "rgb";
              "Xft.autohint" = false;
              "Xft.hintstyle" = "hintslight";
              "Xft.lcdfilter" = "lcddefault";
            };
          };
        };
        };
      };
    };
  }
