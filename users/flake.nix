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
          ];

          home.packages = with pkgs; [
            # GUI Apps
            slack
            discord
            sxiv
            exiv2
            imagemagick

            p3x-onenote

            # CLI tools / Terminal facification
            awscli
            ngrok
            gnumake
            # Moar colors
            starship
            zsh-syntax-highlighting
            # Searching/Movement helpers
            fzf
            zoxide
            ripgrep
            universal-ctags
            xcwd
            # system info
            bottom
            neofetch
            # file browser
            ranger
            # screenshot utility
            scrot

            # Busybox replacements
            usbutils
            pciutils
            less
            tokei

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

          programs.bat = {
            enable = true;
            config.theme = colorscheme.bat-theme-name;
          };

          programs.direnv = {
            enable = true;
            enableNixDirenvIntegration = true;
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

          programs.nushell = {
            enable = true;
            settings = {
              edit_mode = "vi";
              # figure out how to use z add here
              prompt = "za $(pwd) | echo $(starship prompt)";
              startup = [
                "def z [a] {if $(echo $a | empty?) == $true { cd ~ } {echo $a | each { if $it == '-' { cd - } {cd $(zoxide query -- $it | str trim)}}}}"
                "def zi [a] {echo $(do -i {zoxide query -i $a} | str trim) | each { cd $it }}"
                "def za [a:block] {zoxide add $a}"
                "def zq [a] {zoxide query -- $a}"
                "def zl [] {zoxide query --list}"
                "def zqi [a] {echo $(do -i {zoxide query -i $a | str trim }) | each { cd $it }}"
                "def zr [a] {zoxide remove $a}"
                "def zri [a] {zoxide remove -i $a}"
                "def dce [container command] { docker-compose exec $container /bin/bash -c '$command' }"
                "def dcpytest [container] { docker-compose exec $container /bin/bash -c 'pytest' }"
                "def dcpytestlf [container] { docker-compose exec $container /bin/bash -c 'pytest --lf' }"
                "def dcpytestni [container] { docker-compose exec $container /bin/bash -c 'pytest -m \"not integration\"' }"
              ] ++ (lib.mapAttrsToList (k: v: "alias ${k} = ${v}") (import ./zsh/aliases.nix));
              color_config = {
                # green|g, red|r, blue|u, black|b, yellow|y, purple|p, cyan|c, white|w
                primitive_int = "wb";
                primitive_decimal = "wb";
                primitive_boolean = "cyan";
                primitive_binary = "cyan";
                primitive_date = "wd";
                primitive_filesize = "rb";
                primitive_string = "gb";
                primitive_line = "yellow";
                primitive_columnpath = "cyan";
                primitive_pattern = "white";
                primitive_duration = "blue";
                primitive_range = "purple";
                primitive_path = "ub";
                separator_color = "grey";
                header_align = "l"; # left|l, right|r, center|c
                header_color = "blue";
                header_bold = true;
                index_color = "wd";
                # leading_trailing_space_bg = "white";
              };
            };
          };

          programs.zsh = {
            enable = true;
            enableCompletion = true;
            defaultKeymap = "viins";
            enableAutosuggestions = true;
            shellAliases = (import ./zsh/aliases.nix);
            history.extended = true;
            plugins = [
              {
                name = "zsh-syntax-highlighting";
                src = pkgs.fetchFromGitHub {
                  owner = "zsh-users";
                  repo = "zsh-syntax-highlighting";
                  rev = "be3882aeb054d01f6667facc31522e82f00b5e94";
                  sha256 = "0w8x5ilpwx90s2s2y56vbzq92ircmrf0l5x8hz4g1nx3qzawv6af";
                };
              }
            ];
            oh-my-zsh = {
              enable = true;
              plugins = [ "git" "vi-mode" ];
            };

            # For this to work with flakes we need to get this into git
            # TODO: Figure out how to use git-crypt
            # ${builtins.readFile ./zsh/secrets.zsh}
            initExtraBeforeCompInit = ''
              ${builtins.readFile ./zsh/session_variables.zsh}
              ${builtins.readFile ./zsh/functions.zsh}

              eval "$(direnv hook zsh)"

              bindkey -M vicmd 'k' history-beginning-search-backward
              bindkey -M vicmd 'j' history-beginning-search-forward

              alias ls="ls --color=auto -F"
              eval "$(zoxide init zsh)"

              eval "$(starship init zsh)"
            '';
          };

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