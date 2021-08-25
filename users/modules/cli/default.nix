{ config, pkgs, lib, colorscheme, ... }:
{
  home.packages = with pkgs; [
    # CLI tools / Terminal facification
    awscli
    ngrok
    gnumake
    docker-compose
    tokei

    # Moar colors
    starship
    zsh-syntax-highlighting

    # Searching/Movement helpers
    fzf
    zoxide
    ripgrep
    universal-ctags

    # system info
    bottom
    neofetch
  ];

  programs.bat = {
    enable = true;
    config.theme = colorscheme.bat-theme-name;
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    nix-direnv.enableFlakes = true;
  };

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
    # ${builtins.readFile ./zsh/secrets.zsh}
    # ${builtins.readFile ../../../.secrets/env-vars.sh}
    initExtraBeforeCompInit = ''
      ${builtins.readFile ./zsh/session_variables.zsh}
      ${builtins.readFile ./zsh/functions.zsh}

      eval "$(direnv hook zsh)"

      bindkey -M vicmd 'k' history-beginning-search-backward
      bindkey -M vicmd 'j' history-beginning-search-forward

      eval "$(zoxide init zsh)"

      eval "$(starship init zsh)"
    '';
  };
}
