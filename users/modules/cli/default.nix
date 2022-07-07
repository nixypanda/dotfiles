{ config, pkgs, lib, colorscheme, ... }:
{
  home.packages = with pkgs; [
    # CLI tools / Terminal facification
    awscli
    ngrok
    gnumake
    docker-compose
    tokei
    dig
    wireshark
    unzip
    jq
    act
    fx
    dua
    procs
    hexyl
    nix-du
    graphviz
    unrar
    bashmount

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

    # benchmarking
    httperf

    arandr
  ];

  programs.bat = {
    enable = true;
    config.theme = colorscheme.bat-theme-name;
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    stdlib = ''
      ${builtins.readFile ./direnv/project_layouts/poetry.sh}
    '';
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.nushell = {
    enable = true;
  };
  home.file."Library/Application Support/nushell/env.nu".source = ./nu/env.nu;
  home.file."Library/Application Support/nushell/config.nu".source = ./nu/config.nu;

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    defaultKeymap = "viins";
    enableAutosuggestions = true;
    enableSyntaxHighlighting = true;
    shellAliases = import ./zsh/aliases.nix;
    history.extended = true;
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "vi-mode" "web-search" "aws" "terraform" "nomad" "vault" ];
    };

    initExtraBeforeCompInit = ''
      ${builtins.readFile ./zsh/session_variables.zsh}
      ${builtins.readFile ./zsh/functions.zsh}
      ${builtins.readFile ../../../.secrets/env-vars.sh}

      eval "$(direnv hook zsh)"

      bindkey -M vicmd 'k' history-beginning-search-backward
      bindkey -M vicmd 'j' history-beginning-search-forward

      eval "$(zoxide init zsh)"

      eval "$(starship init zsh)"
    '';
  };
}
