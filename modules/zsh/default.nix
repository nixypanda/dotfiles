{ pkgs, ... }:
{
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    defaultKeymap = "viins";
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    shellAliases = import ./aliases.nix;
    history.extended = true;
    oh-my-zsh = {
      enable = true;
      plugins = [
        "git"
        "vi-mode"
        "web-search"
        "aws"
        "terraform"
        "nomad"
        "vault"
      ];
    };

    initExtraBeforeCompInit = # sh
      ''
        ${builtins.readFile ./session_variables.zsh}
        ${if pkgs.stdenv.isDarwin then builtins.readFile ./session_variables.mac.zsh else ""}
        ${builtins.readFile ./functions.zsh}
        ${builtins.readFile ../../.secrets/env-vars.sh}

        bindkey -M vicmd 'k' history-beginning-search-backward
        bindkey -M vicmd 'j' history-beginning-search-forward

        eval "$(direnv hook zsh)"
        eval "$(zoxide init zsh)"
      '';
  };
}
