# Think of porting these to sessionVariables key in home.nix
export LESS=-R
export LESS_TERMCAP_mb=$'\E[1;31m'     # begin blink
export LESS_TERMCAP_md=$'\E[1;36m'     # begin bold
export LESS_TERMCAP_me=$'\E[0m'        # reset bold/blink
export LESS_TERMCAP_so=$'\E[01;44;33m' # begin reverse video
export LESS_TERMCAP_se=$'\E[0m'        # reset reverse video
export LESS_TERMCAP_us=$'\E[1;32m'     # begin underline
export LESS_TERMCAP_ue=$'\E[0m'        # reset underline

export KEYTIMEOUT=1

export DOTFILES=~/.dotfiles

# export ZSH=~/.oh-my-zsh
export NVIM_TUI_ENABLE_TRUE_COLOR=1
export BAT_THEME="OneHalfDark"
export EDITOR="nvim"
export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"
# export SSL_CERT_FILE=~/.nix-profile/etc/ssl/certs/ca-bundle.crt

export PATH="$HOME/.local/bin:$HOME/.nix-profile/bin:$PATH"
# Add python site-packages to path
export NIX_PATH="$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH"
export FPATH=~/.nix-profile/share/zsh/site-functions:$FPATH
