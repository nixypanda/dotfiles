#!/bin/bash

set -eu

ROOT_DIR="$HOME/.dotfiles"
HOSTNAME="${1:-$(hostname)}"
OUT_FILE="/tmp/nix-forecast.txt"

# Run nix-forecast, show a short preview, and save full output
nix-forecast -s --home "${ROOT_DIR}#homeConfigurations.${HOSTNAME}" |
    tee "$OUT_FILE" |
    head -4

echo
echo "Packages to be built:"

# Extract and clean package names

NIX_HASH_STRIP='s|^/nix/store/[^-]*-||'

EXT_COMPLETION='\-completion$'
EXT_ARCHIVE='\.(zip|patch|lock)$'
EXT_SCRIPT='\.(fish|sh|bash|nu|zsh)$'
EXT_METADATA='\.(json|conf|plist|service|keep|md|yml)$'

NAME_VIM_PLUGIN='^vimplugin-' # Build fast enough to not care
NAME_HOME_MANAGER='^(hm_|home-)'
NAME_FIREFOX_PLUGINS='^(bitwarden|clearurls|darkreader|facebook-container|multi-account-containers|return-youtube-dislikes|sponsorblock|temporary-containers|ublock-origin|vimium)'
NAME_PYTHON_ENV='\-env$'

grep "/nix/store/" "$OUT_FILE" |
    sed "$NIX_HASH_STRIP" |
    grep -Ev "$EXT_COMPLETION" |
    grep -Ev "$EXT_ARCHIVE" |
    grep -Ev "$EXT_SCRIPT" |
    grep -Ev "$EXT_METADATA" |
    grep -Ev "$NAME_VIM_PLUGIN" |
    grep -Ev "$NAME_HOME_MANAGER" |
    grep -Ev "$NAME_FIREFOX_PLUGINS" |
    grep -Ev "$NAME_PYTHON_ENV" |
    sort |
    nl
