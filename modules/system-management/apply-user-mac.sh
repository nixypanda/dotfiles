#!/bin/bash

set -e

if [[ -d "$HOME/Applications/Home Manager Apps.backup" ]]; then
    rm -rf "$HOME/Applications/Home Manager Apps.backup"
fi

pushd ~/.dotfiles
home-manager switch --flake "./#nixyMac" -b backup
popd
