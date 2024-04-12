#!/bin/bash

set -e

if [[ -d "$HOME/Applications/Home Manager Apps.backup" ]]; then
    rm -rf "$HOME/Applications/Home Manager Apps.backup"
fi

with_brew=false
while [[ $# -gt 0 ]]; do
    case $1 in
    -b | --with-brew) with_brew=true ;;
    *)
        echo "Unknown parameter passed: $1"
        exit 1
        ;;
    esac
    shift
done

pushd ~/.dotfiles
home-manager switch --flake "./#macbook-pro" -b backup
popd

