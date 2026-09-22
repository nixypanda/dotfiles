#!/bin/bash

set -e

# Default to this machine's hostname; override with `apply-darwin <host>`.
HOST="${1:-$(hostname -s)}"

pushd ~/.dotfiles
sudo darwin-rebuild switch --flake "$HOME/.dotfiles#${HOST}"
popd
