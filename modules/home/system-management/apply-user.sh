#!/bin/bash

set -e

# Default to this machine's hostname; override with `apply-user <host>`.
HOST="${1:-$(hostname -s)}"

pushd ~/.dotfiles
nix run --no-write-lock-file --inputs-from . home-manager#home-manager -- switch --flake "./#${HOST}" -b backup
popd
