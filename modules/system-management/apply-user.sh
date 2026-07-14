#!/bin/bash

set -e

pushd ~/.dotfiles
nix run --no-write-lock-file --inputs-from . home-manager#home-manager -- switch --flake "./#srt-l02-sekhmet" -b backup
popd
