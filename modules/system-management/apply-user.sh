#!/bin/bash

set -e

pushd ~/.dotfiles
nix run home-manager --no-write-lock-file -- switch --flake "./#srt-l02-sekhmet" -b backup
popd
