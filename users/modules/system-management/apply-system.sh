#!/bin/sh

set -e

pushd ~/.dotfiles
sudo nixos-rebuild switch --flake './system#nixos'
popd
