#!/bin/sh
pushd ~/.dotfiles
sudo nixos-rebuild switch --flake './system#nixos'
popd
