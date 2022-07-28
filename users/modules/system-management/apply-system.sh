#!/bin/bash

set -e

pushd ~/.dotfiles
sudo nixos-rebuild switch --flake './system#nixos'
popd
