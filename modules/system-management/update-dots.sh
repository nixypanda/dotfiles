#!/bin/sh

set -e

pushd ~/.dotfiles
sudo nix flake update .
popd
