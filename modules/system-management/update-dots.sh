#!/bin/sh

set -e

pushd ~/.dotfiles
nix flake update --flake .
popd
