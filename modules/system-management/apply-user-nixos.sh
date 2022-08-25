#!/bin/sh

set -e

pushd ~/.dotfiles
home-manager switch --flake "./#nixos"
popd
