#!/bin/sh
pushd ~/.dotfiles
nix flake update ./system
popd
