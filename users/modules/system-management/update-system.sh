#!/bin/sh
pushd ~/.dotfiles
sudo nix flake update ./system
popd
