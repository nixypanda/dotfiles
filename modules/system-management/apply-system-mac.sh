#!/bin/bash

pushd ~/.dotfiles

nix build ~/.dotfiles/\#darwinConfigurations.nixyMac.system
./result/sw/bin/darwin-rebuild switch --flake ~/.dotfiles

popd
