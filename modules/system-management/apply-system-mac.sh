#!/bin/bash

pushd ~/.dotfiles

nix build ~/.dotfiles/\#darwinConfigurations.srt-l02-sekhmet.system
./result/sw/bin/darwin-rebuild switch --flake ~/.dotfiles

popd
