#!/bin/bash

pushd ~/.dotfiles

nix run nix-darwin -- switch --flake ~/.dotfiles/.#srt-l02-sekhmet

popd
