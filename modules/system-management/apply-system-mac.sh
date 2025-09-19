#!/bin/bash

pushd ~/.dotfiles

sudo darwin-rebuild switch --flake ~/.dotfiles/.#srt-l02-sekhmet

popd
