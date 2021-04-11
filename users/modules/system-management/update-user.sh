#!/bin/sh
pushd ~/.dotfiles
nix flake update ./users
popd
