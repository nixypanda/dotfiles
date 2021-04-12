#!/bin/sh
pushd ~/.dotfiles
home-manager switch --flake "./users#macbook-pro"
popd
