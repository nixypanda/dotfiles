#!/bin/sh
pushd ~/.dotfiles
home-manager switch -f ./users/sherub/home.nix
popd
