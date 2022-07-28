#!/bin/bash

set -e

pushd ~/.dotfiles
nix flake update ./users
popd
