#!/bin/sh

set -e

cd ~/.dotfiles
nix flake update --flake .
