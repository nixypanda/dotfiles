#!/bin/bash

set -e

pushd ~/.dotfiles
home-manager switch --flake "./#srt-l02-sekhmet" -b backup
popd
