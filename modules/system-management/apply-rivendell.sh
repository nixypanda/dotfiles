#!/bin/bash
set -e

pushd ~/.dotfiles

env NIX_SSHOPTS='-i /Users/nixypanda/.ssh/github-key -o StrictHostKeyChecking=accept-new' \
  nix run nixpkgs#nixos-rebuild -- switch \
  --flake .#srt-n01-rivendell \
  --build-host nixypanda@srt-n01-rivendell \
  --target-host nixypanda@srt-n01-rivendell \
  --no-reexec \
  --use-substitutes \
  --ask-sudo-password

popd
