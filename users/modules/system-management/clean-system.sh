#!/bin/sh

# Remove all docker shit
docker system prune -a

# Remove old logs
sudo journalctl --vacuum-time=2d

# Remove google chrome shit
rm -rf ~/.cache/google-chrome/Default/Cache
rm -rf ~/.cache/google-chrome/Default/Code\ Cache

# Get rid of stuff older than 2 days
nix-env --delete-generations 2d
sudo nix-env --delete-generations 2d
home-manager expire-generations "-2 days"
nix-store --gc
nix-store --optimize
