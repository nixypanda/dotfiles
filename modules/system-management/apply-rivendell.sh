#!/bin/bash
set -euo pipefail

repo="${HOME}/.dotfiles"
remote="nixypanda@srt-n01-rivendell"
host="srt-n01-rivendell"
ssh_key="${HOME}/.ssh/github-key"
ssh_options="-i ${ssh_key} -o StrictHostKeyChecking=accept-new"

# nixos-rebuild evaluates flakes on the machine where it is invoked, even when
# --build-host is set. Archive the complete flake to Rivendell first so Linux-only
# packages are evaluated on Linux rather than on this Mac.
archive="$(
  NIX_SSHOPTS="${ssh_options}" \
    nix flake archive --json --to "ssh://${remote}" "${repo}"
)"
flake_path="$(printf '%s\n' "${archive}" | jq -er '.path')"

ssh -t \
  -i "${ssh_key}" \
  -o StrictHostKeyChecking=accept-new \
  "${remote}" \
  sudo tailscale set --accept-dns=false "&&" \
  sudo nixos-rebuild switch \
  --flake "${flake_path}#${host}" \
  --no-reexec
