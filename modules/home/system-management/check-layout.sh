#!/bin/sh
# Enforce the top-level dependency direction (see AGENTS.md, "Repo layout"):
#   pkgs/ and colorschemes/ are leaves; modules/ sits above them; services/
#   above modules; profiles/ above services; hosts/ on top. A lower layer must
#   never import a higher one, and hosts must not import each other.
# Heuristic: scans .nix files for relative paths that climb into a higher layer.
set -eu

cd "$(git rev-parse --show-toplevel 2>/dev/null || echo .)"

fail=0
scan() {
  target=$1
  pattern=$2
  matches=$(find "$target" -type f -name '*.nix' -exec grep -En "$pattern" {} + 2>/dev/null || true)
  if [ -n "$matches" ]; then
    printf 'layout: %s must not import a higher layer:\n%s\n' "$target" "$matches" >&2
    fail=1
  fi
}

scan pkgs '\.\.(/\.\.)*/(modules|services|profiles|hosts)/'
scan colorschemes '\.\.(/\.\.)*/(modules|services|profiles|hosts)/'
scan modules '\.\.(/\.\.)*/(services|profiles|hosts)/'
scan services '\.\.(/\.\.)*/(profiles|hosts)/'
scan profiles '\.\.(/\.\.)*/hosts/'
scan hosts '\.\.(/\.\.)*/hosts/'

if [ "$fail" -ne 0 ]; then
  echo 'layout: dependency-direction check FAILED' >&2
  exit 1
fi
echo 'layout: dependency-direction check OK'
