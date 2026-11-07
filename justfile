# Dotfiles management commands.
#
# Run `just` in this repository (or any subdirectory) to list recipes.
#
# Nomenclature: `<verb>-<object> [host]`, where the object is a config layer,
# never a machine or platform. Host is always the final optional argument and
# accepts a canonical flake host name (`srt-l03-shire`) or an alias (`shire`).
# `apply` and `build` are umbrella actions covering every layer for a host.
#
# Verbs: apply, build, forecast, update, check
# Objects: home, system, flake, layout

repo := justfile_directory()

# Shared host lookup helpers. The flake publishes `hostAliases` and `hosts`,
# so these stay in sync with the machine registry.
host_helpers := '''
    resolve_host() {
        local input="${1:-$(hostname -s)}"
        input="${input%.local}"

        local canonical
        canonical=$(nix eval --raw ".#hostAliases.${input}") || {
            echo "error: unknown host '$input'" >&2
            echo "known hosts: $(nix eval --raw .#hostAliases --apply 'a: builtins.concatStringsSep ", " (builtins.attrNames a)')" >&2
            exit 1
        }

        printf '%s\n' "$canonical"
    }

    host_record() {
        nix eval --raw ".#hosts.${1}" --apply 'h: "${h.platform} ${h.user}"'
    }
    '''

# List available recipes.
default:
    @just --list --justfile "{{ justfile() }}"

# Apply every config layer for a host (Darwin: system + Home Manager; NixOS: system).
apply host="":
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{ repo }}"
    {{ host_helpers }}

    host=$(resolve_host "{{ host }}")
    platform=$(host_record "$host" | cut -d' ' -f1)

    just apply-system "$host"

    if [ "$platform" = "darwin" ]; then
        just apply-home "$host"
    fi

# Apply the Home Manager config for a host (Darwin only).
apply-home host="":
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{ repo }}"
    {{ host_helpers }}

    host=$(resolve_host "{{ host }}")
    read -r platform _ <<<"$(host_record "$host")"

    if [ "$platform" != "darwin" ]; then
        echo "apply-home: '$host' has no standalone Home Manager config" >&2
        exit 1
    fi

    nix run --no-write-lock-file --inputs-from . home-manager#home-manager -- switch --flake "./#${host}" -b backup

# Apply the OS config for a host (Darwin locally, NixOS locally or remotely).
apply-system host="":
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{ repo }}"
    {{ host_helpers }}

    host=$(resolve_host "{{ host }}")
    read -r platform user <<<"$(host_record "$host")"
    local_host=$(hostname -s)

    case "$platform" in
        darwin)
            if [ "$host" != "$local_host" ]; then
                echo "apply-system: Darwin config for '$host' must be applied on that host" >&2
                exit 1
            fi
            sudo darwin-rebuild switch --flake ".#${host}"
            ;;
        nixos)
            if [ "$host" = "$local_host" ]; then
                sudo nixos-rebuild switch --flake ".#${host}"
            else
                remote="${user}@${host}"
                NIX_SSHOPTS="-i ${HOME}/.ssh/github-key -o StrictHostKeyChecking=accept-new" \
                    nix run --no-write-lock-file --inputs-from . nixpkgs#nixos-rebuild -- switch \
                    --flake ".#${host}" \
                    --build-host "${remote}" \
                    --target-host "${remote}" \
                    --no-reexec \
                    --use-substitutes \
                    --ask-sudo-password
            fi
            ;;
        *)
            echo "apply-system: unsupported platform '$platform' for '$host'" >&2
            exit 1
            ;;
    esac

# Build every config layer for a host without activating it.
build host="":
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{ repo }}"
    {{ host_helpers }}

    host=$(resolve_host "{{ host }}")
    platform=$(host_record "$host" | cut -d' ' -f1)

    just build-system "$host"

    if [ "$platform" = "darwin" ]; then
        just build-home "$host"
    fi

# Build the Home Manager activation package without activating it (Darwin only).
build-home host="":
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{ repo }}"
    {{ host_helpers }}

    host=$(resolve_host "{{ host }}")
    read -r platform _ <<<"$(host_record "$host")"

    if [ "$platform" != "darwin" ]; then
        echo "build-home: '$host' has no standalone Home Manager config" >&2
        exit 1
    fi

    nix build --no-link ".#homeConfigurations.${host}.activationPackage"

# Build the OS config without activating it.
build-system host="":
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{ repo }}"
    {{ host_helpers }}

    host=$(resolve_host "{{ host }}")
    read -r platform user <<<"$(host_record "$host")"
    local_host=$(hostname -s)

    case "$platform" in
        darwin)
            if [ "$host" != "$local_host" ]; then
                echo "build-system: Darwin config for '$host' must be built on that host" >&2
                exit 1
            fi
            nix build --no-link ".#darwinConfigurations.${host}.system"
            ;;
        nixos)
            if [ "$host" = "$local_host" ]; then
                nix build --no-link ".#nixosConfigurations.${host}.config.system.build.toplevel"
            else
                remote="${user}@${host}"
                NIX_SSHOPTS="-i ${HOME}/.ssh/github-key -o StrictHostKeyChecking=accept-new" \
                    nix run --no-write-lock-file --inputs-from . nixpkgs#nixos-rebuild -- build \
                    --flake ".#${host}" \
                    --build-host "${remote}" \
                    --target-host "${remote}" \
                    --no-reexec \
                    --use-substitutes
            fi
            ;;
        *)
            echo "build-system: unsupported platform '$platform' for '$host'" >&2
            exit 1
            ;;
    esac

# Preview which packages a Home Manager switch would build.
forecast-home host="":
    #!/usr/bin/env bash
    set -eu
    cd "{{ repo }}"
    {{ host_helpers }}

    host=$(resolve_host "{{ host }}")
    read -r platform _ <<<"$(host_record "$host")"

    if [ "$platform" != "darwin" ]; then
        echo "forecast-home: '$host' has no standalone Home Manager config" >&2
        exit 1
    fi

    out_file="/tmp/nix-forecast.txt"

    nix-forecast -s --home "{{ repo }}#homeConfigurations.${host}" |
        tee "$out_file" |
        head -4

    echo
    echo "Packages to be built:"

    nix_hash_strip='s|^/nix/store/[^-]*-||'
    ext_completion='\-completion$'
    ext_archive='\.(zip|patch|lock)$'
    ext_script='\.(fish|sh|bash|nu|zsh)$'
    ext_metadata='\.(json|conf|plist|service|keep|md|yml)$'
    name_vim_plugin='^vimplugin-'
    name_home_manager='^(hm_|home-)'
    name_firefox_plugins='^(bitwarden|clearurls|darkreader|facebook-container|multi-account-containers|return-youtube-dislikes|sponsorblock|temporary-containers|ublock-origin|vimium)'
    name_python_env='\-env$'

    grep "/nix/store/" "$out_file" |
        sed "$nix_hash_strip" |
        grep -Ev "$ext_completion" |
        grep -Ev "$ext_archive" |
        grep -Ev "$ext_script" |
        grep -Ev "$ext_metadata" |
        grep -Ev "$name_vim_plugin" |
        grep -Ev "$name_home_manager" |
        grep -Ev "$name_firefox_plugins" |
        grep -Ev "$name_python_env" |
        sort |
        nl

# Update every flake input and write flake.lock.
update-flake:
    cd "{{ repo }}" && nix flake update --flake .

# Enforce the top-level dependency direction (see AGENTS.md, "Repo layout").
check-layout:
    #!/bin/sh
    # pkgs/ and colorschemes/ are leaves; modules/ sits above them; services/
    # above modules; hosts/ on top. A lower layer must never import a higher one,
    # and hosts must not import each other.
    # Heuristic: scans .nix files for relative paths that climb into a higher layer.
    set -eu

    cd "{{ repo }}"

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

    scan pkgs '\.\.(/\.\.)*/(modules|services|hosts)/'
    scan colorschemes '\.\.(/\.\.)*/(modules|services|hosts)/'
    scan modules '\.\.(/\.\.)*/(services|hosts)/'
    scan services '\.\.(/\.\.)*/hosts/'
    scan hosts '\.\.(/\.\.)*/hosts/'

    if [ "$fail" -ne 0 ]; then
        echo 'layout: dependency-direction check FAILED' >&2
        exit 1
    fi
    echo 'layout: dependency-direction check OK'

# Deprecated aliases.
alias apply-user := apply-home
alias apply-darwin := apply-system

# Deprecated: use `just apply rivendell`.
[private]
apply-rivendell:
    @just apply rivendell
